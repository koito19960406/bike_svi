from curses import def_prog_mode
from torch.utils.data import Dataset, DataLoader
from transformers import AdamW
import torch
from torch import nn
from sklearn.metrics import accuracy_score
from tqdm.notebook import tqdm
import os
from PIL import Image
from transformers import SegformerForSemanticSegmentation, SegformerFeatureExtractor, SegformerConfig
import pandas as pd
import cv2
import numpy as np
import albumentations as aug
from data_preparation import ImageSegmentationDataset

class ImageFeatureExtractor:
    """class for extracting features from street view images.
    - objective features will be extracted through 
        - segmentation with Segformer trained on Mapillary Vistas v2.0 (https://www.mapillary.com/dataset/vistas)
            - reference: https://medium.com/geekculture/semantic-segmentation-with-segformer-2501543d2be4
            - https://blog.roboflow.com/how-to-train-segformer-on-a-custom-dataset-with-pytorch-lightning/
        - object detection with DETR model pre-trained on COCO dataset
    - subjective features will be extracted through perception 
        - image regression with ViT model trained on Place Pulse 2.0 (https://www.dropbox.com/s/grzoiwsaeqrmc1l/place-pulse-2.0.zip?dl=0)
    """
    def __init__(self, input_folder, model_folder):
        self.root_dir = input_folder
        self.model_folder = model_folder
        
    def _create_data_loaders(self):
        # data augmentation instance
        transform = aug.Compose([aug.Flip(p=0.5)])
        # ss feature extract instance
        feature_extractor = SegformerFeatureExtractor(align=False, reduce_zero_label=False)

        # create datasets
        train_dataset = ImageSegmentationDataset(root_dir=self.root_dir, feature_extractor=feature_extractor, transforms=transform)
        valid_dataset = ImageSegmentationDataset(root_dir=self.root_dir, feature_extractor=feature_extractor, transforms=None, train=False)
        
        # create data loaders
        train_dataloader = DataLoader(train_dataset, batch_size=32, shuffle=True)
        valid_dataloader = DataLoader(valid_dataset, batch_size=32) 
        return train_dataloader, valid_dataloader
    
    def train_segmentation_model(self, epoch_num=100):
        # set path to the checkpoint model file
        path_checkpoint = os.path.join(self.model_folder,"segmentation_checkpoint.pt")
        
        # create dataloaders
        train_dataloader, valid_dataloader = self._create_data_loaders()
        
        # load checkpoints if there is
        if os.path.exists(path_checkpoint):
            model = torch.load(path_checkpoint)
            model.load_state_dict(model['model_state_dict'])
            optimizer.load_state_dict(model['optimizer_state_dict'])
            checkpoint_epoch = model['epoch']
        # if not, then initialize a model
        else:
            checkpoint_epoch = 1
            
            classes = pd.read_csv(os.path.join(self.root_dir, 'class_dict_seg.csv'))['name']
            id2label = classes.to_dict()
            label2id = {v: k for k, v in id2label.items()}
            # configuration = SegformerConfig()
            # model = SegformerForSemanticSegmentation(configuration)
            model = SegformerForSemanticSegmentation.from_pretrained("nvidia/mit-b5", ignore_mismatched_sizes=True,
                                                            num_labels=len(id2label), id2label=id2label, label2id=label2id,
                                                            reshape_last_stage=True)
            optimizer = AdamW(model.parameters(), lr=0.00006)
            device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
            model.to(device)
            print("Model Initialized!")
        
        # train through epochs
        for epoch in range(1, epoch_num+1):  # loop over the dataset multiple times
            # skip to the checkpoint_epoch
            if epoch < checkpoint_epoch:
                continue
            print("Epoch:", epoch)
            pbar = tqdm(train_dataloader)
            accuracies = []
            losses = []
            val_accuracies = []
            val_losses = []
            model.train()
            for idx, batch in enumerate(pbar):
                # get the inputs;
                pixel_values = batch["pixel_values"].to(device)
                labels = batch["labels"].to(device)

                # zero the parameter gradients
                optimizer.zero_grad()

                # forward
                outputs = model(pixel_values=pixel_values, labels=labels)

                # evaluate
                upsampled_logits = nn.functional.interpolate(outputs.logits, size=labels.shape[-2:], mode="bilinear", align_corners=False)
                predicted = upsampled_logits.argmax(dim=1)

                mask = (labels != 255) # we don't include the background class in the accuracy calculation
                pred_labels = predicted[mask].detach().cpu().numpy()
                true_labels = labels[mask].detach().cpu().numpy()
                accuracy = accuracy_score(pred_labels, true_labels)
                loss = outputs.loss
                accuracies.append(accuracy)
                losses.append(loss.item())
                pbar.set_postfix({'Batch': idx, 'Pixel-wise accuracy': sum(accuracies)/len(accuracies), 'Loss': sum(losses)/len(losses)})

                # backward + optimize
                loss.backward()
                optimizer.step()
            else:
                model.eval()
                with torch.no_grad():
                    for idx, batch in enumerate(valid_dataloader):
                        pixel_values = batch["pixel_values"].to(device)
                        labels = batch["labels"].to(device)
                        
                        outputs = model(pixel_values=pixel_values, labels=labels)
                        upsampled_logits = nn.functional.interpolate(outputs.logits, size=labels.shape[-2:], mode="bilinear", align_corners=False)
                        predicted = upsampled_logits.argmax(dim=1)
                        
                        mask = (labels != 255) # we don't include the background class in the accuracy calculation
                        pred_labels = predicted[mask].detach().cpu().numpy()
                        true_labels = labels[mask].detach().cpu().numpy()
                        accuracy = accuracy_score(pred_labels, true_labels)
                        val_loss = outputs.loss
                        val_accuracies.append(accuracy)
                        val_losses.append(val_loss.item())
            print(f"Train Pixel-wise accuracy: {sum(accuracies)/len(accuracies)}\
                Train Loss: {sum(losses)/len(losses)}\
                Val Pixel-wise accuracy: {sum(val_accuracies)/len(val_accuracies)}\
                Val Loss: {sum(val_losses)/len(val_losses)}")
            
            # save a checkpoint
            torch.save({
                'epoch': epoch,
                'model_state_dict': model.state_dict(),
                'optimizer_state_dict': optimizer.state_dict(),}, 
                       path_checkpoint)
        
        # save the trained model
        torch.save(model, os.path.join(self.model_folder,"segmentation.pt"))