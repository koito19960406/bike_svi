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
from transformers import DetrFeatureExtractor, DetrForObjectDetection
import glob
import re
import tqdm

"""- objective features will be extracted through 
        - segmentation with Segformer trained on Mapillary Vistas v2.0 (https://www.mapillary.com/dataset/vistas)
            - reference: https://medium.com/geekculture/semantic-segmentation-with-segformer-2501543d2be4
            - https://blog.roboflow.com/how-to-train-segformer-on-a-custom-dataset-with-pytorch-lightning/
            - https://github.com/EEEGUI/Mapillary-vistas-semseg
        - object detection with DETR model pre-trained on COCO dataset
    - subjective features will be extracted through perception 
        - image regression with ViT model trained on Place Pulse 2.0 (https://www.dropbox.com/s/grzoiwsaeqrmc1l/place-pulse-2.0.zip?dl=0)
"""
class ImageSegmenter:
    """class for segmenting street view images.
    """
    def __init__(self, input_folder, model_folder):
        self.root_dir = input_folder
        self.model_folder = model_folder
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        self.label = pd.DataFrame.from_dict(pd.read_json(os.path.join(self.root_dir, "config_v2.0.json"))["labels"].tolist())
        
    def _create_data_loaders(self):
        # data augmentation instance
        transform = aug.Compose([aug.Flip(p=0.5)])
        # ss feature extract instance
        feature_extractor = SegformerFeatureExtractor(align=False, reduce_zero_label=False)

        # create datasets
        train_dataset = ImageSegmentationDataset(root_dir=self.root_dir, feature_extractor=feature_extractor, transforms=transform)
        valid_dataset = ImageSegmentationDataset(root_dir=self.root_dir, feature_extractor=feature_extractor, transforms=None, train=False)
        print("Number of training examples:", len(train_dataset))
        print("Number of validation examples:", len(valid_dataset))
        encoded_inputs = train_dataset[0]
        print("pixel values: ",encoded_inputs["pixel_values"].shape,
              "label shape: ", encoded_inputs["labels"].shape,
              "label: ", encoded_inputs["labels"],
              "unique label: ", encoded_inputs["labels"].squeeze().unique())
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
            accuracies_hist_list = model['accuracies_hist_list']
            losses_hist_list = model['losses_hist_list']
            val_accuracies_hist_list = model['val_accuracies_hist_list']
            val_losses_hist_list = model['val_losses_hist_list']
        # if not, then initialize a model
        else:
            checkpoint_epoch = 1
            
            classes = self.label['name']
            id2label = classes.to_dict()
            label2id = {v: k for k, v in id2label.items()}
            # configuration = SegformerConfig()
            # model = SegformerForSemanticSegmentation(configuration)
            model = SegformerForSemanticSegmentation.from_pretrained("nvidia/mit-b5", ignore_mismatched_sizes=True,
                                                            num_labels=len(id2label), id2label=id2label, label2id=label2id,
                                                            reshape_last_stage=True)
            optimizer = AdamW(model.parameters(), lr=0.00006)
            
            model.to(self.device)
            print("Model Initialized!")
        
        # train through epochs
        for epoch in range(1, epoch_num+1):  # loop over the dataset multiple times
            # skip to the checkpoint_epoch
            if epoch < checkpoint_epoch:
                continue
            print("Epoch:", epoch)
            pbar = tqdm.tqdm(train_dataloader)
            accuracies = []
            losses = []
            val_accuracies = []
            val_losses = []
            model.train()
            for idx, batch in enumerate(pbar):
                # get the inputs;
                pixel_values = batch["pixel_values"].to(self.device)
                labels = batch["labels"].to(self.device)

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
                        pixel_values = batch["pixel_values"].to(self.device)
                        labels = batch["labels"].to(self.device)
                        
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
            accuracy_epoch = sum(accuracies)/len(accuracies)
            loss_epoch = sum(losses)/len(losses)
            val_accuracy_epoch = sum(val_accuracies)/len(val_accuracies)
            val_loss_epoch = sum(val_losses)/len(val_losses)
            print(f"Train Pixel-wise accuracy: {accuracy_epoch}\
                Train Loss: {loss_epoch}\
                Val Pixel-wise accuracy: {val_accuracy_epoch}\
                Val Loss: {val_loss_epoch}")
            
            # save a checkpoint
            torch.save({
                'epoch': epoch,
                'model_state_dict': model.state_dict(),
                'optimizer_state_dict': optimizer.state_dict(),
                'accuracies_hist_list': accuracies_hist_list.append(accuracy_epoch),
                'losses_hist_list': losses_hist_list.append(loss_epoch),
                'val_accuracies_hist_list': val_accuracies_hist_list.append(val_accuracy_epoch),
                'val_losses_hist_list': val_losses_hist_list.append(val_loss_epoch)
                }, 
                       path_checkpoint)
        
        # save the trained model
        torch.save(model, os.path.join(self.model_folder,"segmentation.pt"))
    
    def infer(self):
        # load model
        model = torch.load(os.path.join(self.model_folder,"segmentation.pt"))
        # set up feature extractor
        feature_extractor_inference = SegformerFeatureExtractor(align=False, reduce_zero_label=False)
        # loop through images
        for image_file in tqdm.tqdm(glob.glob(os.path.join(self.input_folder, "gsv/image/perspective/*.png"))):
            # read image_file to opencv
            image = cv2.imread(image_file)
            # get pid
            image_file_name = os.path.split(image_file)[1]
            pid = re.search("(.*)(?<=_Direction)", image_file_name).groups(1)[0]
            pid = re.sub("_Direction", "", pid)
            
            # get pixel_values after feature extraction
            pixel_values = feature_extractor_inference(image, return_tensors="pt").pixel_values.to(self.device)
            # get output logit from the model
            model.eval()
            outputs = model(pixel_values=pixel_values) # logits are of shape (batch_size, num_labels, height/4, width/4)
            logits = outputs.logits.cpu()
            # First, rescale logits to original image size
            upsampled_logits = nn.functional.interpolate(logits,
                            size=image.shape[:-1], # (height, width)
                            mode='bilinear',
                            align_corners=False)

            # Second, apply argmax on the class dimension
            seg = upsampled_logits.argmax(dim=1)[0]
            color_seg = np.zeros((seg.shape[0], seg.shape[1], 3), dtype=np.uint8) # height, width, 3\
            for label, color in enumerate(self.label):
                color_seg[seg == label, :] = color
                # Convert to BGR
                color_seg = color_seg[..., ::-1]

                print(pixel_values.shape)
class ImageDetector:
    """Object detect SVI (only use a pretrained model since the objective is to get vehicles and bicycles)
    """
    
    def __init__(self, input_folder, output_folder, city, pretrained_model = "facebook/detr-resnet-50"):
        self.pretrained_model = pretrained_model
        self.input_folder = os.path.join(input_folder, city)
        self.output_folder = os.path.join(output_folder, city)
        os.makedirs(self.output_folder, exist_ok = True)
        self.city = city
        pass
    
    def detect_object(self):
        # load and create a model
        feature_extractor = DetrFeatureExtractor.from_pretrained(self.pretrained_model)
        model = DetrForObjectDetection.from_pretrained(self.pretrained_model)
        # loop through images for inference
        for image_file in tqdm.tqdm(glob.glob(os.path.join(self.input_folder, "gsv/image/perspective/*.png"))):
            try:
                result_df = pd.read_csv(os.path.join(self.output_folder, "object_detection_raw.csv"))
            except FileNotFoundError:
                result_df = pd.DataFrame(columns=["pid","label","score"])
            image_file_name = os.path.split(image_file)[1]
            # get pid
            pid = re.search("(.*)(?<=_Direction)", image_file_name).groups(1)[0]
            pid = re.sub("_Direction", "", pid)

            # check if the result is already in the csv file
            if not pid in result_df["pid"].tolist():
                # initialize a list to save the result
                result_list = []
                # create input and output
                image = Image.open(image_file)
                inputs = feature_extractor(images=image, return_tensors="pt")
                outputs = model(**inputs)
                # resize and infer
                target_sizes = torch.tensor([image.size[::-1]])
                results = feature_extractor.post_process(outputs, target_sizes=target_sizes)[0]
                # initialize df to save the result
                result_list_agg = []
                for score, label in zip(results["scores"], results["labels"]):
                    # let's only keep detections with score > 0.8
                    if score > 0.8:
                        result_list = [pid, model.config.id2label[label.item()], score.item()]
                        print(
                            f"Detected {model.config.id2label[label.item()]} with confidence "
                            f"{round(score.item(), 3)}"
                        )
                        result_list_agg.append(result_list)
                # if there's no result, then append only pid
                if len(result_list_agg) == 0:
                    result_list_agg.append([pid,None,None])
            # convert the result_list_agg to df and save as csv
            result_list_agg_df = pd.DataFrame(result_list_agg, columns=["pid","label","score"])
            result_df = pd.concat([result_df.reset_index(drop=True), result_list_agg_df.reset_index(drop=True)])
            result_df.to_csv(os.path.join(self.output_folder, "object_detection_raw.csv"), index = False)
    
    def count_vehicle_bicycle(self):
        """calculate total number of bycicycle and vehicles (car, motorcycle, bus, truck) under COCO detection 2017 dataset
        """
        # load the result of object detection as df
        result_df = pd.read_csv(os.path.join(self.output_folder, "object_detection.csv"))
        # filter with bicycle and group by pid
        result_bicycle = (result_df.
                          query("pid == 'bicycle'").
                          groupby("pid").
                          agg(["sum"]).
                          reset_index()
                          )
        print(result_bicycle)
        # filter with vehicles (car, motorcycle, bus, truck) and group by pid
        result_vehicle = (result_df.
                          query("pid == 'car' | pid == 'motorcycle' | pid == 'bus' | pid == 'truck'").
                          groupby("pid").
                          agg(["sum"]).
                          reset_index()
                          )
        print(result_vehicle)
        # join the result back to gsv meta_data
        gsv_metadata = pd.read_csv(os.path.join(self.input_folder,self.city+"/gsv/metadata/gsv_metadata.csv"))
        gsv_metadata = (gsv_metadata.
                        merge(result_bicycle, left_on = "panoid", right_on = "pid", how = "left").
                        merge(result_vehicle, left_on = "panoid", right_on = "pid", how = "left")
                        )
        print(gsv_metadata)
        # save the result as csv
        gsv_metadata.to_csv(os.path.join(self.output_folder, "/object_detection_bicycle_vehicle.csv"), index = False)


class PerceptionPredictor:
    """class for predicting perception score for PlacePulse 2.0 data
    """
    
    def __init__(self, input_folder, model_folder):
        pass
    
if __name__ == '__main__':
    root_dir = "/Volumes/Extreme SSD/bike_svi/"
    input_folder = os.path.join(root_dir,"data/raw")
    output_folder = os.path.join(root_dir,"data/interim/cities")
    model_folder = os.path.join(root_dir,"models")
    # segmentation
    image_segmenter = ImageSegmenter(os.path.join(input_folder,"mapillary_vistas"), model_folder)
    image_segmenter.train_segmentation_model()
    # # object detection
    # for city in os.listdir(input_folder):
    #     if not city.startswith("."):
    #         print(city)
    #         image_detector = ImageDetector(os.path.join(input_folder,"cities"), output_folder, city)
    #         image_detector.detect_object()
    #     else:
    #         pass