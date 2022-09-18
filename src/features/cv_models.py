# from curses import def_prog_mode
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
import json
import shutil
from transformers import DetrFeatureExtractor, DetrForSegmentation
# from detectron2.data import MetadataCatalog
from transformers.models.detr.feature_extraction_detr import rgb_to_id, id_to_rgb
import io
from copy import deepcopy
# from detectron2.utils.visualizer import Visualizer
from multiprocessing.pool import ThreadPool
from threading import Thread
from queue import Queue
import csv 
from pyarrow import feather
import dask
import dask.dataframe as dd

from datasets.simple_segmentation import SimpleSegmentationInferDataset
from datasets.detection import DetectionInferDataset
from color_maps import cityscapes

"""- objective features will be extracted through 
        - segmentation with Segformer trained on Mapillary Vistas v2.0 (https://www.mapillary.com/dataset/vistas)
            - reference: https://medium.com/geekculture/semantic-segmentation-with-segformer-2501543d2be4
            - https://blog.roboflow.com/how-to-train-segformer-on-a-custom-dataset-with-pytorch-lightning/
            - https://github.com/EEEGUI/Mapillary-vistas-semseg
        - object detection with DETR model pre-trained on COCO dataset
    - subjective features will be extracted through perception 
        - image regression with ViT model trained on Place Pulse 2.0 (https://www.dropbox.com/s/grzoiwsaeqrmc1l/place-pulse-2.0.zip?dl=0)
"""

def filter_image(gsv_metadata_folder, input_folder):
    """function to filter out invalid images and save as csv to avoid errors later
    """
    if not os.path.exists(os.path.join(gsv_metadata_folder,"invalid_file_name.csv")):
        invalid_list = []
        for image_file in tqdm.tqdm(os.listdir(input_folder), total = len(os.listdir(input_folder))):
            try:
                image = cv2.imread(os.path.join(input_folder,image_file))
                if image is None:
                    invalid_list.append(image_file)
                        
            except cv2.error:
                invalid_list.append(image_file)
        # save the list as csv
        invalid_df = pd.DataFrame(invalid_list, columns=["file_name"])
        invalid_df.to_csv(os.path.join(gsv_metadata_folder,"invalid_file_name.csv"), index = False)
        print("-"*10, "removed ", str(len(invalid_df.index)), " invalid image files", "-"*10)
    else:
        print("Following file already exists: ", os.path.join(gsv_metadata_folder,"invalid_file_name.csv"))

class ImageSegmentationSimple:
    def __init__(self, input_folder, gsv_metadata_folder, img_output_folder, csv_output_folder):
        self.input_folder = input_folder
        self.gsv_metadata_folder = gsv_metadata_folder
        self.img_output_folder = img_output_folder
        self.csv_output_folder = csv_output_folder
        # create folder if it doesn't exist yet
        os.makedirs(self.img_output_folder, exist_ok = True)
        os.makedirs(self.csv_output_folder, exist_ok = True)
        # set the device
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        self.cpu_num = os.cpu_count()
        self.labels = cityscapes.create_cityscapes_label_colormap()
        # filter image to remove invalid images
        filter_image(self.gsv_metadata_folder, self.input_folder)
            
    def segment_svi(self, pretrained_model = "nvidia/segformer-b5-finetuned-cityscapes-1024-1024"):
        """function to conduct segmentation
        """
        gsv_invalid_file = os.path.join(self.gsv_metadata_folder,"invalid_file_name.csv")
        # import segmentation model
        feature_extractor = SegformerFeatureExtractor()
        model = SegformerForSemanticSegmentation.from_pretrained(pretrained_model).to(self.device)
        
        infer_data = SimpleSegmentationInferDataset(self.input_folder, gsv_invalid_file, feature_extractor, self.img_output_folder)
        infer_loader = torch.utils.data.DataLoader(infer_data,
                                                batch_size=1,
                                                shuffle=False,
                                                num_workers=4,
                                                pin_memory=True)
        
        with torch.no_grad():
            for inputs, file_name in tqdm.tqdm(infer_loader, total = len(infer_loader)):
                # produce logits from the model
                inputs = inputs.to(self.device)
                outputs = model(pixel_values = inputs)
                logits = outputs.logits.cpu()
                upsampled_logits = nn.functional.interpolate(logits,
                    size=inputs.shape[-2:], # (height, width)
                    mode='bilinear',
                    align_corners=False)

                # Second, apply argmax on the class dimension
                seg = upsampled_logits.argmax(dim=1)[0].numpy()
                # save it to png
                cv2.imwrite(os.path.join(self.img_output_folder, "segmented", file_name[0]+".png"), seg)

        
    def calculate_ratio(self, update = False):
        # create output folder for the colored segmentation results
        os.makedirs(os.path.join(self.img_output_folder, "segmented_color"), exist_ok = True)
        

        # dedicated file writing taskS
        def file_writer(index, output_folder, queue):
            # create progress bar
            pbar = tqdm.tqdm(total=queue.qsize())
            # set partition tracker
            partition = 1
            while True:
                if queue.empty():
                    print(f"thread {index} is now empty")
                    # save and exit 
                    try:
                        df.to_feather(os.path.join(output_folder, f"{file_name}.feather"))
                    except UnboundLocalError:
                        print(f"thread thread {index} doesn't have anything to save")
                        break
                    finally:
                        # exit the loop
                        break
                # get an image file from the queue
                image_file = queue.get()
                try:
                    # get the file name
                    file_name = os.path.basename(image_file)[:-4]
                    seg = cv2.imread(image_file, cv2.IMREAD_GRAYSCALE)
                    if seg is None:
                        print("Got an empty image")
                        continue
                    # calculate the total pixels
                    total_pixel = seg.shape[0] * seg.shape[1]
                    # Finally we visualize the prediction
                    color_seg = np.zeros((seg.shape[0], seg.shape[1], 3), dtype=np.uint8)
                    # get an array of unique ids
                    unique_id_array = np.unique(seg)
                    # empty df
                    if partition == 1:
                        df = pd.DataFrame(columns = ["file_name", "name", "pixel_ratio"])
                        
                    for unique_id in unique_id_array:
                        # filter to get a respective label
                        label = list(filter(lambda label: label.trainId == unique_id, self.labels))[0]
                        # give color to color_seg
                        color_seg[seg == unique_id, :] = label.color
                        # calculate the number of pixel for each class
                        pixel_num = np.sum((seg==unique_id))
                        pixel_ratio = pixel_num / total_pixel
                        # write.writerow([file_name, label.name, pixel_ratio])
                        df = pd.concat([df, pd.DataFrame([[file_name, label.name, pixel_ratio]], columns=["file_name", "name", "pixel_ratio"])], ignore_index=True)
                    if partition == 500:
                        df.to_feather(os.path.join(output_folder, f"{file_name}.feather"))
                        df = None
                        partition = 0
                    partition += 1
                    # save as png
                    cv2.imwrite(os.path.join(self.img_output_folder, "segmented_color", file_name + ".png"), color_seg)
                except:
                    print(f"You got an error for {image_file}")
                # mark the unit of work complete
                queue.task_done()
                # update pbar
                pbar.update(1)
            # mark the exit signal as processed, after the file was closed
            queue.task_done()

        # define the shared file path
        output_folder = os.path.join(self.csv_output_folder, 'segmentation_pixel_ratio')
        os.makedirs(output_folder, exist_ok = True)
    
        feather_list = glob.glob(os.path.join(output_folder, "*.feather"))
        try:
            dfs = [dask.delayed(feather.read_feather)(f) for f in feather_list]
            df = dd.from_delayed(dfs)
            filename_set = set(df.iloc[:,0].unique().compute())
            print("successfully read the file")
        except FileNotFoundError:
            print("file not found")
            filename_set = set()
        except IndexError:
            print("index error")
            filename_set = set()
        # create the shared queue
        segmented_img_list = glob.glob(os.path.join(self.img_output_folder, "segmented/*.png"))
        print("image list read")
        queue = Queue()
        [queue.put(i) for i in tqdm.tqdm(segmented_img_list) if os.path.basename(i)[:-4] not in filename_set]
        if queue.qsize() > self.cpu_num:
            # create and start the file writer thread
            threads = [Thread(target=file_writer, args=(index, output_folder, queue), daemon=True) for index in tqdm.tqdm(range(self.cpu_num))]
        else:
            threads = [Thread(target=file_writer, args=(index, output_folder, queue), daemon=True) for index in tqdm.tqdm(range(queue.qsize()))]
        for thread in threads:
            thread.start()
        # wait for threads to finish
        for thread in threads:
            thread.join()
        # wait for all tasks in the queue to be processed
        queue.join()
                
        # transform the csv to wide format
        dfs = [dask.delayed(feather.read_feather)(f) for f in feather_list]
        df = dd.from_delayed(dfs)
        segment_pixel_ratio_df = df.drop_duplicates().compute()
        # create pid
        segment_pixel_ratio_df["pid"] = segment_pixel_ratio_df["file_name"].str.extract("(.*)(?<=_Direction)")
        segment_pixel_ratio_df["pid"] = segment_pixel_ratio_df["pid"].str.replace("_Direction", "")
        segment_pixel_ratio_df = (segment_pixel_ratio_df.pivot_table(index="pid", columns= "name", values='pixel_ratio', aggfunc=np.mean).
                                        reset_index().
                                        fillna(0))
        # save as csv
        segment_pixel_ratio_df.to_csv(os.path.join(self.csv_output_folder, "segmentation_pixel_ratio_wide.csv"), index = False)
        
# class ImageSegmenterBikeLane:
#     """class for segmenting street view images.
#     """
#     def __init__(self, input_folder, model_folder):
#         self.root_dir = input_folder
#         self.model_folder = model_folder
#         self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
#         self.label = json.load(open(os.path.join(self.root_dir,"mapillary_vistas","config_v2.0.json")))['labels']
    
#     def reclassify_labels_bl_only(self):
#         """reclassify labels into binary whether it's bine lane or not
#         """
#         # create a new folder
#         self.new_binary_folder = os.path.join(self.root_dir,"mapillary_binary")
#         os.makedirs(self.new_binary_folder, exist_ok = True)
#         for dataset in ["training","validation"]:
#             print("-"*10,f"working on labels in {dataset}","-"*10)
#             # create a new folder for each dataset
#             os.makedirs(os.path.join(self.new_binary_folder,f"{dataset}/labels"), exist_ok = True)
#             label_file_list = glob.glob(os.path.join(self.root_dir,f"mapillary_vistas/{dataset}/v2.0/labels/*.png"))
#             for label_file in tqdm.tqdm(label_file_list):
#                 key = os.path.split(label_file)[1][:-4]
#                 if not os.path.exists(os.path.join(self.new_binary_folder,f"{dataset}/labels",key+".png")):
#                     # get the 1st of the 3rd dimention only because all the RGB layers have the same values (i.e., Class ID)
#                     label = cv2.imread(label_file)[:,:,0]
#                     # convert label into a binary class: whether bike lane (1) or not (0)
#                     binary_label = label == 13
#                     binary_label = binary_label.astype(np.int0)
#                     # save as a png file
#                     cv2.imwrite(os.path.join(self.new_binary_folder,f"{dataset}/labels",key+".png"), binary_label)
                
#             # move the images into the respective folders
#             self.move_img_to_binary_folder(dataset)
            
#     def move_img_to_binary_folder(self, dataset):
#         """move input images to the new folder created in reclassify_labels_bl_only function
#         """
#         print("-"*10,f"working on images in {dataset}","-"*10)
#         # make a new folder for image
#         os.makedirs(os.path.join(self.new_binary_folder,f"{dataset}/images"), exist_ok = True)
#         image_file_list = glob.glob(os.path.join(self.root_dir,f"mapillary_vistas/{dataset}/images/*.jpg"))
#         for image_file in tqdm.tqdm(image_file_list):
#             # copy into the new folder
#             shutil.copy2(image_file,os.path.join(self.new_binary_folder,f"{dataset}/images/"))
            
#     def _create_data_loaders(self):
#         # data augmentation instance
#         transform = aug.Compose([aug.Flip(p=0.5)])
#         # ss feature extract instance
#         feature_extractor = SegformerFeatureExtractor(align=False, reduce_zero_label=False)

#         # create datasets
#         train_dataset = ImageSegmentationDataset(root_dir=os.path.join(self.root_dir,"mapillary_binary"), feature_extractor=feature_extractor, transforms=transform)
#         valid_dataset = ImageSegmentationDataset(root_dir=os.path.join(self.root_dir,"mapillary_binary"), feature_extractor=feature_extractor, transforms=None, train=False)
#         print("Number of training examples:", len(train_dataset))
#         print("Number of validation examples:", len(valid_dataset))
#         encoded_inputs = train_dataset[0]
#         print("pixel values: ",encoded_inputs["pixel_values"].shape,
#               "label shape: ", encoded_inputs["labels"].shape,
#               "label: ", encoded_inputs["labels"],
#               "unique label: ", encoded_inputs["labels"].squeeze().unique())
#         # create data loaders
#         train_dataloader = DataLoader(train_dataset, batch_size=32, shuffle=True)
#         valid_dataloader = DataLoader(valid_dataset, batch_size=32) 
#         return train_dataloader, valid_dataloader
    
#     def train_segmentation_model(self, epoch_num=100):
#         # set path to the checkpoint model file
#         path_checkpoint = os.path.join(self.model_folder,"segmentation_checkpoint.pt")
        
#         # create dataloaders
#         train_dataloader, valid_dataloader = self._create_data_loaders()
        
#         # load checkpoints if there is
#         if os.path.exists(path_checkpoint):
#             model = torch.load(path_checkpoint)
#             model.load_state_dict(model['model_state_dict'])
#             optimizer.load_state_dict(model['optimizer_state_dict'])
#             checkpoint_epoch = model['epoch']
#             accuracies_hist_list = model['accuracies_hist_list']
#             losses_hist_list = model['losses_hist_list']
#             val_accuracies_hist_list = model['val_accuracies_hist_list']
#             val_losses_hist_list = model['val_losses_hist_list']
#         # if not, then initialize a model
#         else:
#             checkpoint_epoch = 1
            
#             classes = self.label['name']
#             id2label = classes.to_dict()
#             label2id = {v: k for k, v in id2label.items()}
#             # configuration = SegformerConfig()
#             # model = SegformerForSemanticSegmentation(configuration)
#             model = SegformerForSemanticSegmentation.from_pretrained("nvidia/mit-b5", ignore_mismatched_sizes=True,
#                                                             num_labels=len(id2label), id2label=id2label, label2id=label2id,
#                                                             reshape_last_stage=True)
#             optimizer = AdamW(model.parameters(), lr=0.00006)
            
#             model.to(self.device)
#             print("Model Initialized!")
        
#         # train through epochs
#         for epoch in range(1, epoch_num+1):  # loop over the dataset multiple times
#             # skip to the checkpoint_epoch
#             if epoch < checkpoint_epoch:
#                 continue
#             print("Epoch:", epoch)
#             pbar = tqdm.tqdm(train_dataloader)
#             accuracies = []
#             losses = []
#             val_accuracies = []
#             val_losses = []
#             model.train()
#             for idx, batch in enumerate(pbar):
#                 # get the inputs;
#                 pixel_values = batch["pixel_values"].to(self.device)
#                 labels = batch["labels"].to(self.device)

#                 # zero the parameter gradients
#                 optimizer.zero_grad()

#                 # forward
#                 outputs = model(pixel_values=pixel_values, labels=labels)

#                 # evaluate
#                 upsampled_logits = nn.functional.interpolate(outputs.logits, size=labels.shape[-2:], mode="bilinear", align_corners=False)
#                 predicted = upsampled_logits.argmax(dim=1)

#                 mask = (labels != 255) # we don't include the background class in the accuracy calculation
#                 pred_labels = predicted[mask].detach().cpu().numpy()
#                 true_labels = labels[mask].detach().cpu().numpy()
#                 accuracy = accuracy_score(pred_labels, true_labels)
#                 loss = outputs.loss
#                 accuracies.append(accuracy)
#                 losses.append(loss.item())
#                 pbar.set_postfix({'Batch': idx, 'Pixel-wise accuracy': sum(accuracies)/len(accuracies), 'Loss': sum(losses)/len(losses)})

#                 # backward + optimize
#                 loss.backward()
#                 optimizer.step()
#             else:
#                 model.eval()
#                 with torch.no_grad():
#                     for idx, batch in enumerate(valid_dataloader):
#                         pixel_values = batch["pixel_values"].to(self.device)
#                         labels = batch["labels"].to(self.device)
                        
#                         outputs = model(pixel_values=pixel_values, labels=labels)
#                         upsampled_logits = nn.functional.interpolate(outputs.logits, size=labels.shape[-2:], mode="bilinear", align_corners=False)
#                         predicted = upsampled_logits.argmax(dim=1)
                        
#                         mask = (labels != 255) # we don't include the background class in the accuracy calculation
#                         pred_labels = predicted[mask].detach().cpu().numpy()
#                         true_labels = labels[mask].detach().cpu().numpy()
#                         accuracy = accuracy_score(pred_labels, true_labels)
#                         val_loss = outputs.loss
#                         val_accuracies.append(accuracy)
#                         val_losses.append(val_loss.item())
#             accuracy_epoch = sum(accuracies)/len(accuracies)
#             loss_epoch = sum(losses)/len(losses)
#             val_accuracy_epoch = sum(val_accuracies)/len(val_accuracies)
#             val_loss_epoch = sum(val_losses)/len(val_losses)
#             print(f"Train Pixel-wise accuracy: {accuracy_epoch}\
#                 Train Loss: {loss_epoch}\
#                 Val Pixel-wise accuracy: {val_accuracy_epoch}\
#                 Val Loss: {val_loss_epoch}")
            
#             # save a checkpoint
#             torch.save({
#                 'epoch': epoch,
#                 'model_state_dict': model.state_dict(),
#                 'optimizer_state_dict': optimizer.state_dict(),
#                 'accuracies_hist_list': accuracies_hist_list.append(accuracy_epoch),
#                 'losses_hist_list': losses_hist_list.append(loss_epoch),
#                 'val_accuracies_hist_list': val_accuracies_hist_list.append(val_accuracy_epoch),
#                 'val_losses_hist_list': val_losses_hist_list.append(val_loss_epoch)
#                 }, 
#                        path_checkpoint)
        
#         # save the trained model
#         torch.save(model, os.path.join(self.model_folder,"segmentation.pt"))
    
#     def infer(self):
#         # load model
#         model = torch.load(os.path.join(self.model_folder,"segmentation.pt"))
#         # set up feature extractor
#         feature_extractor_inference = SegformerFeatureExtractor(align=False, reduce_zero_label=False)
#         # loop through images
#         for image_file in tqdm.tqdm(glob.glob(os.path.join(self.input_folder, "gsv/image/perspective/*.png"))):
#             # read image_file to opencv
#             image = cv2.imread(image_file)
#             # get pid
#             image_file_name = os.path.split(image_file)[1]
#             pid = re.search("(.*)(?<=_Direction)", image_file_name).groups(1)[0]
#             pid = re.sub("_Direction", "", pid)
            
#             # get pixel_values after feature extraction
#             pixel_values = feature_extractor_inference(image, return_tensors="pt").pixel_values.to(self.device)
#             # get output logit from the model
#             model.eval()
#             outputs = model(pixel_values=pixel_values) # logits are of shape (batch_size, num_labels, height/4, width/4)
#             logits = outputs.logits.cpu()
#             # First, rescale logits to original image size
#             upsampled_logits = nn.functional.interpolate(logits,
#                             size=image.shape[:-1], # (height, width)
#                             mode='bilinear',
#                             align_corners=False)

#             # Second, apply argmax on the class dimension
#             seg = upsampled_logits.argmax(dim=1)[0]
#             #TODO save the result as png file
            
#             color_seg = np.zeros((seg.shape[0], seg.shape[1], 3), dtype=np.uint8) # height, width, 3\
#             for label, color in enumerate(self.label):
#                 color_seg[seg == label, :] = color
#                 # Convert to BGR
#                 color_seg = color_seg[..., ::-1]

#                 print(pixel_values.shape)
class ImageDetector:
    """Object detect SVI (only use a pretrained model since the objective is to get vehicles and bicycles)
    """
    
    def __init__(self, input_folder, output_folder, gsv_metadata_folder, pretrained_model = "facebook/detr-resnet-50"):
        self.pretrained_model = pretrained_model
        self.input_folder = input_folder
        self.output_folder = output_folder
        self.gsv_metadata_folder = gsv_metadata_folder
        os.makedirs(self.output_folder, exist_ok = True)
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        # filter image to remove invalid images
        filter_image(self.gsv_metadata_folder, self.input_folder)
        pass
    
    def detect_object(self):
        gsv_invalid_file = os.path.join(self.gsv_metadata_folder,"invalid_file_name.csv")
        # load and create a model
        feature_extractor = DetrFeatureExtractor.from_pretrained(self.pretrained_model).to(self.device)
        model = DetrForObjectDetection.from_pretrained(self.pretrained_model)
        
        infer_data = DetectionInferDataset(self.input_folder, gsv_invalid_file, feature_extractor, os.path.join(self.output_folder, "object_detection_raw.csv"))
        infer_loader = torch.utils.data.DataLoader(infer_data,
                                                batch_size=1,
                                                shuffle=False,
                                                num_workers=4,
                                                pin_memory=True)
        
        with torch.no_grad():
            for inputs, file_name in tqdm.tqdm(infer_loader, total = len(infer_loader)):
                try:
                    result_df = pd.read_csv(os.path.join(self.output_folder, "object_detection_raw.csv"))
                except FileNotFoundError:
                    result_df = pd.DataFrame(columns=["pid","label","score"])
                # get pid
                pid = re.search("(.*)(?<=_Direction)", file_name).groups(1)[0]
                pid = re.sub("_Direction", "", pid)
                # produce logits from the model
                inputs = inputs.to(self.device)
                outputs = model(pixel_values = inputs)
                # resize and infer
                results = feature_extractor.post_process(outputs, target_sizes=inputs.size())[0]
                # initialize df to save the result
                result_list_agg = []
                for score, label in zip(results["scores"], results["labels"]):
                    # let's only keep detections with score > 0.8
                    if score > 0.8:
                        result_list = [file_name, pid, model.config.id2label[label.item()], score.item()]
                        print(
                            f"Detected {model.config.id2label[label.item()]} with confidence "
                            f"{round(score.item(), 3)}"
                        )
                        result_list_agg.append(result_list)
                # if there's no result, then append only pid
                if len(result_list_agg) == 0:
                    result_list_agg.append([file_name, pid,None,None])
                # convert the result_list_agg to df and save as csv
                result_list_agg_df = pd.DataFrame(result_list_agg, columns=["file_name","pid","label","score"])
                result_df = pd.concat([result_df.reset_index(drop=True), result_list_agg_df.reset_index(drop=True)])
                result_df.to_csv(os.path.join(self.output_folder, "object_detection_raw.csv"), index = False)
                
    
    def count_vehicle_bicycle(self):
        """calculate total number of bycicycle and vehicles (car, motorcycle, bus, truck) under COCO detection 2017 dataset
        """
        # load the result of object detection as df
        result_df = pd.read_csv(os.path.join(self.output_folder, "object_detection.csv"))
        # filter with bicycle and group by pid
        result_bicycle = (result_df.
                          query("label == 'bicycle'").
                          groupby("pid").
                          agg(["sum"]).
                          reset_index()
                          )
        print(result_bicycle)
        # filter with vehicles (car, motorcycle, bus, truck) and group by pid
        result_vehicle = (result_df.
                          query("label == 'car' | label == 'motorcycle' | label == 'bus' | label == 'truck'").
                          groupby("pid").
                          agg(["sum"]).
                          reset_index()
                          )
        print(result_vehicle)
        # join the result back to gsv meta_data
        gsv_metadata = pd.read_csv(os.path.join(self.gsv_metadata_folder, "gsv_metadata.csv"))
        gsv_metadata = (gsv_metadata.
                        merge(result_bicycle, left_on = "panoid", right_on = "pid", how = "left").
                        merge(result_vehicle, left_on = "panoid", right_on = "pid", how = "left")
                        )
        print(gsv_metadata)
        # save the result as csv
        gsv_metadata.to_csv(os.path.join(self.output_folder, "/object_detection_bicycle_vehicle.csv"), index = False)


# class PerceptionPredictor:
#     """class for predicting perception score for PlacePulse 2.0 data
#     """
    
#     def __init__(self, input_folder, model_folder):
#         pass
    
# if __name__ == '__main__':
#     root_dir = "/Volumes/exfat/bike_svi/"
#     input_folder = os.path.join(root_dir,"data/raw")
#     output_folder = os.path.join(root_dir,"data/interim/cities")
#     model_folder = os.path.join(root_dir,"models")
#     # segmentation
#     image_segmenter = ImageSegmentationSimple(os.path.join(input_folder,"cities/London/gsv/image/perspective"),os.path.join(output_folder,"London"))
#     image_segmenter.segment_svi()
#     # image_segmenter = ImageSegmenterBikeLane(input_folder, model_folder)
#     # image_segmenter.reclassify_labels_bl_only()
#     # # object detection
#     # for city in os.listdir(input_folder):
#     #     if not city.startswith("."):
#     #         print(city)
#     #         image_detector = ImageDetector(os.path.join(input_folder,"cities"), output_folder, city)
#     #         image_detector.detect_object()
#     #     else:
#     #         pass