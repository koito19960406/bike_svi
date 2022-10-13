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
import tensorflow as tf
import tensorflow_datasets as tfds

from datasets.simple_segmentation import SimpleSegmentationInferDataset
from datasets.detection import DetectionInferDataset
from datasets.mapillary_dataset import TrainingDataset, parse_config
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
        
class ImageSegmenterBikeLane:
    """class for segmenting street view images.
    """
    def __init__(self, input_folder, model_folder, output_folder):
        self.cpu_num = os.cpu_count()
        self.root_dir = input_folder
        self.model_folder = model_folder
        self.output_folder = output_folder
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        self.class_names, self.class_ids, self.class_evaluate, self.class_colors = parse_config(os.path.join(self.root_dir, "mapillary_vistas"))
        self.ignore_id = 255
        # set class_evaluate for those this study is not interested (change class_evaluate if they are in self.class_names)
        class_names_interested = np.array([
            "Pothole",
            "Street Light",
            "Bike Lane",
            "Utility Pole",
            "Bike Rack",
            "Traffic Sign - Direction (Back)",
            "Traffic Sign - Direction (Front)",
            "Curb",
            "Curb Cut",
            "Building",
            "Vegetation",
            "Sky"])
        self.class_evaluate = np.in1d(self.class_names, class_names_interested)
        # get index of classes to ignore
        ignore_index = np.flatnonzero(self.class_evaluate==False)
        # change class ids to ignore_id if they are ignore_index
        self.class_ids[ignore_index] = self.ignore_id
        # replace ids with 0,1,2,3...
        new_id = 0
        for index, class_id in enumerate(self.class_ids):
            if class_id != 255:
                self.class_ids[index]=new_id
                new_id += 1
        print(self.class_ids)
    def resize_map(self):
        def mapping(path,key):
            # resize
            image = tf.io.read_file(path)
            image = tf.io.decode_jpeg(image, channels = 3)
            image = tf.image.resize(image, [512, 512])
            return image, key
        
        def mapping_label(queue,path_out):
            # resize
            # image = tf.io.read_file(path)
            # image = tf.io.decode_jpeg(image, channels = 3)
            # image = tf.image.resize(image, [512, 512])
            # new_segmentation_map = tf.zeros((image.shape[0], image.shape[1]), dtype=np.uint8)
            # R,C,D = tf.experimental.numpy.all(tf.experimental.numpy.where((image == self.class_colors[:,None,None,:])))
            # new_segmentation_map[C,D] = self.class_ids[R]
            # image = new_segmentation_map
            # create a new mask
            # create progress bar
            pbar = tqdm.tqdm(total=queue.qsize())
            while True:
                if queue.empty():
                    break
                path = queue.get()
                segmentation_map = cv2.imread(path)
                segmentation_map = cv2.cvtColor(segmentation_map, cv2.COLOR_BGR2RGB)
                segmentation_map = cv2.resize(segmentation_map, (512, 512))
                new_segmentation_map = np.zeros((segmentation_map.shape[0], segmentation_map.shape[1]), dtype=np.uint8)
                R,C,D = np.where((segmentation_map == self.class_colors[:,None,None,:]).all(3))
                new_segmentation_map[C,D] = self.class_ids[R]
                # new_segmentation_map = np.expand_dims(new_segmentation_map, axis=2)
                cv2.imwrite(os.path.join(path_out, os.path.basename(path)), new_segmentation_map)
                # mark the unit of work complete
                queue.task_done()
                # update pbar
                pbar.update(1)
            # return image, key
        
        for data_split in ["training", "validation"]:
            for data_type in ["images", "v2.0/labels"]:
                path = os.path.join(self.root_dir, "mapillary_vistas", data_split, data_type)
                path_out = os.path.join(self.root_dir, "mapillary_vistas_resized", data_split, data_type)
                if not os.path.exists(path_out):
                    images = np.array([i for i in os.listdir(path) if i[:2]!= "._"], dtype = "U54")
                    image_paths = [os.path.join(path, i) for i in images]
                    os.makedirs(path_out, exist_ok=True)
                    if data_type == "images":
                        ds = tf.data.Dataset.from_tensor_slices((image_paths, images))
                        ds = ds.map(mapping, num_parallel_calls = tf.data.AUTOTUNE)
                        for img, path in tqdm.tqdm(tfds.as_numpy(ds)):
                            tf.keras.utils.save_img(os.path.join(path_out, path.decode("utf-8")), img)     
                    else:
                        queue = Queue()
                        [queue.put(i) for i in image_paths]
                        threads = [Thread(target=mapping_label, args=(queue, path_out), daemon=True) for index in tqdm.tqdm(range(self.cpu_num))]
                        for thread in threads:
                            thread.start()
                        # wait for threads to finish
                        for thread in threads:
                            thread.join()
                        # wait for all tasks in the queue to be processed
                        queue.join()
                            
                      
        
    def reclassify_labels_bl_only(self):
        """reclassify labels into binary whether it's bine lane or not
        """
        # create a new folder
        self.new_binary_folder = os.path.join(self.root_dir,"mapillary_binary")
        os.makedirs(self.new_binary_folder, exist_ok = True)
        for dataset in ["training","validation"]:
            print("-"*10,f"working on labels in {dataset}","-"*10)
            # create a new folder for each dataset
            os.makedirs(os.path.join(self.new_binary_folder,f"{dataset}/labels"), exist_ok = True)
            label_file_list = glob.glob(os.path.join(self.root_dir,f"mapillary_vistas/{dataset}/v2.0/labels/*.png"))
            for label_file in tqdm.tqdm(label_file_list):
                key = os.path.split(label_file)[1][:-4]
                if not os.path.exists(os.path.join(self.new_binary_folder,f"{dataset}/labels",key+".png")):
                    # get the 1st of the 3rd dimention only because all the RGB layers have the same values (i.e., Class ID)
                    label = cv2.imread(label_file)[:,:,0]
                    # convert label into a binary class: whether bike lane (1) or not (0)
                    binary_label = label == 13
                    binary_label = binary_label.astype(np.int0)
                    # save as a png file
                    cv2.imwrite(os.path.join(self.new_binary_folder,f"{dataset}/labels",key+".png"), binary_label)
                
            # move the images into the respective folders
            self.move_img_to_binary_folder(dataset)
            
    def move_img_to_binary_folder(self, dataset):
        """move input images to the new folder created in reclassify_labels_bl_only function
        """
        print("-"*10,f"working on images in {dataset}","-"*10)
        # make a new folder for image
        os.makedirs(os.path.join(self.new_binary_folder,f"{dataset}/images"), exist_ok = True)
        image_file_list = glob.glob(os.path.join(self.root_dir,f"mapillary_vistas/{dataset}/images/*.jpg"))
        for image_file in tqdm.tqdm(image_file_list):
            # copy into the new folder
            shutil.copy2(image_file,os.path.join(self.new_binary_folder,f"{dataset}/images/"))
            
    def _create_data_loaders(self, binary = False):
        # data augmentation instance
        transform = aug.Compose([aug.Flip(p=0.5)])
        # ss feature extract instance
        feature_extractor = SegformerFeatureExtractor(align=False, reduce_zero_label=False)

        # create datasets
        if binary:
            train_dataset = TrainingDataset(root_dir=os.path.join(self.root_dir,"mapillary_binary"), feature_extractor=feature_extractor, transforms=transform)
            valid_dataset = TrainingDataset(root_dir=os.path.join(self.root_dir,"mapillary_binary"), feature_extractor=feature_extractor, transforms=None, train=False)
        else:
            train_dataset = TrainingDataset(root_dir=os.path.join(self.root_dir,"mapillary_vistas_resized"), feature_extractor=feature_extractor, transforms=transform)
            valid_dataset = TrainingDataset(root_dir=os.path.join(self.root_dir,"mapillary_vistas_resized"), feature_extractor=feature_extractor, transforms=None, train=False)
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
        id2label = {int(id): label for (id, label) in zip(self.class_ids, self.class_names)}
        label2id = {v: k for k, v in id2label.items()}
        model = SegformerForSemanticSegmentation.from_pretrained("nvidia/mit-b5", ignore_mismatched_sizes=True,
                                                        num_labels=len(id2label), id2label=id2label, label2id=label2id,
                                                        reshape_last_stage=True)
        optimizer = AdamW(model.parameters(), lr=0.00006)
        # load checkpoints if there is
        if os.path.exists(path_checkpoint):
            check_point = torch.load(path_checkpoint)
            model.load_state_dict(check_point['model_state_dict'])
            optimizer.load_state_dict(check_point['optimizer_state_dict'])
            checkpoint_epoch = check_point['epoch']
            accuracies_hist_list = check_point['accuracies_hist_list']
            losses_hist_list = check_point['losses_hist_list']
            val_accuracies_hist_list = check_point['val_accuracies_hist_list']
            val_losses_hist_list = check_point['val_losses_hist_list']
        # if not, then initialize a model
        else:
            checkpoint_epoch = 0
            accuracies_hist_list = []
            losses_hist_list = []
            val_accuracies_hist_list = []
            val_losses_hist_list = []
            model.to(self.device)
            print("Model Initialized!")
        
        # train through epochs
        for epoch in range(1, epoch_num+1):  # loop over the dataset multiple times
            # skip to the checkpoint_epoch
            if epoch <= checkpoint_epoch:
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
            
            accuracies_hist_list.append(accuracy_epoch)
            losses_hist_list.append(loss_epoch)
            val_accuracies_hist_list.append(val_accuracy_epoch)
            val_losses_hist_list.append(val_loss_epoch)
            # save a checkpoint
            torch.save({
                'epoch': epoch,
                'model_state_dict': model.state_dict(),
                'optimizer_state_dict': optimizer.state_dict(),
                'accuracies_hist_list': accuracies_hist_list,
                'losses_hist_list': losses_hist_list,
                'val_accuracies_hist_list': val_accuracies_hist_list,
                'val_losses_hist_list': val_losses_hist_list
                }, 
                       path_checkpoint)
        
        # save the trained model
        torch.save(model, os.path.join(self.model_folder,"segmentation.pt"))
    
    def infer(self, load_checkpoint = False):
        # final dictionary
        seg_dict_final = dict()
        # load model
        if load_checkpoint:
            id2label = {int(id): label for (id, label) in zip(self.class_ids, self.class_names)}
            label2id = {v: k for k, v in id2label.items()}
            model = SegformerForSemanticSegmentation.from_pretrained("nvidia/mit-b5", ignore_mismatched_sizes=True,
                                                            num_labels=len(id2label), id2label=id2label, label2id=label2id,
                                                            reshape_last_stage=True).to(self.device)
            check_point = torch.load(os.path.join(self.model_folder,"segmentation_checkpoint.pt"))
            model.load_state_dict(check_point["model_state_dict"])
            model.eval()
        else:
            model = torch.load(os.path.join(self.model_folder,"segmentation.pt")).to(self.device)
            model.eval()
        # set up feature extractor
        feature_extractor_inference = SegformerFeatureExtractor(align=False, reduce_zero_label=False)
        gsv_invalid_file = os.path.join(self.gsv_metadata_folder,"invalid_file_name.csv")
        
        infer_data = SimpleSegmentationInferDataset(self.input_folder, gsv_invalid_file, feature_extractor_inference, self.img_output_folder)
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
                total_pixel = seg.shape[0] * seg.shape[1]
                values, counts = np.unique(seg, return_counts=True)
                seg_dict = [{value: count/total_pixel} for (value, count) in zip(values, counts)]
                seg_dict_final[file_name] = seg_dict
            # save seg_dict_final as feather
            with open(os.path.join(self.output_folder, 'segmentation_result.json'), 'w') as fp:
                json.dump(seg_dict_final, fp)
                
    def json_to_df(self):
        """function to convert json to csv
        """
        with open(os.path.join(self.output_folder, 'segmentation_result.json'), 'r') as file:
            seg_dict_final = json.load(file)
        
        df = pd.json_normalize(seg_dict_final, max_level=0)
        
        # save as csv
        df.to_csv(os.path.join(self.output_folder, 'segmentation_result.csv'))
    
class ImageDetector:
    """Object detect SVI (only use a pretrained model since the objective is to get vehicles and bicycles)
    """
    
    def __init__(self, input_folder, output_folder, gsv_metadata_folder, pretrained_model = "facebook/detr-resnet-50"):
        self.pretrained_model = pretrained_model
        self.input_folder = input_folder
        self.output_folder = output_folder
        self.temp_output_folder = os.path.join(self.output_folder,"detection")
        self.gsv_metadata_folder = gsv_metadata_folder
        os.makedirs(self.temp_output_folder, exist_ok = True)
        self.device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
        # filter image to remove invalid images
        filter_image(self.gsv_metadata_folder, self.input_folder)
        pass
    
    def detect_object(self):
        gsv_invalid_file = os.path.join(self.gsv_metadata_folder,"invalid_file_name.csv")
        # load and create a model
        feature_extractor = DetrFeatureExtractor.from_pretrained(self.pretrained_model)
        model = DetrForObjectDetection.from_pretrained(self.pretrained_model).to(self.device)
        infer_data = DetectionInferDataset(self.input_folder, gsv_invalid_file, feature_extractor, self.temp_output_folder)
        infer_loader = torch.utils.data.DataLoader(infer_data,
                                                batch_size=1, # keep this to 1
                                                shuffle=False,
                                                num_workers=4,
                                                pin_memory=True)
        
        print(len(infer_loader))
        with torch.no_grad():
            # batch count to bundle result into feather file
            # save to feather every 500 count
            batch_count = 1
            batch_size = 500 
            # initialize df to save the result
            result_list_agg = []
            for inputs, file_name in tqdm.tqdm(infer_loader, total = len(infer_loader)):
                # get pid
                pid = re.search("(.*)(?<=_Direction)", file_name[0]).groups(1)[0]
                pid = re.sub("_Direction", "", pid)
                # produce logits from the model
                inputs = inputs.to(self.device)
                outputs = model(pixel_values = inputs)
                img_size = torch.tensor([inputs.size()[2], inputs.size()[3]])
                target_sizes = torch.unsqueeze(img_size, dim=0)
                # resize and infer
                results = feature_extractor.post_process(outputs, target_sizes=target_sizes)[0]
                for score, label in zip(results["scores"], results["labels"]):
                    # let's only keep detections with score > 0.8
                    if score > 0.8:
                        result_list = [file_name[0], pid, model.config.id2label[label.item()], score.item()]
                        # print(
                        #     f"Detected {model.config.id2label[label.item()]} with confidence "
                        #     f"{round(score.item(), 3)}"
                        # )
                        result_list_agg.append(result_list)
                if batch_count % batch_size == 0:
                    # convert the result_list_agg to df and save as csv
                    result_list_agg_df = pd.DataFrame(result_list_agg, columns=["file_name","pid","label","score"])
                    result_list_agg_df.to_feather(os.path.join(self.output_folder, f"object_detection_{str(batch_count)}.feather"), index = False)
                    result_list_agg = []
                # addd 1 to batch_count
                batch_count += 1
            # save the remaining data to the latest feather file
            if len(result_list_agg) != 0:
                result_list_agg_df = pd.DataFrame(result_list_agg, columns=["file_name","pid","label","score"])
                if batch_count < batch_size:
                    result_list_agg_df.to_feather(os.path.join(self.output_folder, f"object_detection_{str(batch_count)}.feather"), index = False)
                else:
                    latest_batch_num = (batch_count // batch_size) * batch_size
                    latest_df = pd.read_feather(os.path.join(self.output_folder, f"object_detection_{str(latest_batch_num)}.feather"))
                    result_list_agg_df = pd.concat([latest_df, result_list_agg_df], ignore_index = True)
                    result_list_agg_df.to_feather(os.path.join(self.output_folder, f"object_detection_{str(latest_batch_num)}.feather"), index = False)
    
    def count_objects(self):
        """calculate total number of bycicycle and vehicles (car, motorcycle, bus, truck) under COCO detection 2017 dataset
        """
        feather_list = glob.glob(os.path.join(self.output_folder, "*.feather"))
        dfs = [dask.delayed(feather.read_feather)(f) for f in feather_list]
        df = dd.from_delayed(dfs)
        result_df = df.drop_duplicates().compute()
        # filter with person and group by pid
        result_person = (result_df.
                        query("label == 'person'").
                        groupby("pid").
                        agg(["count"]).
                        reset_index().
                        droplevel(level=1, axis=1).
                        iloc[:,:2].
                        rename(columns=lambda s: s.replace("file_name", "person_count"))
                        )
        print(result_person)
        # filter with bicycle and group by pid
        result_bicycle = (result_df.
                        query("label == 'bicycle'").
                        groupby("pid").
                        agg(["count"]).
                        reset_index().
                        droplevel(level=1, axis=1).
                        iloc[:,:2].
                        rename(columns=lambda s: s.replace("file_name", "bicycle_count"))
                        )
        print(result_bicycle)
        # filter with vehicles (car, motorcycle, bus, truck) and group by pid
        result_vehicle = (result_df.
                        query("label == 'car' | label == 'motorcycle' | label == 'bus' | label == 'truck'").
                        groupby("pid").
                        agg(["count"]).
                        reset_index().
                        droplevel(level=1, axis=1).
                        iloc[:,:2].
                        rename(columns=lambda s: s.replace("file_name", "vehicle_count"))
                        )
        print(result_vehicle)
        # join the result back to gsv meta_data
        gsv_metadata = pd.read_csv(os.path.join(self.gsv_metadata_folder, "gsv_metadata.csv"))
        gsv_metadata = (gsv_metadata.
                        merge(result_person, left_on = "panoid", right_on = "pid", how = "left").
                        merge(result_bicycle, left_on = "panoid", right_on = "pid", how = "left").
                        merge(result_vehicle, left_on = "panoid", right_on = "pid", how = "left")
                        )
        gsv_metadata = gsv_metadata.loc[:,~gsv_metadata.columns.str.startswith('pid')]
        print(gsv_metadata)
        # save the result as csv
        gsv_metadata.to_csv(os.path.join(self.output_folder, "object_detection_bicycle_vehicle.csv"), index = False)
        
# class PerceptionPredictor:
#     """class for predicting perception score for PlacePulse 2.0 data
#     """
    
#     def __init__(self, input_folder, model_folder):
#         pass
    
if __name__ == '__main__':
    root_dir = "/Volumes/ExFAT/bike_svi/"
    if not os.path.exists(root_dir):
        root_dir = r"E:/bike_svi/"
    input_folder = os.path.join(root_dir,"data/raw")
    output_folder = os.path.join(root_dir,"data/interim/cities")
    model_folder = os.path.join(root_dir,"models")
    #  # segmentation
    # image_segmenter = ImageSegmenterBikeLane(input_folder, model_folder, os.path.join(output_folder, "London"))
    # image_segmenter.resize_map()
    # image_segmenter._create_data_loaders()
    # image_segmenter.train_segmentation_model()
    # image_segmenter.segment_svi()
    # image_segmenter = ImageSegmenterBikeLane(input_folder, model_folder)
    # image_segmenter.reclassify_labels_bl_only()
    # object detection
    for city in os.listdir(os.path.join(input_folder,"cities")):
        if not city.startswith("."):
            print(city) 
            gsv_metadata_folder = os.path.join(input_folder,"cities",city,"gsv/metadata")
            image_detector = ImageDetector(os.path.join(input_folder,"cities", city, "gsv/image/perspective"), 
                os.path.join(output_folder, city),
                gsv_metadata_folder)
            # image_detector.detect_object()
            image_detector.count_objects()
    #     else:
    #         pass