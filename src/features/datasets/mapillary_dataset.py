from torch.utils.data import Dataset
import os
import cv2
import numpy as np
from torchvision import transforms
import torch
import json 

def parse_config(root_dir):
        with open(os.path.join(root_dir, "config_v2.0.json")) as config_file:
            config = json.load(config_file)

        labels = config["labels"]

        class_names = []
        class_ids = []
        class_evaluate = []
        class_colors = []

        for label_id, label in enumerate(labels):
            class_names.append(label["readable"])
            class_ids.append(label_id)
            class_evaluate.append(label["evaluate"])
            class_colors.append(label["color"])
        print("There are {} labels in the config file".format(len(set(class_ids))))
        return np.array(class_names), np.array(class_ids), np.array(class_evaluate), np.array(class_colors)

class TrainingDataset(Dataset):
    """Image segmentation dataset."""

    def __init__(self, root_dir, feature_extractor, transforms=None, train=True):
        """
        Args:
            root_dir (string): Root directory of the dataset containing the images + annotations.
            feature_extractor (SegFormerFeatureExtractor): feature extractor to prepare images + segmentation maps.
            train (bool): Whether to load "training" or "validation" images + annotations.
        """
        # set folders
        self.root_dir = root_dir
        self.feature_extractor = feature_extractor
        self.train = train
        self.transforms = transforms
        
        # set image directories
        sub_path = "training" if self.train else "validation"
        self.img_dir = os.path.join(self.root_dir, sub_path, "images")
        self.ann_dir = os.path.join(self.root_dir, sub_path, "v2.0/labels")
        
        # set config related variables
        self.class_names, self.class_ids, self.class_evaluate, self.class_colors = parse_config(self.root_dir)
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
        
        # read images
        image_file_names = []
        for root, dirs, files in os.walk(self.img_dir):
            files = [file for file in files if not file.startswith(".")]
            image_file_names.extend(files)
        self.images = sorted(image_file_names)
        print(len(self.images))
        
        # read annotations
        annotation_file_names = []
        for root, dirs, files in os.walk(self.ann_dir):
            files = [file for file in files if not file.startswith(".")]
            annotation_file_names.extend(files)
        self.annotations = sorted(annotation_file_names)
        print(len(self.annotations))
        assert len(self.images) == len(self.annotations), "There must be as many images as there are segmentation maps"
    
    # def normalize(self, image):
    #     transform_ops = transforms.Compose([
    #         transforms.ToTensor(),
    #         transforms.Normalize(mean=(0.485, 0.456, 0.406), std=(0.229, 0.224, 0.225))
    #     ])
    #     return transform_ops(image).numpy().reshape(image.shape)
    
    def mask_transform(self, segmentation_map):
        # create a new mask
        new_segmentation_map = np.zeros((segmentation_map.shape[0], segmentation_map.shape[1]), dtype=np.uint8)
        R,C,D = np.where((segmentation_map == self.class_colors[:,None,None,:]).all(3))
        new_segmentation_map[C,D] = self.class_ids[R]
        # new_segmentation_map = np.expand_dims(new_segmentation_map, axis=2)
        return new_segmentation_map
        
    def __len__(self):
        return len(self.images)

    def __getitem__(self, idx):
        print(os.path.join(self.img_dir, self.images[idx]))
        image = cv2.imread(os.path.join(self.img_dir, self.images[idx]))
        image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
        # image = self.normalize(image)

        segmentation_map = cv2.imread(os.path.join(self.ann_dir, self.annotations[idx]))
        segmentation_map = cv2.cvtColor(segmentation_map, cv2.COLOR_BGR2RGB)
        # segmentation_map = self.mask_transform(segmentation_map)
        segmentation_map = cv2.cvtColor(segmentation_map, cv2.COLOR_BGR2GRAY)
        
#         image = Image.open()
#         segmentation_map = Image.open()

        if self.transforms is not None:
            augmented = self.transforms(image=image, mask=segmentation_map)
            # randomly crop + pad both image and segmentation map to same size
            encoded_inputs = self.feature_extractor(augmented['image'], augmented['mask'], return_tensors="pt")
        else:
            encoded_inputs = self.feature_extractor(image, segmentation_map, return_tensors="pt")

        for k,v in encoded_inputs.items():
            encoded_inputs[k].squeeze_() # remove batch dimension

        return encoded_inputs