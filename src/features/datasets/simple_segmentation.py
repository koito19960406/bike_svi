from torch.utils.data import Dataset
import os
import pandas as pd
from PIL import Image
import cv2 
import torch

class SimpleSegmentationInferDataset(Dataset):
    def __init__(self, img_dir, gsv_invalid_file, feature_extractor, segmented_dir):
        super().__init__()

        # set variables
        self.img_dir = img_dir
        self.feature_extractor = feature_extractor
        self.segmented_dir = segmented_dir
        segmented_image_file_list = pd.Series(os.listdir(self.segmented_dir))
        gsv_invalid_file = pd.read_csv(gsv_invalid_file)
        
        # load images and pass to feature extractor
        image_list = []
        for file_name in os.listdir(self.img_dir):
            file_name_key = file_name[:-4]
            # run the following only if the file_name doesn't exist in the file yet
            if len(segmented_image_file_list) > 0:
                # skip if it's already been sgemented or invalids
                if (not segmented_image_file_list.str.contains(file_name_key).any()) | (not gsv_invalid_file["file_name"].str.contains(file_name_key).any()):
                    image_list.append(os.path.join(self.img_dir,file_name))
            else:
                image_list.append(os.path.join(self.img_dir,file_name))
        self.images = image_list

    def __len__(self):
        return len(self.images)

    def __getitem__(self, idx):
        image = cv2.imread(self.images[idx])
        image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
        inputs = torch.squeeze(self.feature_extractor(images=image, return_tensors="pt").pixel_values)
        # file base name
        file_name = os.path.basename(self.images[idx])[:-4]
        return inputs, file_name