from torch.utils.data import Dataset
import os
import pandas as pd
from PIL import Image
import cv2 
import torch
import tqdm
from multiprocessing.pool import ThreadPool
import numpy as np
import itertools

class DetectionInferDataset(Dataset):
    def __init__(self, img_dir, gsv_invalid_file, feature_extractor, object_detection_raw_file):
        super().__init__()
        global segmented_image_file_list
        global gsv_invalid_df
        # set variables
        self.img_dir = img_dir
        self.feature_extractor = feature_extractor
        try:
            self.object_detection_raw = pd.read_csv(object_detection_raw_file)
            detected_image_file_list = self.object_detection_raw["file_name"]
        except:
            detected_image_file_list = None
        gsv_invalid_df = pd.read_csv(gsv_invalid_file)
        self.cpu_num = os.cpu_count()
        # # load images and pass to feature extractor
        # image_list = []
        # for file_name in tqdm.tqdm(os.listdir(self.img_dir)):
        #     file_name_key = file_name[:-4]
        #     if not gsv_invalid_file["file_name"].str.contains(file_name_key).any():
        #         # run the following only if the file_name doesn't exist in the file yet
        #         if len(segmented_image_file_list) > 0:
        #             # skip if it's already been sgemented or invalids
        #             if not segmented_image_file_list.str.contains(file_name_key).any():
        #                 image_list.append(os.path.join(self.img_dir,file_name))
        #             else:
        #                 continue
        #         else:
        #             image_list.append(os.path.join(self.img_dir,file_name))
        #     else:
        #         continue
        # self.images = image_list
        
        
        def check_file(image_file):
            file_name_key = image_file[:-4]
            if not gsv_invalid_df["file_name"].str.contains(file_name_key).any():
                # run the following only if the file_name doesn't exist in the file yet
                if detected_image_file_list is not None:
                    # skip if it's already been sgemented or invalids
                    if not detected_image_file_list.str.contains(file_name_key).any():
                        return os.path.join(self.img_dir,image_file)
                    else:
                        return
                else:
                    return os.path.join(self.img_dir,image_file)
            else:
                return
            
        # apply the function to each row
        def parallelize_function(image_file_list):
            output_list = []
            for image_file in tqdm.tqdm(image_file_list):
                output_temp = check_file(image_file)
                output_list.append(output_temp)
            return output_list
        
        # split the input df and map the input function
        def parallelize_list(cpu_num, input_list, outer_func):
            final_list = []
            num_processes = cpu_num
            pool = ThreadPool(processes=num_processes)
            input_list_split = np.array_split(input_list, num_processes)
            output_list = list(itertools.chain(pool.map(outer_func, input_list_split)))
            # print(pool.map(outer_func, input_list_split))
            print("-"*10,output_list,"-"*10,)
            return output_list
        
        output_list = parallelize_list(self.cpu_num, os.listdir(self.img_dir), parallelize_function)
        self.images = output_list

    def __len__(self):
        try:
            return len(self.images)
        except TypeError:
            return 0

    def __getitem__(self, idx):
        image = cv2.imread(self.images[idx])
        image = cv2.cvtColor(image, cv2.COLOR_BGR2RGB)
        inputs = torch.squeeze(self.feature_extractor(images=image, return_tensors="pt").pixel_values)
        # file base name
        file_name = os.path.basename(self.images[idx])[:-4]
        return inputs, file_name