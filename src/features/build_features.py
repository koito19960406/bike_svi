import os 
import pandas as pd

from cv_models import ImageSegmentationSimple, ImageDetector

class CreateFeatures:
    """class to run all the classes to extract features
    """
    def __init__(self, root_dir, city):
        self.root_dir = root_dir
        self.city = city
        self.output_folder = os.path.join(self.root_dir, "processed", self.city)
        pass
    
    def target_var(self):
        # read data and filter to get necessary columns only
        count_data = pd.read_csv(os.path.join(self.root_dir,"external",self.city,"count_data.csv"))
        count_data = count_data.filter(items = ["count_point_id","year","pedal_cycles"])
        # save the data in interim folder
        count_data.to_csv(os.path.join(self.output_folder,"count_data.csv"))
        
    #TODO add classes to run
    def segmentation(self):
        input_folder = os.path.join(self.root_dir, "raw/cities", self.city, "gsv/image/perspective")
        gsv_metadata_folder = os.path.join(self.root_dir, "raw/cities", self.city, "gsv/metadata")
        img_output_folder = os.path.join(self.root_dir, "interim/cities", self.city, "segmented")
        csv_output_folder = os.path.join(self.root_dir, "processed/cities", self.city)
        image_segmentation = ImageSegmentationSimple(input_folder,  gsv_metadata_folder, img_output_folder, csv_output_folder)
        image_segmentation.filter_image()
        image_segmentation.segment_svi()
        
    def detcetion(self):
        input_folder = os.path.join(self.root_dir, "raw/cities", self.city, "gsv/image/perspective")
        gsv_metadata_folder = os.path.join(self.root_dir, "raw/cities", self.city, "gsv/metadata")
        output_folder = os.path.join(self.root_dir, "processed/cities", self.city)
        image_detector = ImageDetector(input_folder, output_folder, gsv_metadata_folder, self.city)
        image_detector.detect_object()
        image_detector.count_vehicle_bicycle()
        
class AssembleFeatures:
    """class to assemble all the variables to be used in the causal inferene models later
    """
    def __init__(self, input_folder, output_folder):
        self.input_folder = input_folder
        self.output_folder = output_folder
        pass
    
    def join_data(self):
        count_data = pd.read_csv(os.path.join(self.inp))
        control_variables = pd.read_csv(os.path.join(self.input_folder, "count_control_variables.csv"))
        
if __name__ == "__main__":
    root_dir = "/Volumes/exfat/bike_svi/data"
    city_list = []
    with open(os.path.join(root_dir,"external/city_list.txt"), "r") as f:
        for line in f:
            city_list.append(line.strip())
    for city in city_list:
        create_features = CreateFeatures(root_dir, city)
        create_features.segmentation()
        create_features.detcetion()