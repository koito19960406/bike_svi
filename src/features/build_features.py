import os 
import pandas as pd

from cv_models import ImageSegmentationSimple, ImageDetector
from control_var import ControlVariables

class CreateFeatures:
    """class to run all the classes to extract features
    """
    def __init__(self, root_dir, city):
        self.root_dir = root_dir
        self.city = city
        self.output_folder = os.path.join(self.root_dir, "processed/cities", self.city)
        pass
    
    def target_var(self):
        if not os.path.exists(os.path.join(self.output_folder,"count_data.csv")):
            # read data and filter to get necessary columns only
            count_data = pd.read_csv(os.path.join(self.root_dir,"external/cities",self.city,"count_data.csv"))
            count_data = count_data.filter(items = ["count_point_id","year","pedal_cycles"])
            # save the data in interim folder
            count_data.to_csv(os.path.join(self.output_folder,"count_data.csv"), index = False)
        else:
            print("Target variable file already exists")

    def control_var(self):
        input_folder = os.path.join(self.root_dir,"external/cities",self.city)
        control_variables = ControlVariables(input_folder, self.output_folder)
        
    #TODO add classes to run
    def segmentation(self, segment = True):
        input_folder = os.path.join(self.root_dir, "raw/cities", self.city, "gsv/image/perspective")
        gsv_metadata_folder = os.path.join(self.root_dir, "raw/cities", self.city, "gsv/metadata")
        img_output_folder = os.path.join(self.root_dir, "interim/cities", self.city)
        csv_output_folder = os.path.join(self.root_dir, "processed/cities", self.city)
        image_segmentation = ImageSegmentationSimple(input_folder,  gsv_metadata_folder, img_output_folder, csv_output_folder)
        if segment:
            image_segmentation.segment_svi()
        image_segmentation.calculate_ratio(update = True)
        
    def detection(self):
        input_folder = os.path.join(self.root_dir, "raw/cities", self.city, "gsv/image/perspective")
        gsv_metadata_folder = os.path.join(self.root_dir, "raw/cities", self.city, "gsv/metadata")
        output_folder = os.path.join(self.root_dir, "processed/cities", self.city)
        image_detector = ImageDetector(input_folder, output_folder, gsv_metadata_folder, self.city)
        image_detector.detect_object()
        image_detector.count_vehicle_bicycle()
    
    def join_data(self):
        """join all the variables by pid and station id
        """
        count_data = pd.read_csv(os.path.join(os.path.join(self.output_folder,"count_data.csv")))
        gsv_metadata = pd.read_csv(os.path.join(self.root_dir, "raw/cities", self.city, "gsv/metadata/gsv_metadata_dist.csv"))
        # control_variables = pd.read_csv(os.path.join(self.output_folder, "count_control_variables.csv"))
        segmentation = pd.read_csv(os.path.join(self.root_dir, "processed/cities", self.city, "segmentation_pixel_ratio_wide.csv"))
        
        # join gsv metadata and segmentation result by panoid and count station id
        gsv_seg = gsv_metadata[["panoid","year", "count_point_id"]].merge(segmentation, left_on = "panoid", right_on = "pid", how = "left")
        
        # join count data and gsv_seg
        gsv_seg_count  = (gsv_seg.merge(count_data, on = ["count_point_id", "year"], how = "left").
                          drop_duplicates().
                          dropna(subset=["pedal_cycles"]))
        # create period
        def make_periods(year):
            if 2008 <= year <= 2012:
                return 1
            elif 2013 <= year <= 2017:
                return 2 
            elif 2018 <= year <= 2020:
                return 3
        gsv_seg_count["period"] = gsv_seg_count["year"].map(lambda year: make_periods(year))
        
        # save as csv
        gsv_seg_count.to_csv(os.path.join(self.output_folder, "all_var_joined.csv"), index = False)
        
        
if __name__ == "__main__":
    root_dir = "/Volumes/exfat/bike_svi/data"
    if not os.path.exists(root_dir):
        root_dir = r'E:Koichi\exfat\bike_svi\data'
    city_list = []
    with open(os.path.join(root_dir,"external/city_list.txt"), "r") as f:
        for line in f:
            city_list.append(line.strip())
    for city in city_list:
        create_features = CreateFeatures(root_dir, city)
        create_features.target_var()
        create_features.segmentation(segment = False)
        create_features.join_data()
        # create_features.detection()
        # create_features.control_var()
        