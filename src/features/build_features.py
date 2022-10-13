import os 
import pandas as pd
import subprocess
import numpy as np
import math
from tqdm import tqdm
from cv_models import ImageSegmentationSimple, ImageDetector, ImageSegmenterBikeLane
from control_var import ControlVariables
from gis_var import GISVariables

class CreateFeatures:
    """class to run all the classes to extract features
    """
    def __init__(self, root_dir, city):
        self.root_dir = root_dir
        self.city = city
        self.input_folder = os.path.join(self.root_dir,"data/external/cities",self.city)
        self.output_folder = os.path.join(self.root_dir, "data/processed/cities", self.city)
        pass
    
    def target_var(self):
        if not os.path.exists(os.path.join(self.output_folder,"count_data.csv")):
            # read data and filter to get necessary columns only
            count_data = pd.read_csv(os.path.join(self.root_dir,"data/external/cities",self.city,"count_data.csv"))
            count_data = count_data.filter(items = ["count_point_id","year","pedal_cycles"])
            # save the data in interim folder
            count_data.to_csv(os.path.join(self.output_folder,"count_data.csv"), index = False)
        else:
            print("Target variable file already exists")

    def control_var(self):
        control_variables = ControlVariables(self.input_folder, self.output_folder)
        control_variables.main()
        pass
    
    def gis_var(self):
        gis_variables = GISVariables(self.input_folder, self.output_folder)
        # gis_variables.retrieve_poi()
        # run r script for slope
        subprocess.call (["/usr/local/bin/Rscript", "--vanilla", "src/features/terrain.R"])
        
    #TODO add classes to run
    def segmentation(self, segment = True):
        input_folder = os.path.join(self.root_dir, "data/raw/cities", self.city, "gsv/image/perspective")
        gsv_metadata_folder = os.path.join(self.root_dir, "data/raw/cities", self.city, "gsv/metadata")
        img_output_folder = os.path.join(self.root_dir, "data/interim/cities", self.city)
        csv_output_folder = os.path.join(self.root_dir, "data/processed/cities", self.city)
        model_folder = os.path.join(root_dir,"models")
        # image_segmentation = ImageSegmentationSimple(input_folder,  gsv_metadata_folder, img_output_folder, csv_output_folder)
        # if segment:
        #     image_segmentation.segment_svi()
        # image_segmentation.calculate_ratio(update = True)
        image_segmenter = ImageSegmenterBikeLane(input_folder, model_folder, img_output_folder)
        if not os.path.exists(os.path.join(self.root_dir,"data/raw/mapillary_vistas_resized")):
            image_segmenter.resize_map()
        if not os.path.exists(os.path.join(model_folder, "segmentation_checkpoint.pt")) or not os.path.exists(os.path.join(model_folder, "segmentation.pt")):  
            image_segmenter._create_data_loaders()
            image_segmenter.train_segmentation_model()
        image_segmenter.infer()
        
    def detection(self):
        input_folder = os.path.join(self.root_dir, "data/raw/cities", self.city, "gsv/image/perspective")
        gsv_metadata_folder = os.path.join(self.root_dir, "data/raw/cities", self.city, "gsv/metadata")
        output_folder = os.path.join(self.root_dir, "data/processed/cities", self.city)
        image_detector = ImageDetector(input_folder, output_folder, gsv_metadata_folder, self.city)
        image_detector.detect_object()
        image_detector.count_objects()
    
    def const_bikeability_index(self):
        # load segmentation and detection result files
        segmentation = pd.read_csv(os.path.join(os.path.join(self.output_folder, 'segmentation_result.csv')))
        detection = pd.read_csv(os.path.join(self.output_folder, "object_detection_count.csv"))
        
        # mutate sub-indicators into a range of 0-1
        def continuous_to_binary_positive(value, threshold = 0.001):
            if value > threshold:
                return 1
            else:
                return 0
        def continuous_to_binary_negative(value, threshold = 0.001):
            if value > threshold:
                return 0
            else:
                return 1
            
        segmentation["Pothole"] = segmentation["Pothole"].map(continuous_to_binary_negative)
        segmentation["Street Light"] = segmentation["Street Light"].map(continuous_to_binary_positive)
        segmentation["Bike Lane"] = segmentation["Bike Lane"].map(continuous_to_binary_positive)
        segmentation["Utility Pole"] = segmentation["Utility Pole"].map(continuous_to_binary_negative)
        segmentation["Bike Rack"] = segmentation["Bike Rack"].map(continuous_to_binary_positive)
        segmentation["Traffic Sign - Direction (Back)"] = segmentation["Traffic Sign - Direction (Back)"].map(continuous_to_binary_positive)
        segmentation["Traffic Sign - Direction (Front)"] = segmentation["Traffic Sign - Direction (Front)"].map(continuous_to_binary_positive)
        segmentation["Curb"] = segmentation["Curb"].map(continuous_to_binary_negative)
        segmentation["Curb Cut"] = segmentation["Curb Cut"].map(continuous_to_binary_positive)
        segmentation["Building"] = segmentation["Building"].map(continuous_to_binary_positive)
        segmentation["Vegetation"] = segmentation["Vegetation"].map(continuous_to_binary_positive)
        pass
    
    def join_data(self):
        """join all the variables by pid and station id
        """
        # target variable (i.e., count data)
        count_data = pd.read_csv(os.path.join(os.path.join(self.output_folder,"count_data.csv")))
        # visual data
        gsv_metadata = pd.read_csv(os.path.join(self.root_dir, "data/raw/cities", self.city, "gsv/metadata/gsv_metadata_dist.csv"))
        segmentation = pd.read_csv(os.path.join(self.root_dir, "data/processed/cities", self.city, "segmentation_pixel_ratio_wide.csv"))
        detection = pd.read_csv(os.path.join(self.output_folder, "object_detection_count.csv"))
        # join gsv metadata and detection result by panoid and count station id
        gsv_det = gsv_metadata[["panoid","year", "count_point_id"]].merge(detection, left_on = "panoid", right_on = "pid", how = "left")
        # join gsv metadata and segmentation result by panoid and count station id
        gsv_seg = gsv_metadata[["panoid","year", "count_point_id"]].merge(segmentation, left_on = "panoid", right_on = "pid", how = "left")
        # compute visual complexity here
        def compute_complexity(df):
            visual_df = df.iloc[4:]
            count_col_log = math.log(visual_df.shape[0])
            def p_log_p(pix_ratio):
                if pix_ratio == 0:
                    return 0
                else:
                    return pix_ratio * math.log(pix_ratio)
            sum_p_log_p = visual_df.apply(p_log_p).sum()
            return -1*(sum_p_log_p)/count_col_log
        tqdm.pandas()
        gsv_seg["visual_complexity"] = gsv_seg.progress_apply(compute_complexity,axis=1)
        # other covariates
        control_variables = pd.read_csv(os.path.join(self.output_folder, "count_control_variables.csv")).drop_duplicates(subset=["count_point_id"])
        # assign imd score to the nearest years
        existing_year = np.array([2010,2015,2019])
        for year in range(2008,2021):
            diff_year = existing_year - year
            index = np.argmin(diff_year) 
            control_variables[f"IMD_score_{str(year)}"] = control_variables[f"IMD_score_{str(existing_year[index])}"]
        # filter columns
        #TODO fix filtering columns
        control_col_list = ["0 - 9", "10 - 19", "20 - 29", "30 - 39", "40 - 49", "50 - 59", "60 - 69", "70 - 79", "80 - 89", "90+", "all_ages", "IMD_score"]
        control_col_list_extended = control_col_list + ["count_point_id","year"]
        control_variables_cleaned = control_variables.filter(regex='|'.join(control_col_list_extended)).dropna(how='all')
        control_variables_long = pd.wide_to_long(control_variables_cleaned,
            control_col_list, 
            i="count_point_id", 
            j="year",
            sep='_').reset_index()
        # modify column names for clarity
        control_variables_long.columns = control_variables_long.columns.str.replace(" - ", "_").str.replace("+", "")
        # poi
        poi = pd.read_csv(os.path.join(self.root_dir, "data/processed/cities", self.city, "poi.csv"))
        poi_col_list = ["count_point_id", "year", "poi"]
        poi_cleaned = poi.filter(regex='|'.join(poi_col_list)).dropna(how='all')
        poi_long = pd.wide_to_long(poi_cleaned,
            ["poi"], 
            i="count_point_id", 
            j="year",
            sep='_').reset_index()
        slope_col_list = ["count_point_id", "year", "slope"]
        slope = (pd.read_csv(os.path.join(self.root_dir, "data/processed/cities", self.city, "slope.csv")).
            filter(regex='|'.join(slope_col_list)).dropna(how='all'))
        
        # join count data and gsv_seg
        merged_df  = (gsv_seg.merge(count_data, on = ["count_point_id", "year"], how = "left").
            merge(gsv_det, on = ["count_point_id", "year"], how = "left").
            merge(control_variables_long, on = ["count_point_id", "year"], how = "left").
            merge(poi_long, on = ["count_point_id", "year"], how = "left").
            merge(slope, on = ["count_point_id"], how = "left").
            dropna(subset=["pedal_cycles"]))
        # create period
        def make_periods(year):
            if 2008 <= year <= 2012:
                return 1
            elif 2013 <= year <= 2017:
                return 2 
            elif 2018 <= year <= 2020:
                return 3
        merged_df["period"] = merged_df["year"].map(lambda year: make_periods(year))
        
        # save as csv
        merged_df.to_csv(os.path.join(self.output_folder, "all_var_joined.csv"), index = False)
        
        
if __name__ == "__main__":
    root_dir = "/Volumes/ExFAT/bike_svi"
    if not os.path.exists(root_dir):
        root_dir = r'E:Koichi\exfat\bike_svi\data'
    city_list = []
    with open(os.path.join(root_dir,"data/external/city_list.txt"), "r") as f:
        for line in f:
            city_list.append(line.strip())
    for city in city_list:
        create_features = CreateFeatures(root_dir, city)
        # create_features.target_var()
        # create_features.control_var()
        # create_features.gis_var()
        # create_features.segmentation(segment = False)
        # create_features.detection()
        # create_features.control_var()
        create_features.join_data()