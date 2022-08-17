import pandas as pd
import numpy as np
import geopandas as gpd
import os
import streetview
import multiprocessing
from multiprocessing.pool import ThreadPool
# from geopy import distance

class DataProcessor:
    """
    Class for reading, processing, and writing data from the UCI
    `Condition monitoring of hydraulic systems` dataset.
    """
    def __init__(self, input_folder, output_folder):
        self.input_folder = input_folder
        self.output_folder = output_folder
        self.gsv_output_folder = os.path.join(self.output_folder,"gsv")
        self.gsv_metadata_output_folder = os.path.join(self.gsv_output_folder,"metadata")
        self.gsv_image_output_folder = os.path.join(self.gsv_output_folder,"image")
        os.makedirs(self.gsv_metadata_output_folder)
        os.makedirs(self.gsv_image_output_folder)
        pass

    def read_data(self):
        """Read raw data into DataProcessor."""
        self.count_station = pd.read_csv(os.path.join(self.input_folder,"dft_countpoints_region_id_6.csv"))
        self.count_station_gdf = gpd.GeoDataFrame(self.count_station, 
                                    geometry=gpd.points_from_xy(self.count_station.longitude, self.count_station.latitude),
                                    crs=4326)
        
        self.count_data = pd.read_csv(os.path.join(self.input_folder,"dft_aadf_region_id_6.csv"))
        
    def get_gsv_metadata_multiprocessing(self, point_gdf = None):
        """get GSV metadata (e.g., panoids, years, months, etc) for each location and store the result as self.panoids
            Args:
                point_gdf (geodataframe): point locations to retrieve GSV metadata. If None, then use count_station_gdf
        """
        global get_gsv_metadata
        global parallelize_function
        
        # set point_gdf
        if point_gdf == None:
            point_gdf = self.count_station_gdf
        
        # define a function to retrieve GSV metadata based on each row of the input GeoDataFrame
        def get_gsv_metadata(row):
            try:
                panoids = streetview.panoids(lat=row.geometry.y, lon=row.geometry.x)
                panoids = pd.DataFrame.from_dict(panoids)
                # query to get years available
                panoids = panoids.dropna(subset=["year"])
                panoids["input_lat"] = row.geometry.y
                panoids["input_lon"] = row.geometry.x
                return panoids
            except:
                print(row.geometry.y,row.geometry.x)
                print(panoids)
                return
            
        # apply the function to each row
        def parallelize_function(input_df):
            output_df = pd.DataFrame()
            for _, row in input_df.iterrows():
                output_df_temp = get_gsv_metadata(row)
                output_df = pd.concat([output_df,output_df_temp], ignore_index = True)
            return output_df
        
        # split the input df and map the input function
        def parallelize_dataframe(input_df, outer_func):
            num_processes = multiprocessing.cpu_count()
            pool = ThreadPool(processes=num_processes)
            input_df_split = np.array_split(input_df, num_processes)
            output_df = pd.concat(pool.map(outer_func, input_df_split), ignore_index = True)
            return output_df
        
        # run the parallelized functions
        df_output = parallelize_dataframe(point_gdf, parallelize_function)
        
        # save df_output
        df_output.to_csv(os.path.join(self.output_folder, "gsv_metadata.csv"), index = False)
        
        # store as property
        self.gsv_metadata = df_output
        
    
    # calculate the distance from the original input location
    def calc_dist(self):
        # assign gsv_metadata to gsv_metadata
        gsv_metadata = self.gsv_metadata
        # define a function that takes two sets of lat and lon and return distance
        def calc_dist_row(row):
            distance = distance.distance((row.lat,row.lon), (row.input_lat,row.input_lon)).meters
            return distance
        
        gsv_metadata["distance"] = gsv_metadata.apply(lambda row: calc_dist_row(row))
        
        # save df_output
        gsv_metadata.to_csv(os.path.join(self.output_folder, "gsv_metadata.csv"), index = False)
        
        # update self.gsv_metadata
        self.gsv_metadata = gsv_metadata
        
    
    def process_data(self, stable=True):
        """Process raw data into useful files for model."""
        # do processing things

    def write_data(self, processed_data_path):
        """Write processed data to directory."""
        # do writing things