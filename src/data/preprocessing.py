import pandas as pd
import numpy as np
import geopandas as gpd
import os
import streetview
from multiprocessing.pool import ThreadPool
from geopy import distance
from get_img import my_task
from perspective.tool import Equirectangular
import cv2
import threading

class DataProcessor:
    """
    Class for reading, processing, and writing data from the UCI
    `Condition monitoring of hydraulic systems` dataset.
    """
    def __init__(self, input_folder, output_folder, city_name):
        # set and create directories
        self.input_folder = input_folder
        self.output_folder = output_folder
        self.city_name = city_name
        self.city_input_folder = os.path.join(input_folder, city_name)
        self.city_output_folder = os.path.join(self.output_folder, self.city_name)
        self.gsv_output_folder = os.path.join(self.city_output_folder,"gsv")
        self.gsv_metadata_output_folder = os.path.join(self.gsv_output_folder,"metadata")
        self.gsv_image_output_folder = os.path.join(self.gsv_output_folder,"image")
        os.makedirs(self.gsv_metadata_output_folder, exist_ok = True)
        os.makedirs(self.gsv_image_output_folder, exist_ok = True)
        if os.path.exists(os.path.join(self.gsv_metadata_output_folder, "gsv_metadata.csv")):
            self.gsv_metadata = pd.read_csv(os.path.join(self.gsv_metadata_output_folder, "gsv_metadata.csv"))
            
        # get number of cpus
        self.cpu_num = os.cpu_count()
        pass

    def read_data(self):
        """Read raw data into DataProcessor."""
        self.count_station = pd.read_csv(os.path.join(self.city_input_folder,"count_station.csv"))
        self.count_station_gdf = gpd.GeoDataFrame(self.count_station, 
                                    geometry=gpd.points_from_xy(self.count_station.longitude, self.count_station.latitude),
                                    crs=4326)
        
        self.count_data = pd.read_csv(os.path.join(self.city_input_folder,"count_data.csv"))
        
    def get_gsv_metadata_multiprocessing(self, point_gdf = None, update = False):
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
                panoids["count_point_id"] = row["count_point_id"]
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
            num_processes = self.cpu_num
            pool = ThreadPool(processes=num_processes)
            input_df_split = np.array_split(input_df, num_processes)
            output_df = pd.concat(pool.map(outer_func, input_df_split), ignore_index = True)
            return output_df
        
        # run the parallelized functions if the metadata doesn't exist yet
        if not update and os.path.exists(os.path.join(self.gsv_metadata_output_folder, "gsv_metadata.csv")):
            print("The output file already exists, please set update to True if you want to update it")
        else:
            df_output = parallelize_dataframe(point_gdf, parallelize_function)
        
            # save df_output
            df_output.to_csv(os.path.join(self.gsv_metadata_output_folder, "gsv_metadata.csv"), index = False)
            
            # store as property
            self.gsv_metadata = df_output
        
    
    # calculate the distance from the original input location
    def calc_dist(self, update = False):
        if not update and os.path.join(self.gsv_metadata_output_folder, "gsv_metadata_dist.csv"):
            print("The output file already exists, please set update to True if you want to update it")
        else:
            # assign gsv_metadata to gsv_metadata
            gsv_metadata = self.gsv_metadata
            # define a function that takes two sets of lat and lon and return distance
            def calc_dist_row(row):
                dist = distance.distance((row["lat"],row["lon"]), (row["input_lat"],row["input_lon"])).meters
                return dist
            
            gsv_metadata["distance"] = gsv_metadata.apply(lambda row: calc_dist_row(row), axis=1)
            
            # save df_output
            gsv_metadata.to_csv(os.path.join(self.gsv_metadata_output_folder, "gsv_metadata_dist.csv"), index = False)
        
    def download_gsv(self):
        # create output folders
        dir_save = os.path.join(self.gsv_image_output_folder,"panorama")
        os.makedirs(dir_save, exist_ok = True)
        # set path to the pid csv file
        path_pid = os.path.join(self.gsv_metadata_output_folder,"gsv_metadata.csv")
        # set path to user agent info csv file
        ua_path = "src/data/get_img/utils/UserAgent.csv"
        # set path to the 1st error log csv file
        log_path = os.path.join(self.gsv_metadata_output_folder,"gsv_metadata_error_1.csv")
        # Number of threads: num of cpus
        nthreads = self.cpu_num
        # set user agent to avoid gettting banned
        UA = my_task.get_ua(path=ua_path)
        # run the main function to download gsv as the 1st try
        my_task.main(UA, path_pid, dir_save, log_path, nthreads)
        # some good pids will be missed when 1 bad pid is found in multithreading
        # so run again the main function with error log file and only 1 thread
        log_path_2 = os.path.join(self.gsv_metadata_output_folder,"gsv_metadata_error_2.csv")
        my_task.main(UA, log_path, dir_save, log_path_2, 1)
            
            
    def transform_pano_to_perspective(self):
        # define function to run in the threading
        def run(path_input_raw, path_output_c,show_size):
            # get perspective at each 90 degree
            thetas = [0, 90, 180, 270]
            FOV = 90

            # set aspect as 9 to 16
            aspects_v = (2.25, 4)
            aspects = (9, 16)

            img_raw = cv2.imread(path_input_raw, cv2.IMREAD_COLOR)
            equ_raw = Equirectangular(img_raw)

            for theta in thetas:
                height = int(aspects_v[0] * show_size)
                width = int(aspects_v[1] * show_size)
                aspect_name = '%s--%s'%(aspects[0], aspects[1])
                img_raw = equ_raw.GetPerspective(FOV, theta, 0, height, width)
                path_output = path_output_c[:]
                path_output_raw = path_output.replace('.png', '_Direction_%s_FOV_%s_aspect_%s_raw.png'%(theta, FOV, aspect_name))
                cv2.imwrite(path_output_raw, img_raw)
        
        # set and create directories       
        dir_input_raw = os.path.join(self.gsv_image_output_folder,"panorama")
        dir_out_show = os.path.join(self.gsv_image_output_folder,"perspective")
        os.makedirs(dir_out_show, exist_ok = True)

        # set parameters
        index = 0
        show_size = 100  # 像素大小 * 4 或者 3
        threads = []
        num_thread = self.cpu_num

        for name in os.listdir(dir_input_raw):
            index += 1

            path_input_raw = os.path.join(dir_input_raw, name)
            path_output = os.path.join(dir_out_show, name.replace('jpg','png'))

            if index % num_thread == 0:
                print('Now:', index)
                t = threading.Thread(target=run, args=(path_input_raw, path_output, show_size,))
                threads.append(t)
                for t in threads:
                    t.setDaemon(True)
                    t.start()
                t.join()
                threads = []
            else:
                t = threading.Thread(target=run, args=(path_input_raw, path_output, show_size,))
                threads.append(t)