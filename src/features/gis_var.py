import os
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import richdem as rd
import geopandas as gpd
from gis_utils import zonal_stats 
import pandas as pd
from ohsome import OhsomeClient
import rasterio
import gemgis as gg
from tqdm import tqdm
from multiprocessing.pool import ThreadPool
import glob 

class GISVariables:
    """
    class to get gis variables
    """
    def __init__(self, input_folder, output_folder):
        # set folders
        self.input_folder = input_folder
        self.output_folder = output_folder
        
        # import count station and convert to projected crs
        self.count_station = pd.read_csv(os.path.join(self.input_folder,"count_station.csv"))
        self.count_station_gdf = gpd.GeoDataFrame(self.count_station, 
                                    geometry=gpd.points_from_xy(self.count_station.longitude, self.count_station.latitude),
                                    crs=4326).to_crs(3857)
        
        
    def calculate_slope(self):
        """function to calculate slope from dem data
        """
        print("-"*10, f"Calculating slope", "-"*10)
        dem_array = rasterio.open(os.path.join(self.input_folder, "gis_variables/slope/LIDAR_10m_DTM_Composite_2019/LIDAR_10m_DTM_Composite.tif"))
        print(dem_array)
        slope = gg.raster.calculate_slope(dem_array)
        with rasterio.open(
            os.path.join(self.input_folder, "gis_variables/slope/slope.tif"),
            'w',
            driver='GTiff',
            height=slope.shape[0],
            width=slope.shape[1],
            count=1,
            dtype=slope.dtype,
            crs='+proj=latlong'
        ) as dst:
            dst.write(slope, 1)
    
    def extract_slope(self, buffer = 100):
        """function to extract slope at each count station
        """
        def create_buffer(point_shp, buffer):
            """function to create a specified buffer from count station and save as shp
            """
            print("-"*10, f"Creating buffer", "-"*10)
            # get buffer
            buffer_shp = point_shp.buffer(buffer)
            # save
            buffer_shp.to_file(os.path.join(self.input_folder,f"gis_variables/slope/count_station_buffer_{buffer}m.shp"))
            
        def ZonalStats(shape, raster, stats):
            """function to calculate zonal stats

            Args:
                shape (str): shapefile path
                raster (str): raster path
                stats (str): stats as list, f.e. 'min mean max' ; 'min'

            Returns:
                final_gdf: GeoDataFrame
            """
            print("-"*10, f"Calculating zonal stats", "-"*10)
            zonalSt = zonal_stats.zonal_stats(shape, raster, stats = stats)
            df = pd.DataFrame(zonalSt)
            return df
        
        # create buffer from count stations
        create_buffer(self.count_station_gdf, buffer)
        new_df = ZonalStats(os.path.join(self.input_folder,f"gis_variables/slope/count_station_buffer_{buffer}m.shp"), 
                            os.path.join(self.input_folder, "gis_variables/slope/slope.tif"),
                            'mean')
        # save the result as csv
        new_df.to_csv(os.path.join(self.output_folder,"slope.csv"))
    
    def retrieve_poi(self, buffer = 500):
        """ get number of POI within the specified buffer from each count station
        """
        client = OhsomeClient()
        print("Ohsome client has been initialized")
        print(f"Earliest date available is {client.start_timestamp} and lastest is {client.end_timestamp}")
        tqdm.pandas()
        
        def fetch_poi_df(df, buffer, client, index, output_folder):
            # loop through a range of years to get numbers of POI around count stations:
            # define the wrapper function
            def fetch_func(df, buffer, client):
                for year in range(2008,2021):
                    try:
                        response = client.elements.count.post(
                            bcircles = [df.longitude,df.latitude, buffer],
                            time=f"{str(year)}-01-01",
                            filter="amenity=* and type:node")
                    except Exception as e: 
                        print(e)
                        # reconnect to client
                        client = OhsomeClient()
                        print("reconnected to client!")
                        response = client.elements.count.post(
                            bcircles = [df.longitude,df.latitude, buffer],
                            time=f"{str(year)}-01-01",
                            filter="amenity=* and type:node")
                    response_df = response.as_dataframe().reset_index()
                    poi_num = response_df["value"][0]
                    df[f"poi_{str(year)}"] = poi_num
                return pd.DataFrame(df).transpose()
            # read csv
            try:
                poi_df = pd.read_csv(os.path.join(output_folder, f"poi_{str(index)}.csv"))
            except FileNotFoundError:
                poi_df = pd.DataFrame()
            # check if poi_df exists or not
            if len(poi_df.index)>0:
                if df["count_point_id"] in set(poi_df["count_point_id"]):
                    return
                else:
                    df = fetch_func(df, buffer, client)
            else:
                df = fetch_func(df, buffer, client)
            # save the result
            poi_df = pd.concat([poi_df,df], ignore_index = True)
            poi_df.to_csv(os.path.join(output_folder, f"poi_{str(index)}.csv"), index = False)
        
        # apply the function to each row
        def parallelize_function(data):
            index, input_df = data
            input_df.progress_apply(fetch_poi_df, args =(buffer,client,index,self.output_folder), axis=1)
            return
        
        # split the input df and map the input function
        def parallelize_dataframe(input_df, outer_func):
            num_processes = os.cpu_count()
            pool = ThreadPool(processes=num_processes)
            input_df_split = np.array_split(input_df, num_processes)
            index_list = [index for index in range(num_processes)]
            data = [(index,df) for (index,df) in zip(index_list, input_df_split)]
            pool.map(outer_func, data)
            return 
        # run the function
        parallelize_dataframe(self.count_station, parallelize_function)
        
        # join them
        # list of merged files returned
        files = glob.glob(os.path.join(self.output_folder, "poi_*.csv"))
        # joining files with concat and read_csv
        df = pd.concat(map(pd.read_csv, files), ignore_index=True)
        df.to_csv(os.path.join(self.output_folder, "poi.csv"), index = False)
        
if __name__ == "__main__":
    root_dir = "/Volumes/ExFAT/bike_svi/data"
    if not os.path.exists(root_dir):
        root_dir = r'E:Koichi\exfat\bike_svi\data'
    city_list = []
    with open(os.path.join(root_dir,"external/city_list.txt"), "r") as f:
        for line in f:
            city_list.append(line.strip())
    for city in city_list:
        input_folder = os.path.join(root_dir, "external/cities", city)
        output_folder = os.path.join(root_dir, "processed/cities", city)
        gis_variables = GISVariables(input_folder, output_folder)
        gis_variables.retrieve_poi()
        # gis_variables.main()