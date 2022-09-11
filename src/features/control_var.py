import os
import pandas as pd
import geopandas as gpd


class ControlVariables:
    """class to load, clean, and produce control variables
    """
    def __init__(self, input_folder, output_folder):
        # set folders
        self.input_folder = input_folder
        self.output_folder = output_folder
        
        # load datasets
        self.count_station = pd.read_csv(os.path.join(self.input_folder,"count_station.csv"))
        self.count_station_gdf = gpd.GeoDataFrame(self.count_station, 
                                    geometry=gpd.points_from_xy(self.count_station.longitude, self.count_station.latitude),
                                    crs=4326)
        #TODO modify file path and worksheet
        self.population = pd.read_excel(os.path.join(self.input_folder, "population.xlsx"), sheet=1)
        self.deprivation = pd.read_excel(os.path.join(self.input_folder, "deprivation.xlsx"), sheet=1)
        self.lsoa_gdf = gpd.read_file(os.path.join(self.input_folder, "lsoa.shp")).to_crs("EPSG:4326")
        
    def convert_to_spatial(self):
        """convert pandas df to geopandas gdf by joining on lsoa code"""
        #TODO modify key column names and filter to get necessary columns only
        self.population_gdf = pd.merge(self.lsoa_gdf, self.population, left_on = "KEY", right_on = "KEY", how = "left")
        self.deprivation_gdf = pd.merge(self.lsoa_gdf, self.deprivation, left_on = "KEY", right_on = "KEY", how = "left")
        
    def spatial_join(self):
        """join attributes from census data to count station by spatially left joining them
        """
        # spatial join
        self.count_population_joineda_gdf = self.count_station_gdf.sjoin(self.population_gdf, how = "left")
        self.count_deprivation_joined_gdf = self.count_station_gdf.sjoin(self.deprivation_gdf, how = "left")
    
    def merge_save(self):
        """merge the variables and save as csv file
        """
        # convert to pandas df
        count_population_joined_df = pd.DataFrame(self.count_population_joineda_gdf.drop(columns="geometry"))
        count_deprivation_joined_df = pd.DataFrame(self.count_deprivation_joined_gdf.drop(columns="geometry"))
        # merge them
        #TODO modify the key
        count_joined_df = pd.merge(count_population_joined_df, count_deprivation_joined_df, on = "KEY", how = "outer")
        # save as csv files
        count_joined_df.to_csv(os.path.join(self.output_folder,"count_control_variables.csv"), index = False)
    
    def main(self):
        self.convert_to_spatial()
        self.spatial_join()
        self.merge_save()
        pass
    
        