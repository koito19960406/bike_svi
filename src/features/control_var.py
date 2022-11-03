import os
import pandas as pd
import geopandas as gpd
import numpy as np
import re

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
        def load_population(file_path, year):
            print("-"*10, f"Loading population in {year}", "-"*10)
            # load clean data if there is
            try:
                population = pd.read_csv(os.path.join(os.path.dirname(file_path), f"population_{str(year)}.csv"))
                
            except FileNotFoundError:
                if 2012 <= year <= 2020:
                    # load population and cut off empty margin
                    population = pd.read_excel(file_path, sheet_name=f"Mid-{str(year)} Persons", header = None).iloc[4:,:]
                    # set column 'names
                    population.columns = population.iloc[0,:]
                    # remove the 1st row
                    population = population.iloc[1:,:]
                elif year == 2011:
                    # load population and cut off empty margin
                    population = pd.read_excel(file_path, sheet_name=f"Mid-{str(year)} Persons", header = None).iloc[3:,:]
                    # set column 'names
                    population.columns = population.iloc[0,:]
                    # remove the 1st row
                    population = population.iloc[1:,:]
                else:
                    population = pd.read_excel(file_path, sheet_name=f"Mid-{str(year)}")
                        
                # from here group ages by 10 years
                # and it'll get msessy because I'll use if conditions for each year
                if year==2020:
                    drop_col_list = ["OA11CD"]
                elif year == 2019:
                    drop_col_list = ["LSOA Name", "LA Code (2019 boundaries)", "LA name (2019 boundaries)", "LA Code (2020 boundaries)", "LA name (2020 boundaries)"]
                elif 2018 == year:
                    drop_col_list = ["LA (2019 boundaries)","LSOA"]
                elif 2011 <= year <= 2017:
                    drop_col_list = population.iloc[:,1:3]
                else: 
                    # for year 2008-2010
                    drop_col_list = ["LAD11CD", "LAD11NM"]
                    
                population = population.drop(drop_col_list, axis =1)
                # rename column "All Ages" to "all_ages"
                population.columns = population.columns.map(str)
                population.columns = population.columns.str.replace("All Ages", "all_ages")
                age_counter = 0
                while age_counter <= 80:
                    if year != 2011:
                        # sum up 10 columns at a time
                        population[f"{str(age_counter)} - {str(age_counter+9)}"] = population.iloc[:,2:12].sum(axis=1) / population["all_ages"]
                        # drop columns
                        population.drop(population.iloc[:,2:12], inplace=True, axis=1)
                    else:
                        population[f"{str(age_counter)} - {str(age_counter+9)}"] = population.iloc[:,2:4].sum(axis=1) / population["all_ages"]
                        # drop columns
                        population.drop(population.iloc[:,2:4], inplace=True, axis=1)
                    # update age_counter
                    age_counter+=10
                    
                    if age_counter == 90:
                        population.rename(columns={population.columns[0]: "lsoa_code", population.columns[2]: "90+"}, inplace=True)
                        population["90+"] = population["90+"] / population["all_ages"]
                # add prefix
                population.columns = [str(col) + f'_{str(year)}' if i != 0 else col for i, col in enumerate(population.columns)]
                # save to a csv file
                population.to_csv(os.path.join(os.path.dirname(file_path), f"population_{str(year)}.csv"), index = False)
        
        load_population(os.path.join(self.input_folder, "control_variables/population/sape23dt10amid2020coaunformattedsyoaestimateslondon.xlsx"), 2020)
        load_population(os.path.join(self.input_folder, "control_variables/population/SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx"), 2019)
        load_population(os.path.join(self.input_folder, "control_variables/population/SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx"), 2018)
        load_population(os.path.join(self.input_folder, "control_variables/population/SAPE20DT1-mid-2017-lsoa-syoa-estimates-formatted.XLS"), 2017)
        load_population(os.path.join(self.input_folder, "control_variables/population/SAPE20DT1-mid-2016-lsoa-syoa-estimates-formatted.xls"), 2016)
        load_population(os.path.join(self.input_folder, "control_variables/population/SAPE20DT1-mid-2015-lsoa-syoa-estimates-formatted.xls"), 2015)
        load_population(os.path.join(self.input_folder, "control_variables/population/SAPE20DT1-mid-2014-lsoa-syoa-estimates-formatted.xls"), 2014)
        load_population(os.path.join(self.input_folder, "control_variables/population/SAPE20DT1-mid-2013-lsoa-syoa-estimates-formatted.xls"), 2013)
        load_population(os.path.join(self.input_folder, "control_variables/population/SAPE20DT1-mid-2012-lsoa-syoa-estimates-formatted.xls"), 2012)
        load_population(os.path.join(self.input_folder, "control_variables/population/mid-2011-lsoa-quinary-estimates.xls"), 2011)
        load_population(os.path.join(self.input_folder, "control_variables/population/SAPE8DT1b-LSOA-syoa-unformatted-persons-mid2007-to-mid2010.xls"), 2010)
        load_population(os.path.join(self.input_folder, "control_variables/population/SAPE8DT1b-LSOA-syoa-unformatted-persons-mid2007-to-mid2010.xls"), 2009)
        load_population(os.path.join(self.input_folder, "control_variables/population/SAPE8DT1b-LSOA-syoa-unformatted-persons-mid2007-to-mid2010.xls"), 2008)
        
        # merge them into one variable
        if not os.path.exists(os.path.join(self.input_folder, "control_variables/population", "population.csv")):
            for year in range(2008,2021):
                population_temp = pd.read_csv(os.path.join(self.input_folder, "control_variables/population", f"population_{str(year)}.csv"))
                if year == 2008:
                    population = population_temp
                    continue
                population = pd.merge(population, population_temp, on = "lsoa_code")
            # save the merged result as csv
            population.to_csv(os.path.join(self.input_folder, "control_variables/population", "population.csv"))
        self.population = pd.read_csv(os.path.join(self.input_folder, "control_variables/population", "population.csv"))
        
        # load deprivation data
        def load_deprivation(file_path, year, columns):
            deprivation = pd.read_excel(file_path, sheet_name=f"IMD {str(year)}").iloc[:,columns]
            deprivation.rename(columns={deprivation.columns[0]: "lsoa_code", deprivation.columns[1]: f"IMD_score_{str(year)}"}, inplace=True)
            return deprivation
        
        self.deprivation_2019 = load_deprivation(os.path.join(self.input_folder, "control_variables/deprivation/ID 2019 for London.xlsx"), 2019, [0,4])
        self.deprivation_2015 = load_deprivation(os.path.join(self.input_folder, "control_variables/deprivation/ID 2015 for London.xls"), 2015, [0,4])
        self.deprivation_2010 = load_deprivation(os.path.join(self.input_folder, "control_variables/deprivation/id-2010-for-london.xls"), 2010, [0,7])
        
        # load housing price
        def load_housing(file_path):
            housing_df = pd.read_excel(file_path, sheet_name="Data", header = None).iloc[5:,:]
            # set column 'names
            housing_df.columns = housing_df.iloc[0,:]
            # remove the 1st row
            housing_df = housing_df.iloc[1:,:]
            # wide to long format
            housing_df = housing_df.dropna(axis=1).reset_index()
            housing_df.columns = ["housing_price" + re.search(r'[0-9]{4}', col).group() if re.search(r'[0-9]{4}', col) is not None else col for col in housing_df.columns]
            housing_df = pd.wide_to_long(housing_df, stubnames=["housing_price"], i="LSOA code", sep = "", j="year")
            housing_df["housing_price"] = housing_df["housing_price"].replace(":",pd.NA)
            housing_df = housing_df.groupby(["LSOA code", "year"])["housing_price"].agg("mean").reset_index()
            housing_df = housing_df.rename(columns={"LSOA code": "lsoa_code"})
            # back to wide format
            housing_df = pd.pivot(housing_df, index="lsoa_code",columns="year",values="housing_price")
            housing_df.columns = ["housing_price_" + re.search(r'[0-9]{4}', str(col)).group() if re.search(r'[0-9]{4}', str(col)) is not None else col for col in housing_df.columns]
            print(housing_df)
            return housing_df
        
        self.housing_df = load_housing(os.path.join(self.input_folder, "control_variables/housing/hpssadataset46medianpricepaidforresidentialpropertiesbylsoa.xls"))
        
        
        # load gdf 
        def load_lsoa(file_path):
            lsoa = gpd.read_file(file_path).to_crs("EPSG:3857")
            # filter columns
            lsoa = lsoa.iloc[:,[0,-1]]
            # calculate area
            lsoa["area"] = lsoa.geometry.area
            lsoa = lsoa.to_crs("EPSG:4326")
            lsoa.rename(columns={lsoa.columns[0]:"lsoa_code"}, inplace=True)
            return lsoa

        self.lsoa_gdf_2011 = load_lsoa(os.path.join(self.input_folder, "control_variables/statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp"))
        self.lsoa_gdf_2004 = load_lsoa(os.path.join(self.input_folder, "control_variables/statistical-gis-boundaries-london/ESRI/LSOA_2004_London_Low_Resolution.shp")) 
        
    def convert_to_spatial(self):
        """convert pandas df to geopandas gdf by joining on lsoa code"""
        #TODO modify key column names and filter to get necessary columns only
        def merge_gdf(variables_1, variable_2):
            print("-"*10, "converting to spatial variables", "-"*10)
            merged = pd.merge(variables_1, variable_2, on = "lsoa_code", how = "left").drop(['lsoa_code'], axis=1)
            # calculate population density per km2
            population_cols = [col for col in merged.columns if "all_ages" in col]
            if len(population_cols) > 0:
                for population_col in population_cols:
                    year = population_col[-4:]
                    merged[f"pop_den_{year}"] = (merged[population_col]/merged["area"])*1000000
            return merged
        self.population_gdf = merge_gdf(self.lsoa_gdf_2011, self.population)
        self.deprivation_2019_gdf = merge_gdf(self.lsoa_gdf_2011, self.deprivation_2019)
        self.deprivation_2015_gdf = merge_gdf(self.lsoa_gdf_2011, self.deprivation_2015)
        self.deprivation_2010_gdf = merge_gdf(self.lsoa_gdf_2004, self.deprivation_2010)
        self.housing_gdf = merge_gdf(self.lsoa_gdf_2011, self.housing_df)
    
    def spatial_join(self):
        """join attributes from census data to count station by spatially left joining them
        """
        def spatial_join_clen(variables_1, variable_2):
            print("-"*10, "spatial join", "-"*10)
            gdf = gpd.sjoin(variables_1, variable_2, how = "left")
            gdf = gdf.drop(['index_right'], axis=1)
            return gdf
        # spatial join
        self.count_joined_gdf = spatial_join_clen(self.count_station_gdf, self.population_gdf)
        self.count_joined_gdf = spatial_join_clen(self.count_joined_gdf, self.deprivation_2019_gdf)
        self.count_joined_gdf = spatial_join_clen(self.count_joined_gdf, self.deprivation_2015_gdf)
        self.count_joined_gdf = spatial_join_clen(self.count_joined_gdf, self.deprivation_2010_gdf)
        self.count_joined_gdf = spatial_join_clen(self.count_joined_gdf, self.housing_gdf)
    
    def merge_save(self):
        """merge the variables and save as csv file
        """
        print("-"*10, "Saving the result", "-"*10)
        # convert to pandas df
        count_joined_df  = pd.DataFrame(self.count_joined_gdf.drop(columns="geometry"))
        
        # save as csv files
        count_joined_df.to_csv(os.path.join(self.output_folder,"count_control_variables.csv"), index = False)
    
    def main(self):
        self.convert_to_spatial()
        self.spatial_join()
        self.merge_save()
        pass
    

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
        create_features = ControlVariables(input_folder, output_folder)
        create_features.main()