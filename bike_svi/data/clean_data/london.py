from pathlib import Path
import os
import pandas as pd
from concurrent.futures import ThreadPoolExecutor, as_completed
from tqdm import tqdm
import re
import geopandas as gpd
from tenacity import retry, stop_after_attempt
from ohsome import OhsomeClient
from rasterstats import zonal_stats
import rasterio
import numpy as np

from .base import BaseDataCleaner
from .terrain import dem_to_slope

class LondonDataCleaner(BaseDataCleaner):
    def __init__(self, dir_input, dir_output):
        super().__init__(dir_input, dir_output)
        
    # load lsoa data
    def load_lsoa(self, file_path):
        lsoa = gpd.read_file(file_path).to_crs("EPSG:4326")
        # filter columns
        lsoa = lsoa.iloc[:,[0,-1]]
        lsoa.rename(columns={lsoa.columns[0]:"census_id"}, inplace=True)
        return lsoa
        
    def load_age_data(self):
        if os.path.exists(os.path.join(self.dir_input, "control_variables/age", "age.csv")):
            age = pd.read_csv(os.path.join(self.dir_input, "control_variables/age", "age.csv"))
            return age
        
        def _preclean_age_data(file_path, year):
            # load clean data if there is
            if os.path.exists(os.path.join(os.path.dirname(file_path), f"age_{str(year)}.csv")):
                age = pd.read_csv(os.path.join(os.path.dirname(file_path), f"age_{str(year)}.csv"))
                return age
            else:
                if 2012 <= year <= 2020:
                    # load age and cut off empty margin
                    age = pd.read_excel(file_path, sheet_name=f"Mid-{str(year)} Persons", header = None).iloc[4:,:]
                    # set column 'names
                    age.columns = age.iloc[0,:]
                    # remove the 1st row
                    age = age.iloc[1:,:]
                elif year == 2011:
                    # load age and cut off empty margin
                    age = pd.read_excel(file_path, sheet_name=f"Mid-{str(year)} Persons", header = None).iloc[3:,:]
                    # set column 'names
                    age.columns = age.iloc[0,:]
                    # remove the 1st row
                    age = age.iloc[1:,:]
                else:
                    age = pd.read_excel(file_path, sheet_name=f"Mid-{str(year)}")
                # group ages by 10 years
                if year==2020:
                    drop_col_list = ["OA11CD"]
                elif year == 2019:
                    drop_col_list = ["LSOA Name", "LA Code (2019 boundaries)", "LA name (2019 boundaries)", "LA Code (2020 boundaries)", "LA name (2020 boundaries)"]
                elif 2018 == year:
                    drop_col_list = ["LA (2019 boundaries)","LSOA"]
                elif 2011 <= year <= 2017:
                    drop_col_list = age.iloc[:,1:3]
                else: 
                    # for year 2008-2010
                    drop_col_list = ["LAD11CD", "LAD11NM"]

                age = age.drop(drop_col_list, axis =1)
                # rename the first column to "census_id"
                age.rename(columns={age.columns[0]: "census_id"}, inplace=True)
                # aggregate by lsoa
                age = age.groupby(["census_id"]).agg("sum").reset_index()
                # rename column "All Ages" to "all_ages"
                age.columns = age.columns.map(str)
                age.columns = age.columns.str.replace("All Ages", "all_ages")
                age_counter = 0
                while age_counter <= 60:
                    if year != 2011:
                        if age_counter < 60:
                            # sum up 20 columns at a time
                            age[f"age_{str(age_counter)}_{str(age_counter+19)}"] = age.iloc[:,2:22].sum(axis=1) / age["all_ages"] * 100
                            # drop columns
                            age.drop(age.iloc[:,2:22], inplace=True, axis=1)
                        else:
                            # sum from 60 to 90+ (31 columns)
                            age[f"age_{str(age_counter)}_{str(age_counter+30)}"] = age.iloc[:,2:33].sum(axis=1) / age["all_ages"] * 100
                            # drop columns
                            age.drop(age.iloc[:,2:33], inplace=True, axis=1)
                    else:
                        if age_counter < 60:
                            # sum up 4 columns at a time
                            age[f"age_{str(age_counter)}_{str(age_counter+19)}"] = age.iloc[:,2:6].sum(axis=1) / age["all_ages"] * 100
                            # drop columns
                            age.drop(age.iloc[:,2:6], inplace=True, axis=1)
                        else:
                            # sum from 60 to 90+ (7 columns)
                            age[f"age_{str(age_counter)}_{str(age_counter+30)}"] = age.iloc[:,2:9].sum(axis=1) / age["all_ages"] * 100
                            # drop columns
                            age.drop(age.iloc[:,2:9], inplace=True, axis=1)
                    # update age_counter
                    age_counter+=20
                # add year column
                age["year"] = year
                # rename "all_ages" to "total"
                age.rename(columns={"all_ages": "total"}, inplace=True)
                # save to a csv file
                age.to_csv(os.path.join(os.path.dirname(file_path), f"age_{str(year)}.csv"), index = False)
                return age
        
        # List of years and corresponding file paths
        years_and_files = [
            (2020, "control_variables/age/sape23dt10amid2020coaunformattedsyoaestimateslondon.xlsx"),
            (2019, "control_variables/age/SAPE22DT2-mid-2019-lsoa-syoa-estimates-unformatted.xlsx"),
            (2018, "control_variables/age/SAPE21DT1a-mid-2018-on-2019-LA-lsoa-syoa-estimates-formatted.xlsx"),
            (2017, "control_variables/age/SAPE20DT1-mid-2017-lsoa-syoa-estimates-formatted.XLS"),
            (2016, "control_variables/age/SAPE20DT1-mid-2016-lsoa-syoa-estimates-formatted.xls"),
            (2015, "control_variables/age/SAPE20DT1-mid-2015-lsoa-syoa-estimates-formatted.xls"),
            (2014, "control_variables/age/SAPE20DT1-mid-2014-lsoa-syoa-estimates-formatted.xls"),
            (2013, "control_variables/age/SAPE20DT1-mid-2013-lsoa-syoa-estimates-formatted.xls"),
            (2012, "control_variables/age/SAPE20DT1-mid-2012-lsoa-syoa-estimates-formatted.xls"),
            (2011, "control_variables/age/mid-2011-lsoa-quinary-estimates.xls"),
            (2010, "control_variables/age/SAPE8DT1b-LSOA-syoa-unformatted-persons-mid2007-to-mid2010.xls"),
            (2009, "control_variables/age/SAPE8DT1b-LSOA-syoa-unformatted-persons-mid2007-to-mid2010.xls"),
            (2008, "control_variables/age/SAPE8DT1b-LSOA-syoa-unformatted-persons-mid2007-to-mid2010.xls")
        ]

        # clean data in parallel
        with ThreadPoolExecutor() as executor:
            futures = {executor.submit(_preclean_age_data, os.path.join(self.dir_input, file), year): (year, file) for year, file in years_and_files}

            age_dataframes = []
            for future in tqdm(as_completed(futures), total=len(futures), desc="Processing age files"):
                result = future.result()
                age_dataframes.append(result)

        # merge them into one variable
        age = pd.concat(age_dataframes, axis=0)
        # save the merged result as csv
        age.to_csv(os.path.join(self.dir_input, "control_variables/age", "age.csv"), index = False)
        age = pd.read_csv(os.path.join(self.dir_input, "control_variables/age", "age.csv"))
        return age
        
    def load_wealth_data(self):
        # load deprivation data
        def _preclean_wealth_data(file_path, year, columns):
            # load clean data if there is
            if os.path.exists(os.path.join(os.path.dirname(file_path), f"deprivation_{str(year)}.csv")):
                deprivation = pd.read_csv(os.path.join(os.path.dirname(file_path), f"deprivation_{str(year)}.csv"))
                return deprivation
            deprivation = pd.read_excel(file_path, sheet_name=f"IMD {str(year)}").iloc[:,columns]
            # add year column
            deprivation["year"] = year
            deprivation.rename(columns={deprivation.columns[0]: "census_id",  deprivation.columns[1]: f"IMD_score"}, inplace=True)
            # save to a csv file
            deprivation.to_csv(os.path.join(os.path.dirname(file_path), f"deprivation_{str(year)}.csv"), index = False)
            return deprivation
        
        # create a list of years and corresponding file paths and columns
        years_and_files = [
            (2019, "control_variables/deprivation/ID 2019 for London.xlsx", [0,4]),
            (2015, "control_variables/deprivation/ID 2015 for London.xls", [0,4]),
            (2010, "control_variables/deprivation/id-2010-for-london.xls", [0,7])
        ]
        
        # clean data in parallel
        with ThreadPoolExecutor() as executor:
            futures = {executor.submit(_preclean_wealth_data, os.path.join(self.dir_input, file), year, columns): (year, file) for year, file, columns in years_and_files}

            deprivation_dataframes = []
            for future in tqdm(as_completed(futures), total=len(futures), desc="Processing deprivation files"):
                result = future.result()
                deprivation_dataframes.append(result)
        
        # merge them into one variable
        deprivation = pd.concat(deprivation_dataframes, axis=0)
        # save the merged result as csv
        deprivation.to_csv(os.path.join(self.dir_input, "control_variables/deprivation", "deprivation.csv"), index = False)
        # load the merged result
        deprivation = pd.read_csv(os.path.join(self.dir_input, "control_variables/deprivation", "deprivation.csv"))
        return deprivation
        
    # load housing price
    def load_housing_data(self):
        # load housing data if it exists
        if os.path.exists(os.path.join(self.dir_input, "control_variables/housing/housing.csv")):
            housing = pd.read_csv(os.path.join(self.dir_input, "control_variables/housing/housing.csv"))
            return housing
        def _preclean_housing_data(file_path):
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
            housing_df = housing_df.rename(columns={"LSOA code": "census_id"})
            return housing_df

        housing = _preclean_housing_data(os.path.join(self.dir_input, "control_variables/housing/hpssadataset46medianpricepaidforresidentialpropertiesbylsoa.xls"))
        # save it as csv
        housing.to_csv(os.path.join(self.dir_input, "control_variables/housing", "housing.csv"), index = False)
        # load the result
        housing = pd.read_csv(os.path.join(self.dir_input, "control_variables/housing", "housing.csv"))
        return housing
    
    def load_land_use_data(self):
        # load land use data if it exists
        if os.path.exists(os.path.join(self.dir_input, "control_variables/land_use/land_use.csv")):
            land_use = pd.read_csv(os.path.join(self.dir_input, "control_variables/land_use/land_use.csv"))
            return land_use
        def _preclean_land_use_data(file_path):
            land_use_df = pd.read_excel(file_path, sheet_name= 0, header = [3,4,5]).iloc[4:,:].\
            dropna(axis=1,how="all").dropna(axis=0,how="all")
            # combine column names
            land_use_df.columns = land_use_df.columns.map(' '.join)
            # clean up: keep those col names with LSOA code and Total and avoid those with Grand Total
            land_use_df = land_use_df.filter(regex='|'.join(["LSOA code","Total"]))
            land_use_df = land_use_df.drop(land_use_df.filter(regex='Grand Total').columns, axis=1)
            # remove uncessary strings like 1_level_1 and Unnamed: 
            land_use_df.columns = [re.sub("[0-9]+_level_[0-9]+","", col).\
                replace("Unnamed: ", "").replace("Total","").replace("  ", " ").\
                replace("Developed use","").replace("Non-developed use","").\
                replace("Non-developed","").strip() for col in land_use_df.columns]
            land_use_df.columns = ["drop" if len(col) == 0 else col for col in land_use_df.columns]
            # drop unnecessary columns
            land_use_df = land_use_df.drop("drop", axis=1)
            # replace space, "and", and "," with "_"
            land_use_df.columns = [col.replace(" and ", "_").\
                replace(", ","_").replace(" ","_").lower() for col in land_use_df.columns]
            # add prefix ("lu_") to column names
            land_use_df.columns = ["lu_" + col if col != "lsoa_code" else col for col in land_use_df.columns]
            # replace - with 0
            land_use_df = land_use_df.replace("-",0)
            # rename lsoa code
            land_use_df.rename(columns={land_use_df.columns[0]:"census_id"}, inplace=True)
            # create new land use columns by aggregating the existing ones
            land_use_df["lu_residential_community"] = land_use_df["lu_community_service"] + land_use_df["lu_residential"] / 100
            land_use_df["lu_commerce_developed"] = land_use_df["lu_industry_commerce"] + land_use_df["lu_transport_utilities"] + land_use_df["lu_unknown_developed_use"] / 100
            land_use_df["lu_others"] = land_use_df["lu_agriculture"] + land_use_df["lu_forest_open_land_water"] +\
                land_use_df["lu_outdoor_recreation"] + land_use_df["lu_residential_gardens"] + land_use_df["lu_defence"] +\
                land_use_df["lu_minerals_landfill"] + land_use_df["lu_undeveloped_land"] + land_use_df["lu_vacant"] / 100
            # drop the original columns
            land_use_df = land_use_df.drop(["lu_community_service", "lu_residential", "lu_industry_commerce", "lu_transport_utilities",\
                "lu_unknown_developed_use", "lu_agriculture", "lu_forest_open_land_water", "lu_outdoor_recreation", "lu_residential_gardens",\
                "lu_defence", "lu_minerals_landfill", "lu_undeveloped_land", "lu_vacant"], axis=1)
            return land_use_df

        land_use = _preclean_land_use_data(os.path.join(self.dir_input, "control_variables/land_use/Live_Tables_-_Land_Use_Stock_2022_-_LSOA.ods"))
        # save it as csv
        land_use.to_csv(os.path.join(self.dir_input, "control_variables/land_use", "land_use.csv"), index = False)
        # load the result
        land_use = pd.read_csv(os.path.join(self.dir_input, "control_variables/land_use", "land_use.csv"))
        return land_use
    
    def load_data(self):
        # load lsoa data
        self.lsoa_gdf_2011 = self.load_lsoa(os.path.join(self.dir_input, "control_variables/statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp"))
        self.lsoa_gdf_2004 = self.load_lsoa(os.path.join(self.dir_input, "control_variables/statistical-gis-boundaries-london/ESRI/LSOA_2004_London_Low_Resolution.shp")) 
        # load age data
        age = self.load_age_data()
        # load deprivation data
        deprivation = self.load_wealth_data()
        # load housing data
        housing = self.load_housing_data()
        # load land use data
        land_use = self.load_land_use_data()
        
        # load slope data
        path_input = str(Path(self.dir_input) / "gis_variables/slope/LIDAR_10m_DTM_Composite_2019/LIDAR_10m_DTM_Composite.tif")
        self.slope_path = str(Path(self.dir_input) / "gis_variables/slope/slope.tif")
        if not Path(self.slope_path).exists():
            dem_to_slope(path_input, self.slope_path)
        
        self.census_boundaries = {
            "lsoa_gdf_2011": {"data": self.lsoa_gdf_2011,
                            "census_id": "census_id"},
            "lsoa_gdf_2004": {"data": self.lsoa_gdf_2004,
                            "census_id": "census_id"}
        }
        
        self.age_data = {
            -1: {"data": age,
                "census_boundaries": "lsoa_gdf_2011"}
        }

        self.wealth_data = {
            2010: {"data": deprivation[deprivation["year"]==2010],
                "census_boundaries": "lsoa_gdf_2004"},
            2015: {"data": deprivation[deprivation["year"]==2015],
                "census_boundaries": "lsoa_gdf_2011"},
            2019: {"data": deprivation[deprivation["year"]==2019],
                "census_boundaries": "lsoa_gdf_2011"}
        }
        
        self.housing_data = {
            -1: {"data": housing, 
                "census_boundaries": "lsoa_gdf_2011"}
        }
        
        self.landuse_data = {
            -1: {"data": land_use,
                    "census_boundaries": "lsoa_gdf_2011"}
        }
        
        self.data_dict = {
            "age": self.age_data,
            "wealth": self.wealth_data,
            "housing": self.housing_data,
            "land_use": self.landuse_data,
            "population_density": self.age_data
        }
    
        pass
    

    def clean_count_station_data(self):
        """
        create count_station: count_point_id, latitude, longitude
        create count_station_year: count_point_id, year, count
        """
        # read count_station.csv
        self.count_station = pd.read_csv(os.path.join(self.dir_input, "count_station.csv"))
        # only keep count_point_id, latitude, longitude
        self.count_station = self.count_station[["count_point_id", "latitude", "longitude"]]
        # read count_data.csv
        self.count_data = pd.read_csv(os.path.join(self.dir_input, "count_data.csv"))
        # only keep count_point_id, year, count
        self.count_data = self.count_data[["count_point_id", "year", "pedal_cycles"]]
        # rename column "pedal_cycles" to "count"
        self.count_data.rename(columns={"pedal_cycles": "count"}, inplace=True)
        # aggregate count_data by count_point_id and year
        self.count_data = self.count_data.groupby(["count_point_id", "year"]).agg({"count": "mean"}).reset_index()
        # save df.count_station: count_point_id, latitude, longitude
        self.count_station[["count_point_id", "latitude", "longitude"]].to_csv(Path(self.dir_input) / "count_station_clean.csv", index=False)
        # save df.count_station_year: count_point_id, year, count
        self.count_data[["count_point_id", "year", "count"]].to_csv(Path(self.dir_output) / "count_station_year.csv", index=False)
        # join count_station_year with count_station
        self.count_station_year = self.count_data.merge(self.count_station[["count_point_id", "latitude", "longitude"]], on="count_point_id", how="left")
        # convert count_station_year to geodataframe
        self.count_station_year = gpd.GeoDataFrame(self.count_station_year, geometry=gpd.points_from_xy(self.count_station_year.longitude, self.count_station_year.latitude)).\
            set_crs(epsg=4326)
        self.count_station_year_min = min(self.count_station_year["year"])
        self.count_station_year_max = max(self.count_station_year["year"])
        pass
    

    def join_count_station_with_census(self, data_name):
        data = self.data_dict[data_name]
        # join age data with census boundaries and join with count station
        count_census_list = []
        for year in tqdm(data.keys(), desc="Joining count station with census"):
            # extract census boundaries and convert census_boundaries["census_id"] to object 
            census_boundaries = self.census_boundaries[data[year]["census_boundaries"]]
            census_boundaries["data"][census_boundaries["census_id"]] = census_boundaries["data"][census_boundaries["census_id"]].astype(str)
            # extract census data and convert census_data["census_id"] to object
            census_data = data[year]["data"]
            census_data["census_id"] = census_data["census_id"].astype(str)

            # left join and drop empty geometries
            census_data_gdf_year = pd.merge(census_data, 
                                        census_boundaries["data"][[census_boundaries["census_id"], "geometry"]], 
                                        how="left", left_on="census_id", right_on=census_boundaries["census_id"]).\
                                        dropna(subset=["geometry"])

            # convert to geo dataframe
            census_data_gdf_year = gpd.GeoDataFrame(census_data_gdf_year, geometry=census_data_gdf_year.geometry).set_crs(census_boundaries["data"].crs)
            # spatial join count_station with census
            # reproject both to UTM by estimating the UTM zone
            utm_crs = self.count_station_year.estimate_utm_crs()
            count_station_year_utm = self.count_station_year.to_crs(utm_crs)
            census_data_gdf_year_utm = census_data_gdf_year.to_crs(utm_crs)
            # calculate area of census boundaries if data_name contains "density"
            if "density" in data_name:
                census_data_gdf_year_utm["area"] = census_data_gdf_year_utm["geometry"].area / 10**6
            # get a list of unique "year" in census_data_gdf_year_utm's "year" column
            if "year" in census_data_gdf_year_utm.columns:
                unique_years = list(set(count_station_year_utm["year"]))
                # loop through each year and join count_station_year_utm with census_data_gdf_year_utm
                for unique_year in unique_years:
                    count_census_year = gpd.sjoin_nearest(count_station_year_utm[count_station_year_utm["year"] == unique_year], census_data_gdf_year_utm[census_data_gdf_year_utm["year"] == unique_year], 
                                                        how="left", max_distance = 10000, 
                                                        lsuffix = 'count', rsuffix = 'census').reset_index()
                    # drop columns that are not needed
                    count_census_year = count_census_year.drop(columns=["count", "latitude", "longitude", "index_census", "geometry", "census_id", census_boundaries["census_id"]])
                    count_census_list.append(count_census_year)
            else:
                count_census_year = gpd.sjoin_nearest(count_station_year_utm, census_data_gdf_year_utm, 
                                                        how="left", max_distance = 10000, 
                                                        lsuffix = 'count', rsuffix = 'census').reset_index()
                # drop columns that are not needed
                count_census_year = count_census_year.drop(columns=["count", "latitude", "longitude", "index_census", "geometry", "census_id", census_boundaries["census_id"]])
                count_census_list.append(count_census_year)

        # merge all census data
        count_census = pd.concat(count_census_list)

        if "year_count" in count_census.columns:
            # interpolate the missing years for each unique "count_point_id"
            interpolated_dataframes = []
            for count_point_id in tqdm(count_census["count_point_id"].unique(), desc="Interpolating missing years"):
                df_subset = count_census[count_census["count_point_id"] == count_point_id]
                # skip if there is no non-NA values in year_census
                if df_subset["year_census"].isna().all():
                    continue
                missing_years = list(set(df_subset["year_count"].unique()) - set(data.keys()))
                if len(missing_years) > 0:
                    interpolated_df = self.interpolate_missing_years(df_subset, missing_years)
                    interpolated_dataframes.append(interpolated_df)
            count_census = pd.concat([count_census] + interpolated_dataframes)
            # only keep rows where "year_count" and "year_census" are the same
            # make sure "year_count" and "year_census" are the same type (int)
            # drop NaN values in "year_census" and "year_count"
            count_census = count_census.dropna(subset=["year_census", "year_count"])
            count_census["year_count"] = count_census["year_count"].astype(int)
            count_census["year_census"] = count_census["year_census"].astype(int)
            count_census = count_census[count_census["year_count"] == count_census["year_census"]]
            # rename "year_count" to "year"
            count_census = count_census.rename(columns={"year_count": "year"})
            count_census = count_census.drop(columns=["index", "year_census"])

            # drop duplicated rows
            count_census = count_census.drop_duplicates(subset=["count_point_id", "year"])
            
        return count_census


    def interpolate_missing_years(self, df, missing_years):
        df = df.copy()
        # drop NA
        df = df.dropna(subset=["year_census"])
        df["year_census"] = df["year_census"].astype(int).astype(str)
        df.sort_values("year_census", inplace=True)
        years = df["year_census"].unique()
        start_year = min(self.count_station_year_min, int(min(years)))
        end_year = max(self.count_station_year_max, int(max(years)))
        years = pd.date_range(start=str(start_year), end=str(end_year), freq='YS')
        df_fill = pd.DataFrame({'year_census': years})
        # extract year from "year" column
        df_fill['year_census'] = df_fill['year_census'].dt.year
        df_fill = df_fill.astype({"year_census": 'str'})

        # Only keep the years in df_fill that are not in df's 'year_census'
        df_fill = df_fill[~df_fill['year_census'].isin(df['year_census'])]
        df = pd.concat([df_fill, df])

        # make sure to fill "count_point_id" with the same value
        df["count_point_id"] = df["count_point_id"].fillna(method='bfill')
        
        df.sort_values("year_census", inplace=True)
        df = df.interpolate(method='linear', limit_direction='both', axis=0)
        
        # get data for missing_years from df
        missing_years = [str(year) for year in missing_years]
        return df[df["year_census"].isin(missing_years)]

    
    def clean_age_data(self):
        if (Path(self.dir_output) / "count_age.csv").exists():
            self.count_age = pd.read_csv(Path(self.dir_output) / "count_age.csv")
            return

        # join count_station_year with count_station
        self.count_age = self.join_count_station_with_census("age")
        # drop "total" column
        self.count_age = self.count_age.drop(columns=["total"])
        
        # save to csv
        self.count_age.to_csv(Path(self.dir_output) / "count_age.csv", index=False)


    def clean_population_density_data(self):
        if (Path(self.dir_output) / "count_population_density.csv").exists():
            self.count_population_density = pd.read_csv(Path(self.dir_output) / "count_population_density.csv")
            return
    
        # join count_station_year with count_station
        self.count_population_density = self.join_count_station_with_census("population_density")
        
        # calculate population density
        self.count_population_density["pop_den"] = self.count_population_density["total"] / self.count_population_density["area"]
        self.count_population_density = self.count_population_density[["count_point_id", "year", "pop_den"]]
        
        # save to csv
        self.count_population_density.to_csv(Path(self.dir_output) / "count_population_density.csv", index=False)
        
    def clean_wealth_data(self):
        if (Path(self.dir_output) / "count_deprivation.csv").exists():
            self.count_deprivation = pd.read_csv(Path(self.dir_output) / "count_deprivation.csv")
            return

        # join count_station_year with count_station
        self.count_deprivation = self.join_count_station_with_census("wealth")
        
        # save to csv
        self.count_deprivation.to_csv(Path(self.dir_output) / "count_deprivation.csv", index=False)
        pass
    
    def clean_housing_data(self):
        if (Path(self.dir_output) / "count_housing.csv").exists():
            self.count_housing = pd.read_csv(Path(self.dir_output) / "count_housing.csv")
            return

        # join count_station_year with count_station
        self.count_housing = self.join_count_station_with_census("housing")
        
        # save to csv
        self.count_housing.to_csv(Path(self.dir_output) / "count_housing.csv", index=False)
        pass

    def clean_landuse_data(self):
        if (Path(self.dir_output) / "count_land_use.csv").exists():
            self.count_land_use = pd.read_csv(Path(self.dir_output) / "count_land_use.csv")
            return

        # join count_station_year with count_station
        self.count_land_use = self.join_count_station_with_census("land_use")
        
        # drop "index" column
        self.count_land_use = self.count_land_use.drop(columns=["index"])
        # save to csv
        self.count_land_use.to_csv(Path(self.dir_output) / "count_land_use.csv", index=False)
        pass

    def clean_poi_data(self):
        if (Path(self.dir_output) / "count_poi.csv").exists():
            self.count_poi = pd.read_csv(Path(self.dir_output) / "count_poi.csv")
            return
        
        @retry(stop=stop_after_attempt(3))
        def fetch_poi_data(client, bcircles, year):
            return client.elements.count.post(
                bcircles=bcircles,
                time=f"{str(year)}-01-01",
                filter="amenity=* and type:node"
            )

        def fetch_poi(row, buffer, client):
            result = {}
            try:
                response = fetch_poi_data(client, [row.longitude, row.latitude, buffer], row.year)
                response_df = response.as_dataframe().reset_index()
                poi_num = response_df["value"][0]
                # add count_point_id and year to result
                result["count_point_id"] = row.count_point_id
                result["year"] = row.year
                result["poi"] = poi_num
            except Exception as e: 
                print(e)
                result["count_point_id"] = row.count_point_id
                result["year"] = row.year
                result["poi"] = np.nan
            return result

        client = OhsomeClient()
        print("Ohsome client has been initialized")
        print(f"Earliest date available is {client.start_timestamp} and latest is {client.end_timestamp}")
        tqdm.pandas()
        
        results = {}
        # Using ThreadPoolExecutor to parallelize the process
        with ThreadPoolExecutor() as executor:
            futures = {executor.submit(fetch_poi, row, 500, client): row for row in self.count_station_year.itertuples()}
            for future in tqdm(as_completed(futures), total=len(futures)):
                row = futures[future]
                try:
                    data = future.result()
                    results[row.Index] = data
                except Exception as exc:
                    print(f"Generated an exception: {exc}")

        # Convert results to DataFrame and save as csv
        self.count_poi = pd.DataFrame.from_dict(results, orient='index')
        # save to csv
        self.count_poi.to_csv(Path(self.dir_output) / "count_poi.csv", index=False)
        pass
    
    def clean_slope_data(self):
        if (Path(self.dir_output) / "count_slope.csv").exists():
            self.count_slope = pd.read_csv(Path(self.dir_output) / "count_slope.csv")
            return
        
        # create geopandas GeoDataFrame
        # reproject both to UTM by estimating the UTM zone
        utm_crs = self.count_station_year.estimate_utm_crs()
        count_slope = self.count_station_year.to_crs(utm_crs)
        count_slope['geometry'] = count_slope.geometry.buffer(500)
        with rasterio.open(self.slope_path) as src:
            crs = src.crs
        count_slope = count_slope.to_crs(crs)

        # Compute zonal statistics for each polygon
        stats = zonal_stats(count_slope, self.slope_path, stats=['mean'])

        # The result is a list of dictionaries. Convert it to a DataFrame
        stats_df = pd.DataFrame(stats)

        # Add the mean slope to the GeoDataFrame
        count_slope['slope'] = stats_df['mean']    
        
        # drop unnecessary columns
        count_slope = count_slope.drop(columns=["count", "latitude", "longitude", "geometry"])

        # save to csv
        count_slope.to_csv(Path(self.dir_output) / "count_slope.csv", index=False)
        pass
    
    def clean_all(self):
        super().clean_all()
        
if __name__ == "__main__":
    # load data
    dir_input = "/Volumes/ExFAT2/bike_svi/data/external/cities/London"
    dir_output = "/Volumes/ExFAT2/bike_svi/data/interim/cities/London"
    cleaner = LondonDataCleaner(dir_input, dir_output)
    cleaner.clean_all()