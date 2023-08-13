# -*- coding: utf-8 -*-
import logging
from pathlib import Path
from dotenv import find_dotenv, load_dotenv
import pandas as pd
import os
import numpy as np

def main(dir_input, dir_output):
    # Get a list of all CSV files excluding the ones starting with "."
    files = [file for file in dir_input.glob('*.csv') if not file.name.startswith('.')]
    
    # Sort files to make sure "count_station_year.csv" is the first
    files.sort(key=lambda x: x.name != 'count_station_year.csv')

    # Initialize an empty DataFrame
    df_combined = pd.DataFrame()

    # Iterate over each file
    for file in files:
        # Read CSV file into DataFrame
        df = pd.read_csv(file)
        # convert count_point_id and year to string type
        df['count_point_id'] = df['count_point_id'].astype(int).astype(str)
        df['year'] = df['year'].astype(int).astype(str)
        
        # If df_combined is empty, assign df to df_combined
        if df_combined.empty:
            df_combined = df
        else:
            # Perform left join
            df_combined = pd.merge(df_combined, df, on=['count_point_id', 'year'], how='left')

    # feature engineering
    # # log count column
    # df_combined['count_log'] = np.log(df_combined['count'] + 1)
    

    # make slope binary with a threshold of 70 percentile
    df_combined["slope_binary"] = np.where(df_combined['slope'] > df_combined['slope'].quantile(0.7), 1, 0)

    # Apply ifelse conditions
    # multiply all the columns whose names start with "ss_" by 100
    df_combined.loc[:, df_combined.columns.str.startswith('ss_')] *= 100
    df_combined['ss_visual_complexity'] = np.where(df_combined['ss_visual_complexity'] > 100, 100, df_combined['ss_visual_complexity'])
    df_combined['ss_sidewalk_binary'] = np.where(df_combined['ss_sidewalk'] > df_combined['ss_sidewalk'].quantile(0.7), 1, 0)
    df_combined['ss_pedestrian_area_binary'] = np.where(df_combined['ss_pedestrian_area'] > 0, 1, 0)
    df_combined['ss_bike_lane_binary'] = np.where(df_combined['ss_bike_lane'] > 0, 1, 0)
    df_combined['ss_bike_rack_binary'] = np.where(df_combined['ss_bike_rack'] > 0, 1, 0)
    df_combined['ss_parking_binary'] = np.where(df_combined['ss_parking'] > 0, 1, 0)
    df_combined['ss_curb_binary'] = np.where(df_combined['ss_curb'] > 0, 1, 0)
    df_combined['ss_curb_cut_binary'] = np.where(df_combined['ss_curb_cut'] > 0, 1, 0)
    df_combined['ss_pothole_binary'] = np.where(df_combined['ss_pothole'] > 0, 1, 0)
    df_combined['ss_street_light_binary'] = np.where(df_combined['ss_street_light'] > 0, 1, 0)
    df_combined['ss_guard_rail_binary'] = np.where(df_combined['ss_guard_rail'] > 0, 1, 0)
    df_combined['ss_bench_binary'] = np.where(df_combined['ss_bench'] > 0, 1, 0)
    # for vegetation, threshold should be determined by 70% percentile of the distribution
    df_combined['ss_vegetation_binary'] = np.where(df_combined['ss_vegetation'] > df_combined['ss_vegetation'].quantile(0.7), 1, 0)

    # Calculate new columns
    new_columns = ['ss_construction', 'ss_road_flat', 'ss_marking', 'ss_nature', 'ss_street_object', 'od_person_count', 'od_bicycle_count', 'od_vehicle_count', 'od_animal_count']
    df_combined[new_columns[0]] = df_combined['ss_fence'] + df_combined['ss_barrier'] + df_combined['ss_wall'] + df_combined['ss_bridge'] + df_combined['ss_building'] + df_combined['ss_tunnel']
    df_combined[new_columns[1]] = df_combined['ss_crosswalk_plain'] + df_combined['ss_rail_track'] + df_combined['ss_road'] + df_combined['ss_service_lane']
    df_combined[new_columns[2]] = df_combined['ss_lane_marking_crosswalk'] + df_combined['ss_lane_marking_general']
    df_combined[new_columns[3]] = df_combined['ss_mountain'] + df_combined['ss_sand'] + df_combined['ss_snow'] + df_combined['ss_terrain'] + df_combined['ss_water']
    df_combined[new_columns[4]] = df_combined['ss_banner'] + df_combined['ss_billboard'] + df_combined['ss_catch_basin'] + df_combined['ss_cctv_camera'] + df_combined['ss_fire_hydrant'] + df_combined['ss_junction_box'] + df_combined['ss_mailbox'] + df_combined['ss_manhole'] + df_combined['ss_phone_booth'] + df_combined['ss_pole'] + df_combined['ss_traffic_sign_frame'] + df_combined['ss_utility_pole'] + df_combined['ss_traffic_light'] + df_combined['ss_traffic_sign_back'] + df_combined['ss_traffic_sign_front'] + df_combined['ss_trash_can']
    df_combined[new_columns[5]] = df_combined['od_person']
    df_combined[new_columns[6]] = df_combined['od_bicyclist'] + df_combined['od_bicycle']
    df_combined[new_columns[7]] = df_combined['od_bus'] + df_combined['od_car'] + df_combined['od_caravan'] + df_combined['od_motorcycle'] + df_combined['od_truck'] + df_combined['od_other_vehicle'] + df_combined['od_trailer'] + df_combined['od_on_rails'] + df_combined['od_wheeled_slow'] + df_combined['od_ego_vehicle']
    df_combined[new_columns[8]] = df_combined['od_bird'] + df_combined['od_ground_animal']

    # Convert 'count_point_id' to string type
    df_combined['count_point_id'] = df_combined['count_point_id'].astype(str)

    # List of columns to keep
    cols_to_keep = new_columns + ["ss_visual_complexity", "ss_vegetation", "ss_bike_lane", "ss_bike_rack", "ss_curb", "ss_curb_cut", "ss_parking", "ss_pothole", "ss_street_light", "ss_vegetation", "ss_guard_rail", "ss_pedestrian_area", "ss_sidewalk", "ss_street_light", "ss_bench"]

    # Drop original 'ss_' and 'od_' columns that are not included in the cols_to_keep
    ss_od_columns_to_drop = [col for col in df_combined.columns if col.startswith('ss_') or col.startswith('od_')]
    # remove columns that contains "binary" in their names
    ss_od_columns_to_drop = [col for col in ss_od_columns_to_drop if col not in cols_to_keep and "binary" not in col]

    df_combined = df_combined.drop(ss_od_columns_to_drop, axis=1)

    # # Drop rows with NA values
    # df_combined = df_combined.dropna()
    
    # Convert 'year' column to string type
    df_combined['year'] = df_combined['year'].astype(str)

    # only keep years between 2008 and 2023
    df_combined = df_combined[(df_combined['year'] >= '2008') & (df_combined['year'] <= '2023')]
    
    # Find columns where maximum value is greater than 100
    max_over_x_cols = df_combined.columns[(df_combined.max().astype(float) > 100) & (~df_combined.columns.isin(['count_point_id', 'count', 'year']))]

    # Log transform these columns and add '_log' to their names
    for col in max_over_x_cols:
        df_combined[col + '_log'] = np.log(df_combined[col] + 1)
        # drop the original column
        df_combined = df_combined.drop(col, axis=1)

    # Replace infinite values with 0
    df_combined = df_combined.replace([np.inf, -np.inf], 0)

    # Move 'count' column to the first position
    df_combined = df_combined[['count', 'count_log'] + [ col for col in df_combined.columns if col != 'count' ]]
    
    # Save df_combined to CSV 
    df_combined.to_csv(dir_output / "all_var_joined.csv", index=False)

if __name__ == '__main__':
    # find .env automagically by walking up directories until it's found, then
    # load up the .env entries as environment variables
    load_dotenv(find_dotenv())
    
    # set up root_dir
    root_dir = Path(os.getenv("ROOT_DIR"))
    if not root_dir.exists():
        # get root_dir from current working directory
        root_dir = Path(os.getcwd())
    log_path = root_dir / "logs/logs.txt"
    log_fmt = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    logging.basicConfig(filename=log_path, level=logging.INFO, format=log_fmt)

    # not used in this stub but often useful for finding various files
    project_dir = Path(__file__).resolve().parents[2]

    # set up gsv api key
    data_folder = root_dir / "data"
    input_folder = data_folder / "interim/cities"
    output_folder = data_folder / "processed/cities"
    city_list = []
    with open(data_folder / "external/city_list.txt", "r") as f:
        for line in f:
            city_list.append(line.strip())
    for city in city_list:
        print("Processing city: ", city)
        dir_input = Path(input_folder) / city
        dir_output = Path(output_folder) / city
        dir_output.mkdir(parents=True, exist_ok=True)
        main(dir_input, 
            dir_output)
