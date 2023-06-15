# -*- coding: utf-8 -*-
import logging
from pathlib import Path
from dotenv import find_dotenv, load_dotenv
import pandas as pd
import os

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
        
        # If df_combined is empty, assign df to df_combined
        if df_combined.empty:
            df_combined = df
        else:
            # Perform left join
            df_combined = pd.merge(df_combined, df, on=['count_point_id', 'year'], how='left')
    
    # Save df_combined to CSV 
    df_combined.to_csv(dir_output / "all_var_joined.csv", index=False)

if __name__ == '__main__':
    # find .env automagically by walking up directories until it's found, then
    # load up the .env entries as environment variables
    load_dotenv(find_dotenv())
    
    # set up root_dir
    root_dir = Path(os.getenv("ROOT_DIR"))
    log_path = root_dir / "logs"
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
