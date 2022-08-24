# -*- coding: utf-8 -*-
import click
import logging
from pathlib import Path
from dotenv import find_dotenv, load_dotenv
from preprocessing import DataProcessor
import os

# @click.command()
# @click.argument('input_filepath', type=click.Path(exists=True))
# @click.argument('output_filepath', type=click.Path())
def main(input_folder, output_folder, city_name):
    """ Runs data processing scripts to turn raw data from (../raw) into
        cleaned data ready to be analyzed (saved in ../processed).
    """
    logger = logging.getLogger(__name__)
    logger.info('making final data set from raw data')
    
    # initialize DataProcessor
    dataprocessor = DataProcessor(input_folder,output_folder, city_name)
    
    # make sure to create a folder with the correct city name and file names 
    # 1. count_station.csv
    # 2. count_data.csv
    dataprocessor.read_data()
    
    # get metadata
    dataprocessor.get_gsv_metadata_multiprocessing()

    # calculate distance
    dataprocessor.calc_dist()
    
    # download GSV images
    dataprocessor.download_gsv()

    # transform panorama svi to perspective svi
    dataprocessor.transform_pano_to_perspective()
    

if __name__ == '__main__':
    log_fmt = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    logging.basicConfig(level=logging.INFO, format=log_fmt)

    # not used in this stub but often useful for finding various files
    project_dir = Path(__file__).resolve().parents[2]

    # find .env automagically by walking up directories until it's found, then
    # load up the .env entries as environment variables
    load_dotenv(find_dotenv())
    input_folder = "./data/external/"
    output_folder = "./data/raw/"
    city_list = []
    with open(os.path.join(input_folder,"city_list.txt"), "r") as f:
        for line in f:
            city_list.append(line.strip())
    for city in city_list:
        main(input_folder,output_folder, city)
