# -*- coding: utf-8 -*-
import click
import logging
from pathlib import Path
from dotenv import find_dotenv, load_dotenv
from preprocessing import DataProcessor
import os
from get_img.my_task import *

# @click.command()
# @click.argument('input_filepath', type=click.Path(exists=True))
# @click.argument('output_filepath', type=click.Path())
def main(input_folder, output_folder):
    """ Runs data processing scripts to turn raw data from (../raw) into
        cleaned data ready to be analyzed (saved in ../processed).
    """
    logger = logging.getLogger(__name__)
    logger.info('making final data set from raw data')
    
    # initialize DataProcessor
    dataprocessor = DataProcessor(input_folder,output_folder)
    dataprocessor.read_data()
    
    # get metadata
    dataprocessor.get_gsv_metadata_multiprocessing()

    # calculate distance
    dataprocessor.calc_dist()
    
    # download GSV images
    # create output folders
    dir_save = dataprocessor.gsv_image_output_folder
    dir_pid = dataprocessor.gsv_metadata_output_folder
    ua_path = "src/data/get_img/utils/UserAgent.csv"
    log_dir = "src/data/get_img/logging"
    # Number of threads
    nthreads = 8

    for name in os.listdir(dir_pid):
    
        path_pid = os.path.join(dir_pid, name)
        log_path = os.path.join(log_dir,name.replace('.csv','_error.csv'))
        dir_save_c = os.path.join(dir_save, name[:-4])
        if not os.path.exists(dir_save_c):
            os.mkdir(dir_save_c)
        UA = get_ua(path=ua_path)
        main(UA, path_pid, dir_save_c,log_path, nthreads)

    
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
    main(input_folder,output_folder)
