# -*- coding: utf-8 -*-
import pandas as pd
import logging
from pathlib import Path
from dotenv import find_dotenv, load_dotenv
from zensvi.download import GSVDownloader
from zensvi.cv import Segmenter
import os
import numpy as np
import osmnx as ox
import geopandas as gpd

from clean_data import MontrealDataCleaner, LondonDataCleaner
import network


# @click.command()
# @click.argument('input_filepath', type=click.Path(exists=True))
# @click.argument('output_filepath', type=click.Path())
def main(gsv_api_key, log_path, dir_input, dir_temp, dir_output, city_name):
    """Runs data processing scripts to turn raw data from (../raw) into
    cleaned data ready to be analyzed (saved in ../processed).
    """
    logger = logging.getLogger(__name__)
    logger.info("making final data set from raw data")

    # read in count_station_clean.csv
    count_station = pd.read_csv(dir_input / "count_station_clean.csv")
    count_station_year = pd.read_csv(dir_input / "count_station_year.csv")
    # left join them
    count_station = pd.merge(
        count_station, count_station_year, on="count_point_id", how="left"
    ).drop(columns=["count"])
    # drop rows with year before 2007
    count_station = count_station[count_station.year >= 2008]
    network_count_df = network.calculate_centralities(count_station)
    # save network_count_df
    network_count_df.to_csv(dir_output / "count_network.csv", index=False)

    # initialize data cleaner
    if city_name == "Montreal":
        data_cleaner = MontrealDataCleaner(dir_input, dir_output)
    elif city_name == "London":
        data_cleaner = LondonDataCleaner(dir_input, dir_output)

    # clean data
    data_cleaner.clean_all()

    if (
        not Path(dir_temp / "pixel_ratios.csv").exists()
        or not Path(dir_temp / "label_counts.csv").exists()
    ):
        # initialize StreetViewDownloader
        downloader = GSVDownloader(
            gsv_api_key=gsv_api_key, log_path=log_path, distance=10, grid_size=10
        )

        # download gsv
        downloader.download_svi(
            dir_temp,
            input_csv_file=str(dir_input / "count_station_clean.csv"),
            id_columns=["count_point_id"],
            buffer=100,
            augment_metadata=True,
            batch_size=1000,
        )

        # segment gsv
        # loop through dir_temp / "gsv_panorama"
        pixel_ratios = []
        label_counts = []
        for folder in Path(dir_temp / "gsv_panorama").iterdir():
            dir_seg_output = dir_temp / "segmentation" / folder.name
            dir_seg_output.mkdir(parents=True, exist_ok=True)
            segmenter = Segmenter(dataset="mapillary", task="panoptic")
            segmenter.segment(
                folder,
                dir_segmentation_summary_output=str(dir_seg_output),
                pixel_ratio_save_format=["csv"],
                csv_format="wide",
                max_workers=4,
            )
            # merge pixel_ratios.csv and label_counts.csv by looping through dir_temp / folder.name
            pixel_ratios.append(pd.read_csv(dir_seg_output / "pixel_ratios.csv"))
            label_counts.append(pd.read_csv(dir_seg_output / "label_counts.csv"))
        pixel_ratios = pd.concat(pixel_ratios)
        label_counts = pd.concat(label_counts)
        pixel_ratios.to_csv(dir_temp / "pixel_ratios.csv", index=False)
        label_counts.to_csv(dir_temp / "label_counts.csv", index=False)

    # left join pids.csv with pixel_ratios.csv
    count_station = pd.read_csv(dir_input / "count_station_clean.csv")
    # convert count_station to geopandas geodataframe from longitude and latitude
    count_station = gpd.GeoDataFrame(
        count_station,
        geometry=gpd.points_from_xy(count_station.longitude, count_station.latitude),
        crs="EPSG:4326",
    )
    # change to utm projection with osmnx
    count_station["geometry"] = (
        ox.projection.project_gdf(count_station).buffer(100).to_crs(epsg=4326)
    )
    # read in pixel_ratios.csv
    pids = pd.read_csv(dir_temp / "gsv_pids.csv")[
        ["panoid", "year", "month", "lat", "lon"]
    ]
    pixel_ratios = pd.read_csv(dir_temp / "pixel_ratios.csv").fillna(0)
    # calculate visual complexity (i.e. entropy): ∑𝑖=1𝑘𝑃𝑖×ln(𝑃𝑖)ln(𝑘)
    pixel_ratios["visual_complexity"] = (
        -1
        * pixel_ratios.iloc[:, 1:].apply(
            lambda row: np.sum([val * np.log(val) if val > 0 else 0 for val in row]),
            axis=1,
        )
        / np.log(len(pixel_ratios.columns[1:]))
    )
    pids_pixel_ratios = pd.merge(
        pids, pixel_ratios, left_on="panoid", right_on="filename_key", how="left"
    ).drop(columns=["panoid", "filename_key"])
    # convert pids_pixel_ratios to geopandas geodataframe from longitude and latitude
    pids_pixel_ratios = gpd.GeoDataFrame(
        pids_pixel_ratios,
        geometry=gpd.points_from_xy(pids_pixel_ratios.lon, pids_pixel_ratios.lat),
        crs="EPSG:4326",
    )
    # spatial join pids_pixel_ratios with count_station by year
    pids_pixel_ratios_all_year_month = []
    for years in pids_pixel_ratios.year.unique():
        pids_pixel_ratios_year = pids_pixel_ratios[pids_pixel_ratios.year == years]
        for month in pids_pixel_ratios_year.month.unique():
            pids_pixel_ratios_year_month = pids_pixel_ratios_year[
                pids_pixel_ratios_year.month == month
            ]
            pids_pixel_ratios_year_month = gpd.sjoin(
                count_station, pids_pixel_ratios_year_month, how="left", op="intersects"
            )
            # aggregate all the columns except for count_point_id and year
            pids_pixel_ratios_year_month = (
                pids_pixel_ratios_year_month.groupby(
                    ["count_point_id", "year", "month"]
                )
                .agg("mean")
                .reset_index()
            )
            # drop latitude, longitude, lat, lon, and index_right
            pids_pixel_ratios_year_month.drop(
                columns=["latitude", "longitude", "lat", "lon", "index_right"],
                inplace=True,
            )
            pids_pixel_ratios_all_year_month.append(pids_pixel_ratios_year_month)
    pids_pixel_ratios_all_year_month = pd.concat(pids_pixel_ratios_all_year_month)
    # add prefix to column names
    pids_pixel_ratios_all_year_month.columns = ["count_point_id", "year", "month"] + [
        "ss_"
        + col.lower()
        .replace(" - ", "_")
        .replace(" ", "_")
        .replace("(", "")
        .replace(")", "")
        for col in pids_pixel_ratios_all_year_month.columns[3:]
    ]

    # left join pids.csv with label_counts.csv
    label_counts = pd.read_csv(dir_temp / "label_counts.csv").fillna(0)
    pids_label_counts = pd.merge(
        pids, label_counts, left_on="panoid", right_on="filename_key", how="left"
    ).drop(columns=["panoid", "filename_key"])
    # convert pids_pixel_ratios to geopandas geodataframe from longitude and latitude
    pids_label_counts = gpd.GeoDataFrame(
        pids_label_counts,
        geometry=gpd.points_from_xy(pids_label_counts.lon, pids_label_counts.lat),
        crs="EPSG:4326",
    )
    # spatial join pids_label_counts with count_station by year
    pids_label_counts_all_year_month = []
    for years in pids_label_counts.year.unique():
        pids_label_counts_year = pids_label_counts[pids_label_counts.year == years]
        for month in pids_label_counts_year.month.unique():
            pids_label_counts_year_month = pids_label_counts_year[
                pids_label_counts_year.month == month
            ]
            pids_label_counts_year_month = gpd.sjoin(
                count_station, pids_label_counts_year_month, how="left", op="intersects"
            )
            # aggregate all the columns except for count_point_id and year
            pids_label_counts_year_month = (
                pids_label_counts_year_month.groupby(
                    ["count_point_id", "year", "month"]
                )
                .agg("mean")
                .reset_index()
            )
            # drop latitude, longitude, lat, lon, and index_right
            pids_label_counts_year_month.drop(
                columns=["latitude", "longitude", "lat", "lon", "index_right"],
                inplace=True,
            )
            pids_label_counts_all_year_month.append(pids_label_counts_year_month)
    pids_label_counts_all_year_month = pd.concat(pids_label_counts_all_year_month)
    # add prefix to column names
    pids_label_counts_all_year_month.columns = ["count_point_id", "year", "month"] + [
        "od_"
        + col.lower()
        .replace(" - ", "_")
        .replace(" ", "_")
        .replace("(", "")
        .replace(")", "")
        for col in pids_label_counts_all_year_month.columns[3:]
    ]

    # save pids_pixel_ratios and pids_label_counts
    pids_pixel_ratios_all_year_month.to_csv(
        dir_output / "count_pixel_ratios.csv", index=False
    )
    pids_label_counts_all_year_month.to_csv(
        dir_output / "count_label_counts.csv", index=False
    )


if __name__ == "__main__":
    # find .env automagically by walking up directories until it's found, then
    # load up the .env entries as environment variables
    load_dotenv(find_dotenv())

    # set up root_dir
    # check if data folder exists
    root_dir = Path(os.getenv("ROOT_DIR"))
    if not root_dir.exists():
        root_dir = Path("./")
    log_path = root_dir / "logs.txt"
    log_fmt = "%(asctime)s - %(name)s - %(levelname)s - %(message)s"
    logging.basicConfig(filename=log_path, level=logging.INFO, format=log_fmt)

    # not used in this stub but often useful for finding various files
    project_dir = Path(__file__).resolve().parents[2]

    # set up gsv api key
    gsv_api_key = os.getenv("GSV_API_KEY")
    data_folder = root_dir / "data"
    input_folder = data_folder / "external/cities"
    temp_folder = data_folder / "raw/cities"
    output_folder = data_folder / "interim/cities"
    city_list = []
    with open(data_folder / "external/city_list.txt", "r") as f:
        for line in f:
            city_list.append(line.strip())
    for city in city_list:
        print("Processing city: ", city)
        dir_input = Path(input_folder) / city
        dir_output = Path(output_folder) / city
        dir_temp = Path(temp_folder) / city
        dir_output.mkdir(parents=True, exist_ok=True)
        dir_temp.mkdir(parents=True, exist_ok=True)
        main(gsv_api_key, log_path, dir_input, dir_temp, dir_output, city)
