import os
import pandas as pd
import geopandas as gpd
import glob
import rasterio
from pathlib import Path
from lxml import etree
from tqdm import tqdm
from typing import Callable, Union
from concurrent.futures import ThreadPoolExecutor, as_completed
import pandas as pd
from tqdm import tqdm
from ohsome import OhsomeClient
from tenacity import retry, stop_after_attempt
from rasterstats import zonal_stats
import geopandas as gpd
import rasterio
import numpy as np

from .base import BaseDataCleaner
from .terrain import dem_to_slope


class MontrealDataCleaner(BaseDataCleaner):
    def __init__(self, dir_input, dir_output):
        super().__init__(dir_input, dir_output)

    def read_xml(
        self,
        path_input,
        interest_group: list[str],
        interest_col: str,
        condition: Callable,
        yield_data: Callable,
    ):
        # Define namespace dictionary
        namespaces = {
            "generic": "http://www.SDMX.org/resources/SDMXML/schemas/v2_0/generic"
        }

        def iterparse_large_xml(file_path):
            context = etree.iterparse(
                file_path,
                events=("end",),
                tag="{http://www.SDMX.org/resources/SDMXML/schemas/v2_0/generic}Series",
            )

            # Create a manual TQDM instance for yielding progress
            pbar = tqdm(total=None, desc="Yielding data", unit="series")

            for event, elem in context:
                values = {
                    child.get("concept"): child.get("value")
                    for child in elem.findall(
                        "generic:SeriesKey/generic:Value", namespaces=namespaces
                    )
                }

                obs_value_elem = elem.find(
                    "generic:Obs/generic:ObsValue", namespaces=namespaces
                )
                obs_value = (
                    obs_value_elem.get("value") if obs_value_elem is not None else None
                )

                # Check the conditions
                if condition(values, obs_value, interest_group, interest_col):
                    pbar.update()  # Update the progress bar only when yielding
                    yield yield_data(values, obs_value, interest_col)

                elem.clear()
                while elem.getprevious() is not None:
                    del elem.getparent()[0]

            # Close the progress bar when done
            pbar.close()

        # usage
        data_generator = iterparse_large_xml(str(path_input))

        data_list = []
        for data in tqdm(data_generator, desc="Parsing XML"):
            data_list.append(data)

        df = pd.DataFrame(data_list)
        return df

    def read_csv(self, path_input, col_name: str, interest_group: list[str]):
        try:
            df = pd.read_csv(str(path_input))
        except UnicodeDecodeError:
            df = pd.read_csv(str(path_input), encoding="ISO-8859-1")
        df = df[df[col_name].isin(interest_group)]
        return df

    def standardize_census_data(
        self,
        df,
        year,
        census_id: str,
        interest_col: str,
        value_col: str,
        col_mapper: dict,
        custom_clean_func: Callable = None,
    ):
        # rename columns
        df = df.rename(
            {
                census_id: "census_id",
                interest_col: "interest_col",
                value_col: "value_col",
            },
            axis=1,
        )
        # drop other columns
        df = df[["census_id", "interest_col", "value_col"]]
        # set data type
        df = df.astype({"census_id": "object", "interest_col": "str"})
        # Flatten the input_dict to create a mapping dictionary:
        col_mapper = {
            value: key for key, values in col_mapper.items() for value in values
        }
        # filter out interest_col to keep what's in the values of col_mapper
        df = df[df["interest_col"].isin(col_mapper.keys())]
        # convert value_col to numeric
        df["value_col"] = pd.to_numeric(df["value_col"], errors="coerce")
        df["value_col"] = df["value_col"].fillna(0)
        # map interest_col to the values of col_mapper
        df["interest_col"] = df["interest_col"].map(col_mapper)
        # group by census_id and interest_col
        df = df.groupby(["census_id", "interest_col"]).sum().reset_index()
        # pivot table
        df = df.pivot(
            index="census_id", columns="interest_col", values="value_col"
        ).reset_index()
        # add year column
        df["year"] = year

        if custom_clean_func is not None:
            df = custom_clean_func(df)

        return df

    def load_age_data(self, path_input, year):
        # check if the csv file exists
        if os.path.exists(str(path_input.parent / f"age_{str(year)}.csv")):
            df = pd.read_csv(str(path_input.parent / f"age_{str(year)}.csv"))
            return df

        # set variables
        interest_group = {
            2006: [
                "1",
                "2",
                "8",
                "14",
                "20",
                "26",
                "32",
                "38",
                "44",
                "50",
                "56",
                "62",
                "68",
                "74",
                "80",
                "86",
                "92",
                "98",
                "104",
                "110",
                "116",
                "122",
            ],
            2011: [
                "1",
                "3",
                "9",
                "15",
                "22",
                "28",
                "35",
                "41",
                "48",
                "54",
                "61",
                "67",
                "74",
                "80",
                "87",
                "93",
                "99",
                "106",
                "112",
                "118",
                "124",
                "130",
            ],
            2016: [
                "Total - Age",
                "0 to 4 years",
                "5 to 9 years",
                "10 to 14 years",
                "15 to 19 years",
                "20 to 24 years",
                "25 to 29 years",
                "30 to 34 years",
                "35 to 39 years",
                "40 to 44 years",
                "45 to 49 years",
                "50 to 54 years",
                "55 to 59 years",
                "60 to 64 years",
                "65 to 69 years",
                "70 to 74 years",
                "75 to 79 years",
                "80 to 84 years",
                "85 to 89 years",
                "90 to 94 years",
                "95 to 99 years",
                "100 years and over",
            ],
            2021: [
                "Total - Age groups of the population - 100% data",
                "    0 to 4 years",
                "    5 to 9 years",
                "    10 to 14 years",
                "    15 to 19 years",
                "    20 to 24 years",
                "    25 to 29 years",
                "    30 to 34 years",
                "    35 to 39 years",
                "    40 to 44 years",
                "    45 to 49 years",
                "    50 to 54 years",
                "    55 to 59 years",
                "    60 to 64 years",
                "    65 to 69 years",
                "    70 to 74 years",
                "    75 to 79 years",
                "    80 to 84 years",
                "    85 to 89 years",
                "    90 to 94 years",
                "    95 to 99 years",
                "    100 years and over",
            ],
        }

        census_id = {
            2006: "GEO",
            2011: "GEO",
            2016: "GEO_CODE (POR)",
            2021: "ALT_GEO_CODE",
        }

        interest_col = {
            2006: "AGE",
            2011: "AGE",
            2016: "DIM: Age (in single years) and average age (127)",
            2021: "CHARACTERISTIC_NAME",
        }

        value_col = {
            2006: "OBS_VALUE",
            2011: "OBS_VALUE",
            2016: "Dim: Sex (3): Member ID: [1]: Total - Sex",
            2021: "C1_COUNT_TOTAL",
        }

        col_mapper = {
            2006: {
                "total": ["1"],
                "age_0_19": ["2", "8", "14", "20"],
                "age_20_39": ["26", "32", "38", "44"],
                "age_40_59": ["50", "56", "62", "68"],
                "age_60_90": ["74", "80", "86", "92", "98", "104", "110", "116", "122"],
            },
            2011: {
                "total": ["1"],
                "age_0_19": ["3", "9", "15", "22"],
                "age_20_39": ["28", "35", "41", "48"],
                "age_40_59": ["54", "61", "67", "74"],
                "age_60_90": [
                    "80",
                    "87",
                    "93",
                    "99",
                    "106",
                    "112",
                    "118",
                    "124",
                    "130",
                ],
            },
            2016: {
                "total": ["Total - Age"],
                "age_0_19": [
                    "0 to 4 years",
                    "5 to 9 years",
                    "10 to 14 years",
                    "15 to 19 years",
                ],
                "age_20_39": [
                    "20 to 24 years",
                    "25 to 29 years",
                    "30 to 34 years",
                    "35 to 39 years",
                ],
                "age_40_59": [
                    "40 to 44 years",
                    "45 to 49 years",
                    "50 to 54 years",
                    "55 to 59 years",
                ],
                "age_60_90": [
                    "60 to 64 years",
                    "65 to 69 years",
                    "70 to 74 years",
                    "75 to 79 years",
                    "80 to 84 years",
                    "85 to 89 years",
                    "90 to 94 years",
                    "95 to 99 years",
                    "100 years and over",
                ],
            },
            2021: {
                "total": ["Total - Age groups of the population - 100% data"],
                "age_0_19": [
                    "    0 to 4 years",
                    "    5 to 9 years",
                    "    10 to 14 years",
                    "    15 to 19 years",
                ],
                "age_20_39": [
                    "    20 to 24 years",
                    "    25 to 29 years",
                    "    30 to 34 years",
                    "    35 to 39 years",
                ],
                "age_40_59": [
                    "    40 to 44 years",
                    "    45 to 49 years",
                    "    50 to 54 years",
                    "    55 to 59 years",
                ],
                "age_60_90": [
                    "    60 to 64 years",
                    "    65 to 69 years",
                    "    70 to 74 years",
                    "    75 to 79 years",
                    "    80 to 84 years",
                    "    85 to 89 years",
                    "    90 to 94 years",
                    "    95 to 99 years",
                    "    100 years and over",
                ],
            },
        }

        def custom_clean_func(df):
            # clean "census_id" column
            # convert to string
            df["census_id"] = df["census_id"].astype(str)
            df["census_id"] = df["census_id"].str[:4] + df["census_id"].str[-4:]
            return df

        if (year == 2006) | (year == 2011):

            def age_condition_func(values, obs_value, interest_group, interest_col):
                return (
                    values.get("GEO", None) is not None
                    and len(values.get("GEO", None)) == 11
                    and values.get("Age", values.get(interest_col)) in interest_group
                    and values.get("Sex", None) == "1"
                    and obs_value is not None
                )

            def yield_func(values, obs_value, interest_col):
                return {
                    "GEO": values["GEO"],
                    interest_col: values.get("Age", values.get("AGE")),
                    "OBS_VALUE": obs_value,
                }

            # read the xml file
            df = self.read_xml(
                path_input,
                interest_group[year],
                interest_col[year],
                condition=age_condition_func,
                yield_data=yield_func,
            )
            # standardize census data
            df = self.standardize_census_data(
                df,
                year,
                census_id[year],
                interest_col[year],
                value_col[year],
                col_mapper[year],
                custom_clean_func=custom_clean_func,
            )

        elif (year == 2016) | (year == 2021):
            # read the csv file
            df = self.read_csv(path_input, interest_col[year], interest_group[year])
            # standardize census data
            if year == 2016:
                df = self.standardize_census_data(
                    df,
                    year,
                    census_id[year],
                    interest_col[year],
                    value_col[year],
                    col_mapper[year],
                    custom_clean_func=custom_clean_func,
                )
            else:
                df = self.standardize_census_data(
                    df,
                    year,
                    census_id[year],
                    interest_col[year],
                    value_col[year],
                    col_mapper[year],
                )
        # save the data
        df.to_csv(str(path_input.parent / f"age_{str(year)}.csv"), index=False)
        return df

    def load_income_data(self, path_input, year):
        # check if the csv file exists
        if os.path.exists(str(path_input.parent / f"income_{str(year)}.csv")):
            df = pd.read_csv(str(path_input.parent / f"income_{str(year)}.csv"))
            return df

        # set variables
        interest_group = {
            2006: ["8"],
            2016: ["After-tax income"],
            2021: ["After-tax income"],
        }

        census_id = {2006: "GEO", 2016: "GEO_CODE (POR)", 2021: "DGUID"}

        interest_col = {
            2006: "B06_PresInc",
            2016: "DIM: Income sources and taxes (34)",
            2021: "Income sources and taxes (32)",
        }

        value_col = {
            2006: "OBS_VALUE",
            2016: "Dim: Income statistics (4): Member ID: [4]: Median amount ($) (Note: 35)",
            2021: "Income statistics (8):Median amount ($)[3]",
        }

        col_mapper = {
            2006: {"average_income": ["8"]},
            2016: {"average_income": ["After-tax income"]},
            2021: {"average_income": ["After-tax income"]},
        }

        if year == 2006:

            def condition_func(values, obs_value, interest_group, interest_col):
                return (
                    values.get("GEO", None) is not None
                    and len(values.get("GEO", None)) == 11
                    and values.get(interest_col) in interest_group
                    and values.get("Age") == "1"
                    and values.get("Sex", None) == "1"
                    and obs_value is not None
                )

            def yield_func(values, obs_value, interest_col):
                return {
                    "GEO": values["GEO"],
                    interest_col: values[interest_col],
                    "OBS_VALUE": obs_value,
                }

            def custom_clean_func(df):
                # clean "census_id" column
                df["census_id"] = df["census_id"].str[:4] + df["census_id"].str[-4:]
                return df

            # read the xml file
            df = self.read_xml(
                path_input,
                interest_group[year],
                interest_col[year],
                condition=condition_func,
                yield_data=yield_func,
            )
            # standardize census data
            df = self.standardize_census_data(
                df,
                year,
                census_id[year],
                interest_col[year],
                value_col[year],
                col_mapper[year],
                custom_clean_func=custom_clean_func,
            )

        elif (year == 2016) | (year == 2021):
            # read the csv file
            df = self.read_csv(path_input, interest_col[year], interest_group[year])
            # standardize census data
            if year == 2021:

                def custom_clean_func(df):
                    # clean "census_id" column
                    df["census_id"] = df["census_id"].str[9:]
                    return df

                df = self.standardize_census_data(
                    df,
                    year,
                    census_id[year],
                    interest_col[year],
                    value_col[year],
                    col_mapper[year],
                    custom_clean_func=custom_clean_func,
                )
            else:
                df = self.standardize_census_data(
                    df,
                    year,
                    census_id[year],
                    interest_col[year],
                    value_col[year],
                    col_mapper[year],
                )
        # save the data
        df.to_csv(str(path_input.parent / f"income_{str(year)}.csv"), index=False)
        return df

    def load_housing_data(self, path_input, year):
        # check if the csv file exists
        if os.path.exists(str(path_input.parent / f"housing_{str(year)}.csv")):
            df = pd.read_csv(str(path_input.parent / f"housing_{str(year)}.csv"))
            return df

        # set variables
        interest_group = {
            2006: ["14"],
            2016: ["Median value of dwellings $"],
            2021: ["Median value of dwelling $"],
        }

        census_id = {2006: "GEO", 2016: "GEO_CODE (POR)", 2021: "DGUID"}

        interest_col = {
            2006: "B06_ValueDwell",
            2016: "DIM: Value (owner-estimated) of dwelling (16)",
            2021: "Value (owner-estimated) of dwelling (16)",
        }

        value_col = {
            2006: "OBS_VALUE",
            2016: "Dim: Structural type of dwelling (10): Member ID: [1]: Total - Structural type of dwelling",
            2021: "Structural type of dwelling (10):Total - Structural type of dwelling[1]",
        }

        col_mapper = {
            2006: {"housing_price": ["14"]},
            2016: {"housing_price": ["Median value of dwellings $"]},
            2021: {"housing_price": ["Median value of dwelling $"]},
        }

        if year == 2006:

            def condition_func(values, obs_value, interest_group, interest_col):
                return (
                    values.get("GEO", None) is not None
                    and len(values.get("GEO", None)) == 11
                    and values.get(interest_col) in interest_group
                    and values.get("BedRm") == "1"
                    and values.get("DType", None) == "1"
                    and obs_value is not None
                )

            def yield_func(values, obs_value, interest_col):
                return {
                    "GEO": values["GEO"],
                    interest_col: values[interest_col],
                    "OBS_VALUE": obs_value,
                }

            def custom_clean_func(df):
                # clean "census_id" column
                df["census_id"] = df["census_id"].str[:4] + df["census_id"].str[-4:]
                return df

            # read the xml file
            df = self.read_xml(
                path_input,
                interest_group[year],
                interest_col[year],
                condition=condition_func,
                yield_data=yield_func,
            )
            # standardize census data
            df = self.standardize_census_data(
                df,
                year,
                census_id[year],
                interest_col[year],
                value_col[year],
                col_mapper[year],
                custom_clean_func=custom_clean_func,
            )

        elif (year == 2016) | (year == 2021):
            # read the csv file
            df = self.read_csv(path_input, interest_col[year], interest_group[year])
            # standardize census data
            if year == 2021:

                def custom_clean_func(df):
                    # clean "census_id" column
                    df["census_id"] = df["census_id"].str[9:]
                    return df

                df = self.standardize_census_data(
                    df,
                    year,
                    census_id[year],
                    interest_col[year],
                    value_col[year],
                    col_mapper[year],
                    custom_clean_func=custom_clean_func,
                )
            else:
                df = self.standardize_census_data(
                    df,
                    year,
                    census_id[year],
                    interest_col[year],
                    value_col[year],
                    col_mapper[year],
                )
        # save the data
        df.to_csv(str(path_input.parent / f"housing_{str(year)}.csv"), index=False)
        return df

    def load_data(self):
        # load count data
        # get a list of files in the directory that doesn't end with "count_station.csv"
        list_files = [
            file
            for file in Path(self.dir_input).iterdir()
            if file.suffix == ".csv"
            and not file.stem.startswith("count_station")
            and not file.name.startswith(".")
        ]
        # load csv files into a list of dataframes
        list_df = [pd.read_csv(file) for file in list_files]
        # concatenate the list of dataframes into a single dataframe
        self.count_df = pd.concat(list_df)

        # load administrative data
        dissem_area_2006 = gpd.read_file(
            Path(self.dir_input)
            / "control_variables/admin_boundary/gda_000b06a_e/gda_000b06a_e.shp"
        ).to_crs("EPSG:4326")
        dissem_area_2011 = gpd.read_file(
            Path(self.dir_input)
            / "control_variables/admin_boundary/gda_000b11a_e/gda_000b11a_e.shp"
        ).to_crs("EPSG:4326")
        dissem_area_2016 = gpd.read_file(
            Path(self.dir_input)
            / "control_variables/admin_boundary/lda_000b16a_e/lda_000b16a_e.shp"
        ).to_crs("EPSG:4326")
        dissem_area_2021 = gpd.read_file(
            Path(self.dir_input)
            / "control_variables/admin_boundary/lda_000b21a_e/lda_000b21a_e.shp"
        ).to_crs("EPSG:4326")
        census_tract_2016 = gpd.read_file(
            Path(self.dir_input)
            / "control_variables/admin_boundary/lct_000b16a_e/lct_000b16a_e.shp"
        ).to_crs("EPSG:4326")
        census_tract_2021 = gpd.read_file(
            Path(self.dir_input)
            / "control_variables/admin_boundary/lct_000b21a_e/lct_000b21a_e.shp"
        ).to_crs("EPSG:4326")
        census_subdivision_2016 = gpd.read_file(
            Path(self.dir_input)
            / "control_variables/admin_boundary/lcsd000b16a_e/lcsd000b16a_e.shp"
        ).to_crs("EPSG:4326")
        census_subdivision_2021 = gpd.read_file(
            Path(self.dir_input)
            / "control_variables/admin_boundary/lcsd000b21a_e/lcsd000b21a_e.shp"
        ).to_crs("EPSG:4326")

        # load age data
        age_2006 = self.load_age_data(
            Path(self.dir_input)
            / "control_variables/age/97-551-XCB2006006/Generic_97-551-XCB2006006.xml",
            2006,
        )
        age_2011 = self.load_age_data(
            Path(self.dir_input)
            / "control_variables/age/98-311-XCB2011018/Generic_98-311-XCB2011018.xml",
            2011,
        )
        age_2016 = self.load_age_data(
            Path(self.dir_input)
            / "control_variables/age/98-400-X2016003_ENG_CSV/98-400-X2016003_English_CSV_data.csv",
            2016,
        )
        age_2021 = self.load_age_data(
            Path(self.dir_input)
            / "control_variables/age/98-401-X2021007_eng_CSV/98-401-X2021007_English_CSV_data.csv",
            2021,
        )

        # load income (Average/median 2005 after-tax income $) data
        income_2006 = self.load_income_data(
            Path(self.dir_input)
            / "control_variables/deprivation/97-563-XCB2006072/Generic_97-563-XCB2006072.xml",
            2006,
        )
        income_2016 = self.load_income_data(
            Path(self.dir_input)
            / "control_variables/deprivation/98-400-X2016121_ENG_CSV/98-400-X2016121_English_CSV_data.csv",
            2016,
        )
        income_2021 = self.load_income_data(
            Path(self.dir_input)
            / "control_variables/deprivation/98100071-eng/98100071.csv",
            2021,
        )

        # load housing (Median value of dwelling $) data
        housing_2006 = self.load_housing_data(
            Path(self.dir_input)
            / "control_variables/housing/97-554-XCB2006040/Generic_97-554-XCB2006040.xml",
            2006,
        )
        housing_2016 = self.load_housing_data(
            Path(self.dir_input)
            / "control_variables/housing/98-400-X2016233_ENG_CSV/98-400-X2016233_English_CSV_data.csv",
            2016,
        )
        housing_2021 = self.load_housing_data(
            Path(self.dir_input)
            / "control_variables/housing/98100257-eng/98100257.csv",
            2021,
        )

        # load landuse data
        landuse_2016 = gpd.read_file(
            Path(self.dir_input) / "control_variables/land_use/graffectations.geojson"
        )

        # load slope data
        path_input = str(
            Path(self.dir_input)
            / "gis_variables/slope/cdem_dem_031H_tif/cdem_dem_031H.tif"
        )
        self.slope_path = str(Path(self.dir_input) / "gis_variables/slope/slope.tif")
        if not Path(self.slope_path).exists():
            dem_to_slope(path_input, self.slope_path)

        # store them in a dictionary
        self.census_boundaries = {
            "dissem_area_2006": {"data": dissem_area_2006, "census_id": "DAUID"},
            "dissem_area_2011": {"data": dissem_area_2011, "census_id": "DAUID"},
            "dissem_area_2016": {"data": dissem_area_2016, "census_id": "DAUID"},
            "dissem_area_2021": {"data": dissem_area_2021, "census_id": "DAUID"},
            "census_tract_2016": {"data": census_tract_2016, "census_id": "CTUID"},
            "census_tract_2021": {"data": census_tract_2021, "census_id": "CTUID"},
            "census_subdivision_2016": {
                "data": census_subdivision_2016,
                "census_id": "CSDUID",
            },
            "census_subdivision_2021": {
                "data": census_subdivision_2021,
                "census_id": "CSDUID",
            },
        }

        self.age_data = {
            2006: {"data": age_2006, "census_boundaries": "dissem_area_2006"},
            2011: {"data": age_2011, "census_boundaries": "dissem_area_2011"},
            2016: {"data": age_2016, "census_boundaries": "dissem_area_2016"},
            2021: {"data": age_2021, "census_boundaries": "census_tract_2021"},
        }

        self.income_data = {
            2006: {"data": income_2006, "census_boundaries": "dissem_area_2006"},
            2016: {"data": income_2016, "census_boundaries": "census_tract_2016"},
            2021: {"data": income_2021, "census_boundaries": "census_tract_2021"},
        }

        self.housing_data = {
            2006: {"data": housing_2006, "census_boundaries": "dissem_area_2006"},
            2016: {
                "data": housing_2016,
                "census_boundaries": "census_subdivision_2016",
            },
            2021: {
                "data": housing_2021,
                "census_boundaries": "census_subdivision_2021",
            },
        }

        self.landuse_data = {2016: {"data": landuse_2016, "census_boundaries": None}}

        self.data_dict = {
            "age": self.age_data,
            "income": self.income_data,
            "housing": self.housing_data,
            "landuse": self.landuse_data,
            "population_density": self.age_data,
        }

    def join_count_station_with_census(self, data_name):
        data = self.data_dict[data_name]
        # join age data with census boundaries and join with count station
        count_census_list = []
        for year in tqdm(data.keys(), desc="Joining count station with census"):
            # extract census boundaries and convert census_boundaries["census_id"] to object
            census_boundaries = self.census_boundaries[data[year]["census_boundaries"]]
            census_boundaries["data"][census_boundaries["census_id"]] = (
                census_boundaries["data"][census_boundaries["census_id"]].astype(str)
            )
            # extract census data and convert census_data["census_id"] to object
            census_data = data[year]["data"]
            census_data["census_id"] = census_data["census_id"].astype(str)

            # left join and drop empty geometries
            census_data_gdf_year = pd.merge(
                census_data,
                census_boundaries["data"][[census_boundaries["census_id"], "geometry"]],
                how="left",
                left_on="census_id",
                right_on=census_boundaries["census_id"],
            ).dropna(subset=["geometry"])

            # convert to geo dataframe
            census_data_gdf_year = gpd.GeoDataFrame(
                census_data_gdf_year, geometry=census_data_gdf_year.geometry
            ).set_crs(census_boundaries["data"].crs)
            # spatial join count_station with census
            # reproject both to UTM by estimating the UTM zone
            utm_crs = self.count_station_year_month.estimate_utm_crs()
            count_station_year_month_utm = self.count_station_year_month.to_crs(utm_crs)
            census_data_gdf_year_utm = census_data_gdf_year.to_crs(utm_crs)
            # calculate area of census boundaries if data_name contains "density"
            if "density" in data_name:
                census_data_gdf_year_utm["area"] = (
                    census_data_gdf_year_utm["geometry"].area / 10**6
                )
            count_census_year = gpd.sjoin_nearest(
                count_station_year_month_utm,
                census_data_gdf_year_utm,
                how="left",
                max_distance=10000,
                lsuffix="count",
                rsuffix="census",
            ).reset_index()
            # drop columns that are not needed
            count_census_year = count_census_year.drop(
                columns=[
                    "count",
                    "latitude",
                    "longitude",
                    "index_census",
                    "geometry",
                    "census_id",
                    census_boundaries["census_id"],
                ]
            )
            count_census_list.append(count_census_year)

        # merge all census data
        count_census = pd.concat(count_census_list)

        # interpolate the missing years for each unique "count_point_id"
        interpolated_dataframes = []
        for count_point_id in tqdm(
            count_census["count_point_id"].unique(), desc="Interpolating missing years"
        ):
            df_subset = count_census[count_census["count_point_id"] == count_point_id]
            missing_years = list(
                set(df_subset["year_count"].unique()) - set(data.keys())
            )
            if len(missing_years) > 0:
                interpolated_df = self.interpolate_missing_years(
                    df_subset, missing_years
                )
                interpolated_dataframes.append(interpolated_df)

        count_census = pd.concat([count_census] + interpolated_dataframes)
        # only keep rows where "year_count" and "year_census" are the same
        # make sure "year_count" and "year_census" are the same type (int)
        # drop NaN values in "year_census" and "year_count"
        count_census = count_census.dropna(subset=["year_census", "year_count"])
        count_census["year_count"] = count_census["year_count"].astype(int)
        count_census["year_census"] = count_census["year_census"].astype(int)
        count_census = count_census[
            count_census["year_count"] == count_census["year_census"]
        ]
        # rename "year_count" to "year"
        count_census = count_census.rename(columns={"year_count": "year"})
        count_census = count_census.drop(columns=["index", "year_census"])

        return count_census

    def clean_count_station_data(self):
        # check if the csv file exists
        if (
            os.path.exists(str(Path(self.dir_input) / "count_station_clean.csv"))
            and os.path.exists(
                str(Path(self.dir_output) / "count_station_year_month.csv")
            )
            and os.path.exists(str(Path(self.dir_input) / "count_station_old_new.csv"))
        ):
            self.count_station = pd.read_csv(
                str(Path(self.dir_input) / "count_station_clean.csv")
            )
            self.count_station_year_month = pd.read_csv(
                str(Path(self.dir_output) / "count_station_year_month.csv")
            )
            # join count_station_year_month with count_station
            self.count_station_year_month = self.count_station_year_month.merge(
                self.count_station[["count_point_id", "latitude", "longitude"]],
                on="count_point_id",
                how="left",
            )
            # convert count_station_year_month to geodataframe
            self.count_station_year_month = gpd.GeoDataFrame(
                self.count_station_year_month,
                geometry=gpd.points_from_xy(
                    self.count_station_year_month.longitude,
                    self.count_station_year_month.latitude,
                ),
            ).set_crs(epsg=4326)
            self.count_station_year_month_min = min(
                self.count_station_year_month["year"]
            )
            self.count_station_year_month_max = max(
                self.count_station_year_month["year"]
            )
            return
        # filter "Description_Code_Banque" to only keep "Pietons"
        self.count_df = self.count_df[
            self.count_df["Description_Code_Banque"] == "Pietons"
        ]
        # rename columns
        self.count_df = self.count_df.rename(
            columns={
                "Id_Reference": "count_point_id_old",
                "Latitude": "latitude",
                "Longitude": "longitude",
            }
        )
        # get year and month from "Date"
        self.count_df["Date"] = pd.to_datetime(self.count_df["Date"])
        self.count_df["year"] = self.count_df["Date"].dt.year
        self.count_df["month"] = self.count_df["Date"].dt.month

        # save df.count_station: count_point_id, latitude, longitude
        self.count_station = self.count_df.drop_duplicates(
            subset=["count_point_id_old"]
        )[["count_point_id_old", "latitude", "longitude"]]
        # get unique pairs of latitude and longitude
        count_station_new = self.count_station.drop_duplicates(
            subset=["latitude", "longitude"]
        )
        # create a new id column
        count_station_new["count_point_id"] = np.arange(len(count_station_new))
        # save to csv
        count_station_new[["count_point_id", "latitude", "longitude"]].to_csv(
            Path(self.dir_input) / "count_station_clean.csv", index=False
        )
        # merge the two dataframes
        self.count_station = count_station_new[
            ["count_point_id", "latitude", "longitude"]
        ].merge(self.count_station, on=["latitude", "longitude"], how="left")
        # merge with count_station_year_month
        self.count_df = self.count_df.merge(
            self.count_station[
                ["count_point_id_old", "count_point_id", "latitude", "longitude"]
            ],
            on="count_point_id_old",
            how="left",
        )
        # group by count_point_id and year and month
        # aggregate counts (Approche_Nord, Approche_Sud, Approche_Est, Approche_Ouest) by count_point_id and year
        self.count_df["count"] = self.count_df[
            ["Approche_Nord", "Approche_Sud", "Approche_Est", "Approche_Ouest"]
        ].sum(axis=1)
        self.count_station_year_month = (
            self.count_df.groupby(["count_point_id", "year", "month"], as_index=False)
            .agg({"count": "mean"})
            .reset_index()
        )
        # current count column = count in 15 mins -> multiply by 4 to get count in 1 hour -> multiply by 12 to get count in daytime on a day (12 hours)
        self.count_station_year_month["count"] = (
            self.count_station_year_month["count"] * 4 * 12
        )
        self.count_station_year_month[
            ["count_point_id", "year", "month", "count"]
        ].to_csv(Path(self.dir_output) / "count_station_year_month.csv", index=False)
        # join count_station_year_month with count_station
        self.count_station_year_month = self.count_station_year_month.merge(
            count_station_new[["count_point_id", "latitude", "longitude"]],
            on="count_point_id",
            how="left",
        )
        # convert count_station_year_month to geodataframe
        self.count_station_year_month = gpd.GeoDataFrame(
            self.count_station_year_month,
            geometry=gpd.points_from_xy(
                self.count_station_year_month.longitude,
                self.count_station_year_month.latitude,
            ),
        ).set_crs(epsg=4326)
        self.count_station_year_month_min = min(self.count_station_year_month["year"])
        self.count_station_year_month_max = max(self.count_station_year_month["year"])

    def interpolate_missing_years(self, df, missing_years):
        df = df.copy()
        # drop NA
        df = df.dropna(subset=["year_census"])
        df["year_census"] = df["year_census"].astype(int).astype(str)
        df.sort_values("year_census", inplace=True)
        years = df["year_census"].unique()
        start_year = min(self.count_station_year_month_min, int(min(years)))
        end_year = max(self.count_station_year_month_max, int(max(years)))
        years = pd.date_range(start=str(start_year), end=str(end_year), freq="YS")
        df_fill = pd.DataFrame({"year_census": years})
        # extract year from "year" column
        df_fill["year_census"] = df_fill["year_census"].dt.year
        df_fill = df_fill.astype({"year_census": "str"})

        # Only keep the years in df_fill that are not in df's 'year_census'
        df_fill = df_fill[~df_fill["year_census"].isin(df["year_census"])]
        df = pd.concat([df_fill, df])

        # make sure to fill "count_point_id" with the same value
        df["count_point_id"] = df["count_point_id"].fillna(method="bfill")

        df.sort_values("year_census", inplace=True)
        df = df.interpolate(method="linear")

        # get data for missing_years from df
        missing_years = [str(year) for year in missing_years]
        return df[df["year_census"].isin(missing_years)]

    def clean_age_data(self):
        if (Path(self.dir_output) / "count_age.csv").exists():
            self.count_age = pd.read_csv(Path(self.dir_output) / "count_age.csv")
            return

        # join count_station_year_month with count_station
        self.count_age = self.join_count_station_with_census("age")
        # divide "age_" columns by "total" and multiply by 100
        for col in self.count_age.columns:
            if "age_" in col:
                self.count_age[col] = (
                    self.count_age[col] / self.count_age["total"] * 100
                )

        # drop "total" column
        self.count_age = self.count_age.drop(columns=["total"])

        # save to csv
        self.count_age.to_csv(Path(self.dir_output) / "count_age.csv", index=False)

    def clean_population_density_data(self):
        if (Path(self.dir_output) / "count_population_density.csv").exists():
            self.count_population_density = pd.read_csv(
                Path(self.dir_output) / "count_population_density.csv"
            )
            return

        # join count_station_year_month with count_station
        self.count_population_density = self.join_count_station_with_census(
            "population_density"
        )

        # calculate population density
        self.count_population_density["pop_den"] = (
            self.count_population_density["total"]
            / self.count_population_density["area"]
        )
        self.count_population_density = self.count_population_density[
            ["count_point_id", "year", "pop_den"]
        ]

        # save to csv
        self.count_population_density.to_csv(
            Path(self.dir_output) / "count_population_density.csv", index=False
        )

    def clean_wealth_data(self):
        if (Path(self.dir_output) / "count_income.csv").exists():
            self.count_income = pd.read_csv(Path(self.dir_output) / "count_income.csv")
            return

        # join count_station_year_month with count_station
        self.count_income = self.join_count_station_with_census("income")

        # save to csv
        self.count_income.to_csv(
            Path(self.dir_output) / "count_income.csv", index=False
        )
        pass

    def clean_housing_data(self):
        if (Path(self.dir_output) / "count_housing.csv").exists():
            self.count_housing = pd.read_csv(
                Path(self.dir_output) / "count_housing.csv"
            )
            return

        # join count_station_year_month with count_station
        self.count_housing = self.join_count_station_with_census("housing")

        # save to csv
        self.count_housing.to_csv(
            Path(self.dir_output) / "count_housing.csv", index=False
        )
        pass

    def clean_landuse_data(self):
        if (Path(self.dir_output) / "count_land_use.csv").exists():
            self.count_land_use = pd.read_csv(
                Path(self.dir_output) / "count_land_use.csv"
            )
            return

        # join count_station_year_month with count_station
        # take 500m from the count station
        utm_crs = self.count_station_year_month.estimate_utm_crs()
        count_station_year_month_utm = self.count_station_year_month.to_crs(utm_crs)
        count_station_year_month_utm["geometry"] = count_station_year_month_utm.buffer(
            500
        )

        # convert landuse_2016 to UTM
        land_use = self.landuse_data[2016]["data"].to_crs(utm_crs)
        # calculate area of each land use type
        land_use["area"] = land_use["geometry"].area
        # rename land use types
        land_use["land_use"] = land_use["AFFECTATIO"].replace(
            {
                "Conservation": "lu_others",
                "Dominante résidentielle": "lu_residential_community",
                "Industrie": "lu_commerce_developped",
                "Grande emprise ou grande infrastructure publique": "lu_commerce_developped",
                "Activités diversifiées": "lu_commerce_developped",
                "Agricole": "lu_others",
                "Grand espace vert ou récréation": "lu_others",
                "Centre-ville d'agglomération": "lu_commerce_developped",
            }
        )

        # spatial join count_station_year_month_utm with landuse_2016
        self.count_land_use = gpd.sjoin(
            count_station_year_month_utm, land_use, how="left", op="intersects"
        ).reset_index()

        # group by count_point_id, year, and land_use
        self.count_land_use = self.count_land_use.groupby(
            ["count_point_id", "year", "land_use"], as_index=False
        ).agg({"area": "sum"})

        # pivot table
        self.count_land_use = self.count_land_use.pivot_table(
            index=["count_point_id", "year"], columns="land_use", values="area"
        ).reset_index()

        # fill NaN with 0
        self.count_land_use = self.count_land_use.fillna(0)

        # calculate total area and divide land use area by total area and multiply by 100
        self.count_land_use["total_area"] = self.count_land_use.iloc[:, 2:].sum(axis=1)
        self.count_land_use.iloc[:, 2:] = (
            self.count_land_use.iloc[:, 2:].div(
                self.count_land_use["total_area"], axis=0
            )
            * 100
        )
        self.count_land_use = self.count_land_use.drop(columns="total_area")

        # save to csv
        self.count_land_use.to_csv(
            Path(self.dir_output) / "count_land_use.csv", index=False
        )
        pass

    def clean_poi_data(self):
        if (Path(self.dir_output) / "count_poi.csv").exists():
            self.count_land_use = pd.read_csv(Path(self.dir_output) / "count_poi.csv")
            return

        @retry(stop=stop_after_attempt(3))
        def fetch_poi_data(client, bcircles, year):
            return client.elements.count.post(
                bcircles=bcircles,
                time=f"{str(year)}-01-01",
                filter="amenity=* and type:node",
            )

        def fetch_poi(row, buffer, client):
            result = {}
            try:
                response = fetch_poi_data(
                    client, [row.longitude, row.latitude, buffer], row.year
                )
                response_df = response.as_dataframe().reset_index()
                poi_num = response_df["value"][0]
                # add count_point_id and year to result
                result["count_point_id"] = row.count_point_id
                result["year"] = row.year
                result["poi"] = poi_num
            except Exception as e:
                print(e)
            return result

        client = OhsomeClient()
        print("Ohsome client has been initialized")
        print(
            f"Earliest date available is {client.start_timestamp} and latest is {client.end_timestamp}"
        )
        tqdm.pandas()

        results = {}
        # Using ThreadPoolExecutor to parallelize the process
        with ThreadPoolExecutor() as executor:
            futures = {
                executor.submit(fetch_poi, row, 500, client): row
                for row in self.count_station_year_month.itertuples()
            }
            for future in tqdm(as_completed(futures), total=len(futures)):
                row = futures[future]
                try:
                    data = future.result()
                    results[row.Index] = data
                except Exception as exc:
                    print(f"Generated an exception: {exc}")

        # Convert results to DataFrame and save as csv
        self.count_poi = pd.DataFrame.from_dict(results, orient="index")
        # save to csv
        self.count_poi.to_csv(Path(self.dir_output) / "count_poi.csv", index=False)

    def clean_slope_data(self):
        if (Path(self.dir_output) / "count_slope.csv").exists():
            self.count_land_use = pd.read_csv(Path(self.dir_output) / "count_slope.csv")
            return

        # create geopandas GeoDataFrame
        count_slope = self.count_station_year_month.to_crs("EPSG:3857")
        count_slope["geometry"] = count_slope.geometry.buffer(500)
        with rasterio.open(self.slope_path) as src:
            crs = src.crs
        count_slope = count_slope.to_crs(crs)

        # Compute zonal statistics for each polygon
        stats = zonal_stats(count_slope, self.slope_path, stats=["mean"])

        # The result is a list of dictionaries. Convert it to a DataFrame
        stats_df = pd.DataFrame(stats)

        # Add the mean slope to the GeoDataFrame
        count_slope["slope"] = stats_df["mean"]

        # drop unnecessary columns
        count_slope = count_slope.drop(
            columns=["count", "latitude", "longitude", "geometry"]
        )

        # save to csv
        count_slope.to_csv(Path(self.dir_output) / "count_slope.csv", index=False)
        pass

    def clean_all(self):
        super().clean_all()


if __name__ == "__main__":
    # load data
    dir_input = "/Volumes/ExFAT2/bike_svi/data/external/cities/Montreal"
    dir_output = "/Volumes/ExFAT2/bike_svi/data/interim/cities/Montreal"
    cleaner = MontrealDataCleaner(dir_input, dir_output)
    cleaner.clean_all()
