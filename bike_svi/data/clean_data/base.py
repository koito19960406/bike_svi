from pathlib import Path

from abc import ABC, abstractmethod

class BaseDataCleaner(ABC):
    def __init__(self, dir_input, dir_output):
        self.dir_input = dir_input
        self.dir_output = dir_output
        Path(self.dir_output).mkdir(parents=True, exist_ok=True)

    @abstractmethod
    def load_data(self):
        pass

    @abstractmethod
    def clean_count_station_data(self):
        """
        create count_station: count_point_id, latitude, longitude
        create count_station_year: count_point_id, year, count
        """
        pass
    
    @abstractmethod
    def join_count_station_with_census(self, count_station, census):
        """
        join count_station with census
        """
        pass

    @abstractmethod
    def clean_age_data(self):
        """ 
        create age: count_point_id, year, age_0_19, age_20_39, age_40_59, age_60_90 (above 90)
        """
        pass

    @abstractmethod
    def clean_population_density_data(self):
        """
        create population_density: count_point_id, year, population_density
        """
        pass
    
    @abstractmethod
    def clean_wealth_data(self):
        pass
    
    @abstractmethod
    def clean_housing_data(self):
        pass

    @abstractmethod
    def clean_landuse_data(self):
        pass

    @abstractmethod
    def clean_poi_data(self):
        pass
    
    @abstractmethod
    def clean_slope_data(self):
        pass
    
    def clean_all(self):
        self.load_data()
        self.clean_count_station_data()
        self.clean_age_data()
        self.clean_population_density_data()
        self.clean_wealth_data()
        self.clean_housing_data()
        self.clean_landuse_data()
        self.clean_poi_data()
        self.clean_slope_data()