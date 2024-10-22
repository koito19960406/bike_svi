import matplotlib.colors as mc  # For the legend
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import os
import dotenv
import itertools
from matplotlib.cm import ScalarMappable
import geopandas as gpd


# reference: https://www.python-graph-gallery.com/heatmap-for-timeseries-matplotlib
class DataVizExpolorer:
    def __init__(self, input_folder, output_folder, variable_to_plot):
        self.input_folder = input_folder
        self.output_folder = output_folder
        self.variable_to_plot = variable_to_plot
        self.count_df = pd.read_csv(
            os.path.join(
                self.input_folder, "processed/cities/London/all_var_joined.csv"
            )
        )
        self.count_station = pd.read_csv(
            os.path.join(self.input_folder, "external/cities/London/count_station.csv")
        )
        self.borough_gdf = gpd.read_file(
            os.path.join(
                self.input_folder,
                "external/cities/London/control_variables/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp",
            )
        ).to_crs(4326)

    def spatial_join(self):
        """function to aggreaget to borough level"""
        # join count_df and count_station
        self.count_gdf = pd.merge(
            self.count_df, self.count_station, on="count_point_id", how="left"
        )
        self.count_gdf = gpd.GeoDataFrame(
            self.count_gdf,
            geometry=gpd.points_from_xy(
                self.count_gdf.longitude, self.count_gdf.latitude
            ),
        )
        # spatial join
        self.borough_count_gdf = gpd.sjoin(self.count_gdf, self.borough_gdf, how="left")

    def prep_data(self):
        """function to clean data"""
        # run spatial join
        self.spatial_join()
        # aggregate by borough
        self.borough_count_df = pd.DataFrame(
            self.borough_count_gdf.drop(columns="geometry")
        )
        self.borough_count_df = (
            self.borough_count_df.groupby(["NAME", "year"])[self.variable_to_plot]
            .agg("mean")
            .reset_index()
        )
        self.borough_count_df = (
            self.borough_count_df.set_index(["NAME", "year"])
            .reindex(
                pd.MultiIndex.from_tuples(
                    itertools.product(
                        self.borough_count_df.NAME.unique(),
                        self.borough_count_df.year.unique(),
                    )
                )
            )
            .reset_index()
            .rename(columns={"level_0": "borough_name", "level_1": "year"})
        )

    def plot(self):
        # prepare data to get agregated data at borough level
        self.prep_data()
        variable_to_plot = self.borough_count_df[self.variable_to_plot]
        borough_name = self.borough_count_df["borough_name"]
        year = self.borough_count_df["year"]
        variable_to_plot = variable_to_plot.values.reshape(
            len(borough_name.unique()), len(year.unique()), order="F"
        )

        # set up grid
        fig, ax = plt.subplots()
        plt.gcf().set_size_inches(10, 10)

        MIN = np.nanmin(variable_to_plot)
        MAX = np.nanmax(variable_to_plot)

        xgrid = np.arange(min(year.unique()), max(year.unique()) + 2)
        ygrid = np.arange(len(borough_name.unique()) + 1)

        ax.pcolormesh(xgrid, ygrid, variable_to_plot, cmap="magma", vmin=MIN, vmax=MAX)
        # Set tick positions for both axes
        ax.yaxis.set_ticks([i for i in range(len(borough_name.unique()))])
        # ax.xaxis.set_ticks([10, 20, 30])
        # Remove ticks by setting their length to 0
        ax.yaxis.set_tick_params(length=0)
        ax.yaxis.set_ticklabels(borough_name.unique(), size=5)
        ax.xaxis.set_tick_params(length=0)
        ax.set_frame_on(False)  # remove all spines
        # add legend
        # First, let's make some room for the legend in the bottom.
        fig.subplots_adjust(bottom=0.15)

        # Create a new axis to contain the color bar
        # Values are:
        # (x coordinate of left border,
        #  y coordinate for bottom border,
        #  width,
        #  height)
        cbar_ax = fig.add_axes([0.3, 0.05, 0.4, 0.025])

        # Create a normalizer that goes from minimum to maximum temperature
        norm = mc.Normalize(MIN, MAX)
        print(MIN, MAX)
        # Create the colorbar and set it to horizontal
        cb = fig.colorbar(
            ScalarMappable(norm=norm, cmap="magma"),
            cax=cbar_ax,  # Pass the new axis
            orientation="horizontal",
        )

        # Remove tick marks
        cb.ax.xaxis.set_tick_params(size=0)

        # Set legend label
        cb.set_label(self.variable_to_plot, size=12)

        # Set common labels for x and y axes
        fig.text(0.5, 0.1, "Year", ha="center", va="center", fontsize=14)
        fig.text(
            0.02,
            0.5,
            "Boroughs",
            ha="center",
            va="center",
            rotation="vertical",
            fontsize=14,
        )

        fig.suptitle(
            f"Changes of {self.variable_to_plot} between 2008 and 2020 by boroughs",
            fontsize=20,
            y=0.95,
        )

        # save
        fig.set_facecolor("white")
        fig.savefig(
            os.path.join(self.output_folder, f"{self.variable_to_plot}_heatmap.png"),
            dpi=300,
        )
        plt.close("all")


if __name__ == "__main__":
    dotenv.load_dotenv(dotenv.find_dotenv())
    root_dir = os.getenv("ROOT_DIR")
    input_folder = os.path.join(root_dir, "data")
    output_folder = "./reports/figures"
    variable_to_plot_list = [
        "pedal_cycles",
        "vegetation",
        "sky",
        "building",
        "sidewalk",
    ]
    # variable_to_plot_list = ["sky"]

    for variable_to_plot in variable_to_plot_list:
        visualizer = DataVizExpolorer(input_folder, output_folder, variable_to_plot)
        visualizer.plot()
