import osmnx as ox
import geopandas as gpd
import pandas as pd
import networkx as nx
from shapely.geometry import Point

import osmnx as ox
import geopandas as gpd
import pandas as pd
import networkx as nx
from shapely.geometry import Point


def get_street_network_centrality(location, network_type="drive"):
    """
    Download OSM street network, calculate centrality indicators, and include speed limits.

    Parameters:
    location : str or tuple
        City name or bounding box coordinates (north, south, east, west)
    network_type : str, optional
        Type of street network to download (default is 'drive')

    Returns:
    GeoDataFrame with centrality indicators and speed limits as columns
    """
    ox.config(log_console=True)
    # Download the street network
    if isinstance(location, str):
        G = ox.graph_from_place(location, network_type=network_type, simplify=True)
    else:
        G = ox.graph_from_bbox(*location, network_type=network_type, simplify=True)

    # Add edge speeds
    G = ox.add_edge_speeds(G)

    # Convert multigraph to simple graph
    G_simple = nx.Graph(G)

    # Convert to GeoDataFrame
    gdf_nodes, gdf_edges = ox.graph_to_gdfs(G)

    # Calculate centrality measures
    centrality_measures = {
        "degree": nx.degree_centrality(G_simple),
        "betweenness": nx.betweenness_centrality(G_simple),
        "closeness": nx.closeness_centrality(G_simple),
    }

    # Try to calculate eigenvector centrality, but don't fail if it doesn't converge
    try:
        centrality_measures["eigenvector"] = nx.eigenvector_centrality(
            G_simple, max_iter=1000
        )
    except nx.PowerIterationFailedConvergence:
        print(
            "Eigenvector centrality calculation did not converge. Skipping this measure."
        )

    # Add centrality measures to GeoDataFrame
    for measure, values in centrality_measures.items():
        gdf_nodes[f"{measure}_centrality"] = gdf_nodes.index.map(values)

    # Calculate average speed limit for each node
    speed_limits = {}
    for node, edges in G.adj.items():
        speeds = [e.get("speed_kph", 0) for e in edges.values()]
        speed_limits[node] = sum(speeds) / len(speeds) if speeds else 0

    gdf_nodes["avg_speed_limit_kph"] = gdf_nodes.index.map(speed_limits)

    return gdf_nodes


def nearest_join_centrality(df, gdf_network, lat_col="latitude", lon_col="longitude"):
    """
    Perform nearest join between a DataFrame with lat/lon and a network GeoDataFrame.

    Parameters:
    df : pandas DataFrame
        Input DataFrame with latitude and longitude columns
    gdf_network : GeoDataFrame
        Network GeoDataFrame with centrality indicators and speed limits
    lat_col : str, optional
        Name of the latitude column in df (default is 'latitude')
    lon_col : str, optional
        Name of the longitude column in df (default is 'longitude')

    Returns:
    pandas DataFrame with original columns, centrality indicator columns, and speed limit
    """
    # Convert DataFrame to GeoDataFrame
    gdf = gpd.GeoDataFrame(
        df, geometry=gpd.points_from_xy(df[lon_col], df[lat_col]), crs="EPSG:4326"
    )

    # Ensure both GeoDataFrames have the same CRS
    gdf_network = gdf_network.to_crs(gdf.crs)

    # Perform spatial join
    joined = gpd.sjoin_nearest(gdf, gdf_network)

    # Drop unnecessary columns and reset index
    result = joined.drop(columns=["geometry", "index_right"]).reset_index(drop=True)

    return result


import osmnx as ox
import pandas as pd
import geopandas as gpd
from shapely.geometry import Point, Polygon
import networkx as nx
from tqdm import tqdm
from concurrent.futures import ProcessPoolExecutor, as_completed
from ohsome import OhsomeClient
from datetime import datetime
import pyproj
from shapely.ops import transform

from concurrent.futures import ThreadPoolExecutor


def fetch_pois_for_location(args):
    client, lat, lon, year = args
    buffer = create_buffer(lat, lon, 1000)
    buffer_gdf = gpd.GeoDataFrame({"geometry": [buffer]}, crs="EPSG:4326")
    timestamp = f"{int(year)}-01-01"
    tags = "amenity=* or shop=* or leisure=* or tourism=*"
    pois = get_historical_pois(client, buffer_gdf, timestamp, tags)
    return (lat, lon, year), pois


def get_all_pois(df, max_workers=10):
    client = OhsomeClient()
    unique_locations = df[["latitude", "longitude", "year"]].drop_duplicates()
    all_pois = {}

    with ThreadPoolExecutor(max_workers=max_workers) as executor:
        futures = [
            executor.submit(
                fetch_pois_for_location,
                (client, row["latitude"], row["longitude"], row["year"]),
            )
            for _, row in unique_locations.iterrows()
        ]

        for future in tqdm(
            as_completed(futures), total=len(unique_locations), desc="Fetching POIs"
        ):
            key, pois = future.result()
            all_pois[key] = pois

    return all_pois


def create_buffer(lat, lon, distance):
    point = Point(lon, lat)
    local_azimuthal_projection = (
        f"+proj=aeqd +R=6371000 +units=m +lat_0={lat} +lon_0={lon}"
    )
    wgs84_to_aeqd = pyproj.Transformer.from_proj(
        pyproj.Proj("+proj=longlat +datum=WGS84 +no_defs"),
        pyproj.Proj(local_azimuthal_projection),
        always_xy=True,
    )
    aeqd_to_wgs84 = pyproj.Transformer.from_proj(
        pyproj.Proj(local_azimuthal_projection),
        pyproj.Proj("+proj=longlat +datum=WGS84 +no_defs"),
        always_xy=True,
    )
    point_aeqd = wgs84_to_aeqd.transform(lon, lat)
    buffer_aeqd = Point(point_aeqd).buffer(distance)
    buffer_wgs84 = transform(aeqd_to_wgs84.transform, buffer_aeqd)
    return buffer_wgs84


def get_historical_pois(client, buffer_gdf, timestamp, tags):
    return client.elements.geometry.post(
        bpolys=buffer_gdf, time=timestamp, filter=tags
    ).as_dataframe()


def process_row(row_data, all_pois):
    row_index, lat, lon, year = row_data
    try:
        point = Point(lon, lat)

        # Get the street network
        G = ox.graph_from_point((lat, lon), dist=1000, network_type="all")
        G_proj = ox.project_graph(G)

        # Add edge speeds
        G_proj = ox.add_edge_speeds(G_proj)
        G_proj = ox.add_edge_travel_times(G_proj)

        G_simple = nx.Graph(G_proj)

        # Get pre-fetched POIs
        pois = all_pois[(lat, lon, year)]

        # Filter out non-Point geometries
        pois = pois[pois.geometry.type == "Point"]

        # Project POIs to match the graph projection
        pois_proj = pois.to_crs(G_proj.graph["crs"])

        # Find the nearest node for each POI
        poi_nodes = ox.nearest_nodes(G_proj, pois_proj.geometry.x, pois_proj.geometry.y)

        # Find the nearest edge and node
        nearest_edge = ox.nearest_edges(G_proj, point.x, point.y)
        nearest_node = nearest_edge[0]

        # Calculate POI-specific measures
        if poi_nodes:
            poi_betweenness = nx.betweenness_centrality_subset(
                G_simple, sources=poi_nodes, targets=poi_nodes
            )
        else:
            poi_betweenness = {node: 0 for node in G_simple.nodes()}

        # Calculate accessibility to POIs
        accessibility = sum(
            1 / (1 + nx.shortest_path_length(G_simple, nearest_node, poi_node))
            for poi_node in poi_nodes
        )

        # Get the speed of the nearest edge
        edge_speed = G_proj.edges[nearest_edge]["speed_kph"]
        print(poi_betweenness[nearest_node], accessibility, edge_speed)
        return {
            "index": row_index,
            "poi_betweenness": poi_betweenness[nearest_node],
            "poi_accessibility": accessibility,
            "traffic_speed": edge_speed,
        }

    except Exception as e:
        print(f"Error processing row {row_index}: {e}")
        return {
            "index": row_index,
            "poi_betweenness": None,
            "poi_accessibility": None,
            "traffic_speed": None,
        }


def calculate_centralities(df, max_workers=16):
    # Get all POIs before starting multiprocessing
    all_pois = get_all_pois(df, max_workers)

    results = []

    # Prepare data for multiprocessing
    row_data = [
        (index, row["latitude"], row["longitude"], row["year"])
        for index, row in df.iterrows()
    ]

    # Use ProcessPoolExecutor to process rows in parallel
    with ProcessPoolExecutor(max_workers=max_workers) as executor:
        # Submit all tasks
        futures = [executor.submit(process_row, data, all_pois) for data in row_data]

        # Process results as they complete
        for future in tqdm(
            as_completed(futures), total=len(df), desc="Processing rows"
        ):
            results.append(future.result())

    # Create a new dataframe from the results
    result_df = pd.DataFrame(results).set_index("index")

    # Merge with original dataframe
    final_df = df.join(result_df)

    return final_df
