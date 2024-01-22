pacman::p_load(tidyverse, sf, raster, osmdata, dotenv, gstat)

# define parameters -----------------------------------------------------------
load_dot_env()
root_dir <- Sys.getenv("ROOT_DIR")
if (!file.exists(root_dir)) {
  root_dir <- here()
}
external_dir <- file.path(root_dir, "data/external/cities/London")
raw_dir <- file.path(root_dir, "data/raw/cities/London")
# center of london
center <- c(-0.1278, 51.5074)
# 5km buffer from the center
# convert center to sf
center_buffer <- st_as_sf(data.frame(lon = center[1], lat = center[2]), coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(3857) %>% 
    st_buffer(5000) %>% 
    st_transform(4326)

# slope map in london
slope <- raster::raster(paste0(external_dir, "/gis_variables/slope/LIDAR_10m_DTM_Composite_2019/LIDAR_10m_DTM_Composite.tif")) 
# reproject center buffer to the same projection as slope
center_buffer_projected <- center_buffer %>%
    st_transform(st_crs(slope))
slope <- slope %>%
    raster::crop(., center_buffer_projected) %>%
    raster::mask(., center_buffer_projected) %>% 
    raster::rasterToPolygons() %>%
    st_as_sf()
census <- read_sf(paste0(external_dir, "/control_variables/statistical-gis-boundaries-london/ESRI/LSOA_2011_London_gen_MHW.shp")) %>% 
    st_transform(4326) %>% 
    st_make_valid() %>% 
    st_intersection(center_buffer)
street <- st_bbox(center_buffer) %>%
    opq() %>%
    add_osm_feature(key = "highway") %>%
    osmdata_sf() %>%
    magrittr::extract2("osm_lines") %>% 
    dplyr::select(osm_id, name, geometry) %>%
    st_transform(4326) %>%
    # crop by the buffer
    st_intersection(center_buffer)
gsv_points <- read_csv(paste0(raw_dir, "/gsv_pids.csv")) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    st_intersection(center_buffer)
# download point of interest from osmdata
poi_points <- st_bbox(center_buffer) %>% 
    opq() %>% 
    add_osm_feature(key = "amenity") %>% 
    osmdata_sf() %>% 
    magrittr::extract2("osm_points") %>% 
    dplyr::select(osm_id, name, geometry) %>% 
    st_transform(4326) %>% 
    st_intersection(center_buffer)
# create 100m grid for the center buffer
grid <- center_buffer %>%
    st_transform(3857) %>%
    st_make_grid(cellsize = 100, what = "polygons", square = FALSE) %>% 
    st_transform(4326) %>%
    st_sf() %>% 
    st_intersection(center_buffer) %>% 
    st_transform(3857)
    
generate_heatmap_values <- function(grid, n_clusters = 3, seed = 123) {
  # Set seed for reproducibility
  set.seed(seed)
  
  # Get the centroids of the grid cells
  centroids <- st_centroid(grid)
  print(centroids)
  # Select random centroids to be cluster centers
  cluster_centers <- centroids[sample(1:nrow(centroids), n_clusters), ]
  
  # Generate clustered random values around the selected centroids
  random_values <- data.frame()
  
  for(i in 1:nrow(cluster_centers)) {
    cluster_center <- cluster_centers[i, ]
    n <- sample(10:50, 1)  # Randomly choose the number of points in the cluster
    
    # Generate random points around the cluster center
    x <- st_coordinates(cluster_center)[1] + runif(n, -200, 200)
    y <- st_coordinates(cluster_center)[2] + runif(n, -200, 200)
    
    # Generate random values for the heatmap
    values <- runif(n, 0, 100)
    
    random_values <- rbind(random_values, data.frame(x, y, values))
  }
  
  # Convert to an sf object
  random_values_sf <- st_as_sf(random_values, coords = c("x", "y"), crs = st_crs(grid))
  
  # Convert to SpatialPointsDataFrame for IDW interpolation
  random_values_sp <- as(random_values_sf, "Spatial")
  
  # Perform IDW interpolation
  idw_result <- idw(values ~ 1, random_values_sp, newdata = as(grid, "Spatial"), idp = 0.5)
  
  # Extract interpolated values
  interpolated_values <- as.data.frame(idw_result)[, "var1.pred"]
  
  # Assign interpolated values to the grid
  grid$value <- interpolated_values
  
  return(grid)
}

# Generate heatmap values and update the grid
updated_grid <- generate_heatmap_values(grid, n_clusters = 100) %>% 
    st_transform(4326) %>% 
    # intersect with census
    st_intersection(census)

# plot -----------------------------------------------------------------------
# set up color and breaks
my_palette <- colorRampPalette(c("#62428b", "#FFFFFF", "#5d9242"))

# Create the color scale
color_scale <- my_palette(100)
# create separate maps with theme_minimal
map_slope <- ggplot(slope) +
    geom_sf(aes(fill = LIDAR_10m_DTM_Composite), color = NA) +
    # use red to blue color
    scale_fill_gradientn(colours = color_scale) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank())
# save the map
ggsave("./reports/figures/slope_map.png", map_slope, width = 6, height = 6)

# map census with POPDEN as fill
map_census <- ggplot(census) +
    geom_sf(aes(fill = POPDEN), color = NA) +
    # use red to blue color
    scale_fill_gradientn(colours = color_scale) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank())
# save the map
ggsave("./reports/figures/census_map.png", map_census, width = 6, height = 6)

# plot street and gsv points (#5d9242)
map_street <- ggplot(street) +
    geom_sf(color = "black", linewidth = 0.2) +
    geom_sf(data = gsv_points, color = "#5d9242", size = 0.1, alpha = 0.5) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank())
# save the map
ggsave("./reports/figures/street_map.png", map_street, width = 6, height = 6)

# plot street network and poi points (#62428b)
map_poi <- ggplot(street) +
    geom_sf(color = "black", linewidth = 0.1) +
    geom_sf(data = poi_points, color = "#62428b", size = 0.1, alpha = 0.5) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank())
# save the map
ggsave("./reports/figures/poi_map.png", map_poi, width = 6, height = 6)

# map grid with random values as fill
map_grid <- ggplot(updated_grid) +
    geom_sf(aes(fill = value), color = NA) +
    # use red to blue color
    scale_fill_gradientn(colours = color_scale) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank())
# save the map
ggsave("./reports/figures/grid_map.png", map_grid, width = 6, height = 6)
