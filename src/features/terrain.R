pacman::p_load(tidyverse,sf,raster,exactextractr)



# load data ---------------------------------------------------------------
root_dir <- "/Volumes/ExFAT/bike_svi"
dem <- raster(paste0(root_dir, "/data/external/cities/London/gis_variables/slope/LIDAR_10m_DTM_Composite_2019/LIDAR_10m_DTM_Composite.tif"))
count_station <- read_csv(paste0(root_dir, "/data/external/cities/London/count_station.csv")) %>% 
  st_as_sf(coords=c("longitude","latitude"), crs=4326) %>%
  st_transform(3857) %>%
  st_buffer(500)

# compute slope -----------------------------------------------------------
slope <- terrain(dem, opt='slope', unit='degrees', neighbors=8)

# zonal stats -------------------------------------------------------------
count_station <- count_station %>% 
  mutate(slope=exact_extract(slope,.,"mean"))


# save --------------------------------------------------------------------
count_station %>% 
  st_drop_geometry() %>% 
  write.csv(paste0(root_dir, "/data/processed/cities/London/slope.csv"))