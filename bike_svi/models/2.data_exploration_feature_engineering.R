pacman::p_load(
  tidyverse, Hmisc, GGally, corrplot, RColorBrewer, ggplot2,
  hrbrthemes, stargazer, plotly, sf, basemaps, magrittr, cowplot, dotenv,
  ggnewscale, here, ggspatial, lwgeom, ggimage, cropcircles, ggrepel, osmdata,
  ggridges, scales
)
extrafont::loadfonts()

# data exploration --------------------------------------------------------
load_dot_env()
root_dir <- Sys.getenv("ROOT_DIR")
if (!file.exists(root_dir)) {
  root_dir <- here()
}


# load external/city_list.txt to get the list of cities
city_list <- read.csv(paste0(root_dir, "/data/external/city_list.txt"), header = FALSE, sep = "\t") %>%
  rename(city = V1) %>%
  mutate(city = str_replace_all(city, " ", "_")) %>%
  as.vector() %>%
  unlist()


# reference: https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/#define-a-map-theme
theme_map <- function(...,
                      default_font_color = "#4e4d47",
                      default_background_color = "#f5f5f2",
                      default_font_family = "Ubuntu Regular") {
  theme_ipsum() +
    theme(
      text = element_text(
        family = default_font_family,
        color = default_font_color
      ),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # background colors
      # plot.background = element_rect(fill = default_background_color,
      #                                color = NA),
      # panel.background = element_rect(fill = default_background_color,
      #                                 color = NA),
      # legend.background = element_rect(fill = default_background_color,
      #                                  color = NA),
      # borders and margins
      plot.margin = ggplot2::margin(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = ggplot2::margin(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(
        size = 9, hjust = 0,
        color = default_font_color
      ),
      plot.title = element_text(
        size = 15, hjust = 0.5,
        color = default_font_color
      ),
      plot.subtitle = element_text(
        size = 10, hjust = 0.5,
        color = default_font_color,
        margin = ggplot2::margin(
          b = -0.1,
          t = -0.1,
          l = 2,
          r = 0,
          unit = "cm"
        ),
        debug = F
      ),
      # captions
      plot.caption = element_text(
        size = 7,
        hjust = .5,
        margin = ggplot2::margin(
          t = 0.2,
          b = 0,
          r = 0,
          unit = "cm"
        ),
        color = "#939184"
      ),
      ...
    )
}

prep_data <- function(sf, col1, col2, bivariate_color_scale) {
  # create 3 buckets for gini
  quantiles1 <- sf %>%
    pull({{ col1 }}) %>%
    quantile(probs = seq(0, 1, length.out = 4), na.rm = T)

  # create 3 buckets for mean income
  quantiles2 <- sf %>%
    pull({{ col2 }}) %>%
    quantile(probs = seq(0, 1, length.out = 4), na.rm = T)

  # create color scale that encodes two variables
  # red for gini and blue for mean income
  # the special notation with gather is due to readibility reasons
  bivariate_color_scale %<>%
    gather("group", "fill")
  # cut into groups defined above and join fill
  sf %<>%
    mutate(
      centroid = map(geometry, st_centroid),
      coords = map(centroid, st_coordinates),
      coords_x = map_dbl(coords, 1),
      coords_y = map_dbl(coords, 2)
    ) %>%
    mutate(
      quantiles1_col = cut(
        .data[[{{ col1 }}]],
        breaks = quantiles1,
        include.lowest = TRUE
      ),
      quantiles2_col = cut(
        .data[[{{ col2 }}]],
        breaks = quantiles2,
        include.lowest = TRUE
      ),
      # by pasting the factors together as numbers we match the groups defined
      # in the tibble bivariate_color_scale
      group = paste(
        as.numeric(quantiles1_col), "-",
        as.numeric(quantiles2_col)
      )
    ) %>%
    # we now join the actual hex values per "group"
    # so each municipality knows its hex value based on the his gini and avg
    # income value
    left_join(bivariate_color_scale, by = "group") %>%
    mutate(fill = replace_na(fill, "#5A5A5A")) %>%
    st_transform(3857)
  return(sf)
}

create_legend <- function(bivariate_color_scale, col1, col2, axis1 = "", axis2 = "") {
  # separate the groups
  bivariate_color_scale %<>%
    gather("group", "fill") %>%
    separate("group", into = c({{ col1 }}, {{ col2 }}), sep = " - ") %>%
    mutate(
      "{{col1}}" := as.integer(.data[[{{ col1 }}]]),
      "{{col2}}" := as.integer(.data[[{{ col2 }}]])
    )
  legend <- ggplot() +
    geom_tile(
      data = bivariate_color_scale,
      mapping = aes(
        x = .data[[{{ col1 }}]],
        y = .data[[{{ col2 }}]],
        fill = fill
      )
    ) +
    scale_fill_identity() +
    labs(
      x = axis1,
      y = axis2
    ) +
    theme_map() +
    # make font small enough
    theme(
      axis.title = element_text(size = 6)
    ) +
    # quadratic tiles
    coord_fixed()
  return(legend)
}

map_bivariate <- function(sf,
                          col1,
                          col2,
                          bivariate_color_scale,
                          map_token = "",
                          axis1 = "", axis2 = "",
                          title = "",
                          subtitle = "",
                          caption = "") {
  flush_cache()
  clean_sf <- prep_data(sf, col1, col2, bivariate_color_scale)
  legend_custom <- create_legend(bivariate_color_scale, col1, col2, axis1 = axis1, axis2 = axis2)
  map <- basemap_ggplot(st_bbox(clean_sf),
    map_service = "carto",
    map_type = "light_no_labels", map_res = 1,
    force = T
  ) +
    # color municipalities according to their combination
    geom_sf(
      data = clean_sf,
      aes(fill = fill),
      # use thin white stroke for municipalities
      color = "white",
      size = 0.1
    ) +
    # as the sf object clean_sf has a column with name "fill" that
    # contains the literal color as hex code for each municipality, we can use
    # scale_fill_identity here
    scale_fill_identity() +
    # add titles
    labs(
      x = NULL,
      y = NULL,
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    # add the theme
    theme_map()
  final_plot <- ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend_custom, 0.75, 0, 0.18, 0.18)
  ggsave(
    plot = final_plot,
    filename = paste0(figure_dir, "/map_grid.png"),
    width = 7,
    height = 7,
    units = c("in")
  )
}

bivariate_color_scale <- tibble(
  "3 - 3" = "#383b38", # high-high
  "2 - 3" = "#54436c",
  "1 - 3" = "#704b9e", # low-high
  "3 - 2" = "#516f41",
  "2 - 2" = "#797f7f", # medium-medium
  "1 - 2" = "#a18eb9",
  "3 - 1" = "#6aa64b", # high-low
  "2 - 1" = "#9fbd91",
  "1 - 1" = "#d3d3d3" # low-low
)

# loop through the cities
for (city in city_list) {
  if (city == "Montreal"){
    next
  }
  # create folders for each city
  model_dir <- paste0(root_dir, "/models/", city)
  if (!dir.exists(model_dir)) {
    dir.create(model_dir)
  }
  figure_dir <- paste0("reports/figures/", city)
  if (!dir.exists(figure_dir)) {
    dir.create(figure_dir)
  }
  external_dir <- paste0(root_dir, "/data/external/cities/", city)
  raw_dir <- paste0(root_dir, "/data/raw/cities/", city)
  interim_dir <- paste0(root_dir, "/data/interim/cities/", city)
  processed_dir <- paste0(root_dir, "/data/processed/cities/", city)
  target <- case_when(
    city == "London" ~ "cyclists",
    city == "Montreal" ~ "pedestrians"
  )
  # read in the data
  # binarize some variables: ss_sidewalk, ss_pedestrian_area, ss_bike_lane, ss_bike_rack, ss_parking
  # TODO: only keep some columns from object detection (i.e., columns that start with "od_" + vehicles + people):
  # od_person_count: od_person
  # od_bicycle: od_bicyclist, od_bicycle
  # od_vehicle_count: od_vehicle, od_bus, od_car, od_caravan, od_motorcycle, od_truck, od_other_vehicle, od_trailer, od_train, od_wheeled_slow, od_ego_vehicle
  # od_animal_count: od_bird, od_ground_animal
  all_var_raw <- read.csv(paste0(processed_dir, "/all_var_joined.csv"))
  print(all_var_raw)
  all_var_with_id <- all_var_raw %>%
    dplyr::select(-contains("binary"), -contains("count_log")) %>%
    dplyr::select(-c(
      age_60_90, lu_others,
      ss_bike_rack, ss_curb, ss_curb_cut,
      ss_pothole,
      ss_pedestrian_area, ss_bench
    ))
  print(str(all_var_with_id))

  all_var <- all_var_with_id %>%
    dplyr::select(-c(count_point_id))

  summary_stats_latetx <- all_var %>%
    mutate(year = as.numeric(year)) %>%
    drop_na() %>%
    stargazer(type = "latex", out = paste0(model_dir, "/summary_stats_latetx.tex"))
  capture.output(summary_stats_latetx, file = paste0(model_dir, "/summary_stats_latetx.txt"))

  # correlation matrix
  corrmatrix <- cor(all_var %>% dplyr::select(-c(year)), use = "complete.obs")
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # matrix of the p-value of the correlation
  p.mat <- cor.mtest(all_var %>% dplyr::select(-c(year)))
  # original color: purple(#7B52AE) and green (#74B652)
  col1 <- colorRampPalette(c("#62428b", "#FFFFFF", "#5d9242"))
  pdf(paste0(figure_dir, "/correlation_mat.pdf"), height = 8, width = 8)
  corrplot(corrmatrix,
    method = "square", tl.col = "black", tl.cex = 1 ,
    p.mat = p.mat, sig.level = 0.05, insig = "pch", pch.cex = 1, col = col1(10),
    title = "Correlation matrix of all variables",
    mar = c(0, 0, 1, 0)
  )
  dev.off()

  # violiin plot of "count" by "year" --------------------------------------
  count_violin_plot <- all_var_raw %>%
    # convert count over 1000 to 1000
    mutate(count = ifelse(count > 1000, 1000, count)) %>%
    ggplot(aes(x = count, y = year, group = year)) +
    geom_density_ridges(fill = "#7B52AE", color = "black", alpha = 0.5, from = 0, to = 1000) +
    labs(
      x = "Count",
      y = "Year",
      title = paste0("Distribution of ", target, " count by year"),
      subtitle = city,
      caption = ""
    ) +
    # set breaks and labels: x-axis by 250 and y-axis by 1
    scale_x_continuous(breaks = seq(0, 1000, by = 250), labels = c("0", "250", "500", "750", ">=1000")) +
    scale_y_continuous(breaks = seq(min(all_var_raw$year), max(all_var_raw$year), by = 1), labels = seq(min(all_var_raw$year), max(all_var_raw$year), by = 1)) +
    theme_ipsum() +
    theme(
      plot.margin = ggplot2::margin(0, 0, 0, 0),
      legend.margin = ggplot2::margin(0, 0, 0, 0),  # Remove margin around the legend
      legend.box.margin = ggplot2::margin(0, 0, 0, 0)  # Remove margin around the legend box
    )
  # save
  ggsave(
    plot = count_violin_plot,
    filename = paste0(figure_dir, "/count_ridge_plot.png"),
    width = 7,
    height = 7,
    units = c("in")
  )

  # Boxplot of "count" by "year" --------------------------------------
  count_boxplot <- all_var_raw %>%
    # convert count over 1000 to 1000
    # mutate(count = ifelse(count > 1000, 1000, count)) %>%
    ggplot(aes(x = factor(year), y = count_log)) +
    geom_boxplot(fill = "#7B52AE", color = "black", alpha = 0.5) +
    labs(
      x = "Year",
      y = "Count (log)",
      title = paste0("Distribution of ", target, " count by year"),
      subtitle = city,
      caption = ""
    ) +
  # Dynamically set breaks and labels for y-axis
  scale_y_continuous(
    breaks = pretty_breaks(n = 5)
  ) +
    theme_ipsum() +
    theme(
      plot.margin = ggplot2::margin(0, 0, 0, 0),
      legend.margin = ggplot2::margin(0, 0, 0, 0),  # Remove margin around the legend
      legend.box.margin = ggplot2::margin(0, 0, 0, 0),  # Remove margin around the legend box
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
    )

  # save
  ggsave(
    plot = count_boxplot,
    filename = paste0(figure_dir, "/count_boxplot.png"),
    width = 7,
    height = 7,
    units = c("in")
  )

  # ridge plot of "od_person_count", "od_vehicle_count", "od_biycle_count": not grouped by year --------------------------------------
  od_ridge_plot <- all_var_raw %>%
    # show three variables: od_person_count, od_vehicle_count, od_bicycle_count one by one in the same plot
    pivot_longer(
      cols = c(od_person_count, od_vehicle_count, od_bicycle_count),
      names_to = "variable",
      values_to = "value"
    ) %>%
    # create an order for the variables: 1. od_person_count, 2. od_vehicle_count, 3. od_bicycle_count
    mutate(variable = factor(variable, levels = c("od_person_count", "od_vehicle_count", "od_bicycle_count"))) %>%
    ggplot(aes(x = value, y = variable, group = variable)) +
    geom_density_ridges(fill = "#7B52AE", color = "black", alpha = 0.5, from = 0, to = 15, scale = 1, rel_min_height = 0.001) +
    labs(
      x = "Count",
      y = "Variable",
      title = "Distribution of object detection count",
      subtitle = city,
      caption = ""
    ) +
    # set breaks and labels: x-axis by 250 and y-axis by 1
    scale_x_continuous(breaks = seq(0, 15, by = 3)) +
    scale_y_discrete(labels = c("Person", "Vehicle", "Bicycle")) +
    theme_ipsum() +
    theme(
      plot.margin = ggplot2::margin(0, 0, 0, 0),
      legend.margin = ggplot2::margin(0, 0, 0, 0),  # Remove margin around the legend
      legend.box.margin = ggplot2::margin(0, 0, 0, 0)  # Remove margin around the legend box
    )
  # save
  ggsave(
    plot = od_ridge_plot,
    filename = paste0(figure_dir, "/od_ridge_plot.png"),
    width = 7,
    height = 7,
    units = c("in")
  )

  # map greenery and cycling ------------------------------------------------
  count_station <- read_csv(paste0(external_dir, "/count_station_clean.csv")) %>%
    st_as_sf(., coords = c("longitude", "latitude"), crs = 4326)

  all_var_map <- read.csv(paste0(processed_dir, "/all_var_joined.csv")) %>%
    left_join(., count_station, by = "count_point_id") %>%
    st_as_sf()

  hex_grid <- count_station %>%
    st_transform(3857) %>%
    st_make_grid(cellsize = 1000, square = F) %>%
    st_as_sf() %>%
    st_transform(4326) %>%
    mutate(grid_id = row_number())

  hex_grid_summarized <- hex_grid %>%
    st_join(., all_var_map) %>%
    st_drop_geometry() %>%
    group_by(grid_id) %>%
    dplyr::summarize(across(everything(), .f = list(mean), na.rm = TRUE)) %>%
    rename_with(.fn = function(x) {
      str_remove(x, "_1")
    })

  hex_grid_joined <- hex_grid %>%
    left_join(., hex_grid_summarized, by = "grid_id") %>%
    drop_na(count, ss_vegetation) %>%
    rename(geometry = x) %>%
    st_transform(3857)

  map_bivariate(hex_grid_joined,
    "ss_vegetation",
    "count",
    bivariate_color_scale,
    map_token = Sys.getenv("MAP_TOKEN"),
    axis1 = "More greenery →",
    axis2 = "More count →",
    title = paste0("Greenery and count of ", target, " in ", city),
    subtitle = "between 2008-2020 (1km grid)",
    caption = ""
  )

  # map change in count ----------------------------------------------------
  count_station_year_month <- read.csv(paste0(interim_dir, "/count_station_year_month.csv")) %>%
    left_join(., count_station, by = "count_point_id") %>%
    st_as_sf()

  hex_grid <- count_station %>%
    st_transform(3857) %>%
    st_make_grid(cellsize = 1000, square = F) %>%
    st_as_sf() %>%
    st_transform(4326) %>%
    mutate(grid_id = row_number())

  hex_grid_summarized <- hex_grid %>%
    st_join(., count_station_year_month) %>%
    st_drop_geometry() %>%
    mutate(year_group = cut(year,
      breaks = c(2007, 2015, 2023),
      labels = c("2008-2014", "2015-2020")
    )) %>%
    relocate(year_group, .after = year) %>%
    group_by(grid_id, year_group) %>%
    dplyr::summarize(across(everything(), .f = list(mean), na.rm = TRUE)) %>%
    rename_with(.fn = function(x) {
      str_remove(x, "_1$")
    }) %>%
    filter(year_group %in% c("2008-2014", "2015-2020")) %>%
    dplyr::select(grid_id, year_group, count) %>%
    pivot_wider(names_from = year_group, values_from = count) %>%
    mutate(change = (`2015-2020` - `2008-2014`),
      change = case_when(
        change > 500 ~ 500,
        change < -500 ~ -500,
        TRUE ~ change
      )
    )

  hex_grid_joined <- hex_grid %>%
    left_join(., hex_grid_summarized, by = "grid_id") %>%
    drop_na(change) %>%
    rename(geometry = x) %>%
    st_transform(3857)

  # set up color and breaks
  my_palette <- colorRampPalette(c("#62428b", "#FFFFFF", "#5d9242"))

  # Create the color scale
  color_scale <- my_palette(100)

  subtitle <- case_when(
    city == "London" ~ "Change in cyclist count between 2010-2014 and 2015-2019",
    city == "Montreal" ~ "Change in pedestrian count between 2009-2014 and 2015-2022"
  )
  fill <- case_when(
    city == "London" ~ "Change in cyclist count",
    city == "Montreal" ~ "Change in pedestrian count"
  )
  data_source <- case_when(
    city == "London" ~ "Transport for London",
    city == "Montreal" ~ "Ville de Montréal"
  )
  flush_cache()
  map <- basemap_ggplot(st_bbox(hex_grid_joined),
    map_service = "carto",
    map_type = "light_no_labels", map_res = 1,
    force = T) +
    new_scale_fill() + # Add this line to introduce a new fill scale
    geom_sf(
      data = hex_grid_joined %>% dplyr::select(change),
      mapping = aes(fill = as.numeric(change)),
      color = "black",
      size = 0.05
    ) +
    scale_fill_gradientn(colours = color_scale, 
      breaks = c(-500, -250, 0, 250, 500),
      labels = c("<-500", "-250", "0", "250", ">500"),
      limits = range(hex_grid_joined$change, na.rm = TRUE),
      guide = guide_colourbar(direction = "horizontal",
        title.position = "bottom")
    ) +
    # add titles
    labs(
      x = NULL,
      y = NULL,
      fill = fill,
      title = city,
      subtitle = subtitle,
      caption = paste0("Basemap: carto \nData: ", data_source)
    ) +
    guides(fill = guide_legend(reverse = TRUE)) +
    # add the theme
    theme_map() +
    # make sure margins are 0
    theme(
      plot.margin = ggplot2::margin(0, 0, 0, 0),
      legend.margin = ggplot2::margin(0, 0, 0, 0),  # Remove margin around the legend
      legend.box.margin = ggplot2::margin(0, 0, 0, 0)  # Remove margin around the legend box
    )
  # This code hasn't been executed and serves as a mock-up.
  # You'll need to replace placeholder values and paths with your actual data.
  ggsave(
    plot = map,
    filename = paste0(figure_dir, "/map_grid_count_change.png"),
    width = 7,
    height = 7,
    units = c("in")
  )

  #TODO add map of GSV locations and street networks
  # london: center = 51.5431569,-0.1382722
  # montreal: center = 45.5189821,-73.5838274
  # get street network from OSM with osmdata and save as shapefile
  if (city == "London"){
    center_lon <- -0.1382722
    center_lat <- 51.5431569
  } else if (city == "Montreal"){
    center_lon <- -73.5838274
    center_lat <- 45.5189821
  }
  center_sf <- st_sfc(st_point(c(center_lon, center_lat)), crs = 4326) %>% 
    st_as_sf() %>%
    mutate(center = 1)
  # get a hex_grid_joined that touches the center point
  hex_center_id <- hex_grid_joined %>%
    st_transform(4326) %>%
    st_join(., center_sf) %>%
    dplyr::filter(center == 1) %>%
    pull(grid_id) # everything outside of this hex will be more transparent

  # create a buffer around the center of the hex
  center_buffer_1500m <- hex_grid_joined %>%
    dplyr::filter(grid_id == hex_center_id) %>%
    st_centroid() %>%
    st_transform(3857) %>%
    st_buffer(1500) %>%  # this will be the frame of the plot
    st_transform(4326)
  # Identifying the center hexagon and creating an alpha variable
  hex_grid_joined_intersected <- hex_grid_joined %>%
    st_transform(4326) %>%
    st_intersection(center_buffer_1500m) %>%
    mutate(alpha = 1)
  
  # create 100m buffer around count_station
  count_station_center <- count_station %>%
    st_intersection(center_buffer_1500m)

  count_station_buffer <- count_station_center %>%
    st_transform(3857) %>%
    st_buffer(100) %>%
    st_transform(4326) %>% 
    st_intersection(center_buffer_1500m)

  if (!file.exists(paste0(external_dir, "/street_network.geojson"))){
    street <- st_bbox(center_buffer_1500m) %>%
      opq() %>%
      add_osm_feature(key = "highway") %>%
      osmdata_sf() %>%
      magrittr::extract2("osm_lines") %>% 
      dplyr::select(osm_id, name, geometry) %>%
      st_transform(4326) %>%
      # crop by the buffer
      st_intersection(center_buffer_1500m)
    street %>%
      st_write(paste0(external_dir, "/street_network.geojson"), driver = "GeoJSON")
  } else {
    street <- read_sf(paste0(external_dir, "/street_network.geojson"))
  }
  # load GSV points
  gsv_points <- read_csv(paste0(raw_dir, "/gsv_pids.csv")) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
    st_intersection(center_buffer_1500m) %>% 
    st_intersection(count_station_buffer)
  # plot street network and GSV points: street in black, GSV points in #74B652, and center_sf in #7B52AE
  plot <- ggplot() +
    new_scale_fill() + # Add this line to introduce a new fill scale
    geom_sf(data = hex_grid_joined_intersected %>% dplyr::select(change, alpha),
      mapping = aes(fill = as.numeric(change), alpha = alpha),
      color = "black",
      size = 0.05
    ) +
    scale_alpha_continuous(guide = FALSE) +  # Add this line to remove the alpha legend
    # scale_alpha_manual(values = c(1, 0.2), guide = FALSE) +  # Add this line to set manual alpha values
    scale_fill_gradient2(
      low = "#7B52AE",                    # Color for the low end of the scale
      mid = "white",                   # Color for the midpoint of the scale
      high = "#74B652",                    # Color for the high end of the scale
      midpoint = 0,                    # Value at which mid color should be placed
      breaks = seq(-500, 500, by = 250),
      labels = c("<-500", "-250", "0", "250", ">500"),
      limits = c(-500, 500),
      guide = FALSE
    ) +
    geom_sf(data = count_station_buffer, color='black', fill='white', alpha = 0.8) +
    geom_sf(data = count_station_center, shape = 21, color='black', fill='#74B652', size = 0.1) +
    geom_sf(data = street, color = "black", linewidth = 0.1) +
    geom_sf(data = gsv_points, shape = 21, color='black', fill='white', size = 0.1) +
    labs(
      x = NULL,
      y = NULL
    ) +
    theme_map() +
    theme(
      plot.margin = ggplot2::margin(0, 0, 0, 0),
      legend.margin = ggplot2::margin(0, 0, 0, 0),  # Remove margin around the legend
      legend.box.margin = ggplot2::margin(0, 0, 0, 0)  # Remove margin around the legend box
    )
  # save
  ggsave(
    plot = plot,
    filename = paste0(figure_dir, "/grid_street_network_gsv_points.png"),
    width = 7,
    height = 7,
    units = c("in")
  )
}
