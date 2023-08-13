pacman::p_load(tidyverse, Hmisc, GGally, corrplot, RColorBrewer, ggplot2, 
               hrbrthemes,stargazer,plotly, sf, basemaps, magrittr,cowplot,dotenv,
               basemapR, ggnewscale, here)
extrafont::loadfonts()

# data exploration --------------------------------------------------------
load_dot_env()
root_dir <- Sys.getenv("ROOT_DIR")
if (!file.exists(root_dir)){
  root_dir <- here()
}


# load external/city_list.txt to get the list of cities
city_list <- read.csv(paste0(root_dir,"/data/external/city_list.txt"), header = FALSE, sep = "\t") %>% 
  rename(city = V1) %>% 
  mutate(city = str_replace_all(city, " ", "_")) %>% 
  as.vector() %>% 
  unlist()


# reference: https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/#define-a-map-theme
theme_map <- function(...,
                      default_font_color = "#4e4d47",
                      default_background_color = "#f5f5f2",
                      default_font_family = "Ubuntu Regular"
) {
  theme_ipsum() +
    theme(
      text = element_text(family = default_font_family,
                          color = default_font_color),
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
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = default_font_color),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = default_font_color),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = default_font_color,
                                   margin = ggplot2::margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   r = 0,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = ggplot2::margin(t = 0.2,
                                                  b = 0,
                                                  r = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}

prep_data <- function(sf, col1, col2, bivariate_color_scale){
  # create 3 buckets for gini
  quantiles1 <- sf %>%
    pull({{col1}}) %>%
    quantile(probs = seq(0, 1, length.out = 4), na.rm=T)
  
  # create 3 buckets for mean income
  quantiles2 <- sf %>%
    pull({{col2}}) %>%
    quantile(probs = seq(0, 1, length.out = 4), na.rm=T)
  
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
        .data[[{{col1}}]],
        breaks = quantiles1,
        include.lowest = TRUE
      ),
      quantiles2_col = cut(
        .data[[{{col2}}]],
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
    mutate(fill=replace_na(fill,"#5A5A5A")) %>% 
    st_transform(3857)
  return(sf)
}

create_legend <- function(bivariate_color_scale, col1, col2, axis1="", axis2=""){
  # separate the groups
  bivariate_color_scale %<>%
    gather("group", "fill") %>%
    separate("group", into = c({{col1}}, {{col2}}), sep = " - ") %>%
    mutate("{{col1}}" := as.integer(.data[[{{col1}}]]),
           "{{col2}}" := as.integer(.data[[{{col2}}]]))
  legend <- ggplot() +
    geom_tile(
      data = bivariate_color_scale,
      mapping = aes(
        x = .data[[{{col1}}]],
        y = .data[[{{col2}}]],
        fill = fill)
    ) +
    scale_fill_identity() +
    labs(x = axis1,
         y = axis2) +
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
                          map_token="",
                          axis1="", axis2="",
                          title="",
                          subtitle="",
                          caption=""){
  flush_cache()
  clean_sf <- prep_data(sf, col1, col2, bivariate_color_scale)
  legend_custom <- create_legend(bivariate_color_scale, col1, col2, axis1=axis1,axis2=axis2)
  map <- basemap_ggplot(st_bbox(clean_sf), map_service="carto", 
                        map_type = "light_no_labels",map_res = 1,
                        force=T) +
    # color municipalities according to their combination
    geom_sf(
      data=clean_sf,
      aes(fill = fill),
      # use thin white stroke for municipalities
      color = "white",
      size = 0.1) +
    # as the sf object clean_sf has a column with name "fill" that
    # contains the literal color as hex code for each municipality, we can use
    # scale_fill_identity here
    scale_fill_identity() +
    # add titles
    labs(x = NULL,
         y = NULL,
         title = title,
         subtitle = subtitle,
         caption = caption) +
    # add the theme
    theme_map()
  final_plot <- ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend_custom, 0.75, 0.15, 0.18, 0.18)
  ggsave(plot = final_plot, 
         filename = paste0(figure_dir, "/map_grid.png"),
                           width = 7,
                           height = 7,
                           units = c("in"))
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
for (city in city_list){
  # create folders for each city
  model_dir <- paste0(root_dir,"/models/", city)
  if (!dir.exists(model_dir)){
    dir.create(model_dir)
  }
  figure_dir <- paste0("reports/figures/", city)
  if (!dir.exists(figure_dir)){
    dir.create(figure_dir)
  }
  external_dir <- paste0(root_dir,"/data/external/cities/", city)
  processed_dir <- paste0(root_dir,"/data/processed/cities/", city)
  
  # read in the data
  # binarize some variables: ss_sidewalk, ss_pedestrian_area, ss_bike_lane, ss_bike_rack, ss_parking
  #TODO: only keep some columns from object detection (i.e., columns that start with "od_" + vehicles + people):
  # od_person_count: od_person
  # od_bicycle: od_bicyclist, od_bicycle
  # od_vehicle_count: od_vehicle, od_bus, od_car, od_caravan, od_motorcycle, od_truck, od_other_vehicle, od_trailer, od_train, od_wheeled_slow, od_ego_vehicle
  # od_animal_count: od_bird, od_ground_animal
  all_var_with_id <- read.csv(paste0(processed_dir, "/all_var_joined.csv")) %>%
    dplyr::select(-contains("binary"), -contains("count_log")) %>% 
    dplyr::select(-c(age_60_90,lu_others,
                     ss_bike_rack, ss_curb, ss_curb_cut,
                     ss_pothole,
                     ss_pedestrian_area, ss_bench
    ))
    # relocate(count) %>% 
    # mutate(ss_visual_complexity = ifelse(ss_visual_complexity>1,1,ss_visual_complexity),
    #       ss_sidewalk = ifelse(ss_sidewalk>0.05,1,0),
    #       ss_pedestrian_area = ifelse(ss_pedestrian_area>0.05,1,0),
    #       ss_bike_lane = ifelse(ss_bike_lane>0.05,1,0),
    #       ss_bike_rack = ifelse(ss_bike_rack>0.05,1,0),
    #       ss_parking = ifelse(ss_parking>0.05,1,0),
    #       ss_construction = ss_fence + ss_barrier + ss_wall + ss_bridge + ss_building + ss_tunnel,
    #       ss_road_flat = ss_crosswalk_plain + ss_rail_track + ss_road + ss_service_lane,
    #       ss_marking = ss_lane_marking_crosswalk + ss_lane_marking_general,
    #       ss_nature = ss_mountain + ss_sand + ss_snow + ss_terrain + ss_water,
    #       ss_street_object = ss_banner + ss_billboard + ss_catch_basin + ss_cctv_camera + ss_fire_hydrant + ss_junction_box + ss_mailbox + ss_manhole + ss_phone_booth + ss_pole
    #         + ss_traffic_sign_frame + ss_utility_pole + ss_traffic_light + ss_traffic_sign_back + ss_traffic_sign_front + ss_trash_can,
    #       od_person_count = od_person,
    #       od_bicycle_count = od_bicyclist + od_bicycle,
    #       od_vehicle_count = od_vehicle + od_bus + od_car + od_caravan + od_motorcycle + od_truck + od_other_vehicle + od_trailer + od_train + od_wheeled_slow + od_ego_vehicle,
    #       od_animal_count = od_bird + od_ground_animal,
    #       count_point_id = as.character(count_point_id)
    #       ) %>%
    # drop_na()

  all_var <- all_var_with_id %>% 
    dplyr::select(-c(count_point_id))

  # # summary stats
  # summary_stats <- describe(all_var, na.rm = T)
  # print(summary_stats)
  # capture.output(summary_stats, file= paste0(model_dir, "/summary_stats.txt"))
  summary_stats_latetx <- all_var %>% 
    mutate(year = as.numeric(year)) %>% 
    drop_na() %>% 
    stargazer(type = "latex", out = paste0(model_dir, "/summary_stats_latetx.tex"))
  capture.output(summary_stats_latetx, file= paste0(model_dir, "/summary_stats_latetx.txt"))

  # correlation matrix
  corrmatrix <- cor(all_var,use="complete.obs")
  cor.mtest <- function(mat, ...) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
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
  p.mat <- cor.mtest(all_var)
  # original color: purple(#7B52AE) and green (#74B652)
  col1 <- colorRampPalette(c("#62428b", "#FFFFFF", "#5d9242"))
  pdf(paste0(figure_dir, "/correlation_mat.pdf"), height = 7, width = 7)
  corrplot(corrmatrix,method = "square",  tl.col = "black", tl.cex = 0.6, 
          p.mat = p.mat, sig.level = 0.05, insig = "pch", pch.cex = 1, col = col1(100),
          title = "Correlation matrix of all variables",
          mar=c(0,0,1,0))
  dev.off()

  # # create df to remove variables over 0.6 correlation
  # corrdf <- corrmatrix %>%
  #   as.data.frame() %>%
  #   tibble::rownames_to_column("Var1") %>%
  #   gather("Var2", "value", -Var1)
  # 
  # corrdf %>% filter(value>=0.6|value<=-0.6)
  # # data normalizatioon -----------------------------------------------------
  # max_over_x <- function(value){
  #   if (is.numeric(value)){
  #     if (max(value,na.rm = T)>1){
  #       return(TRUE)
  #     }
  #     else{
  #       return(FALSE)
  #     }
  #   }
  #   else{
  #     return(FALSE)
  #   }
  # }
  # all_var_scaled <- all_var_with_id %>%
  #   mutate(year = as.character(year)) %>%
  #   # rename_at(vars(contains('count')), ~paste0(., "_log")) %>%
  #   rename_if(max_over_x, list(~paste0(., "_log"))) %>%
  #   mutate_at(vars(contains("_log")), function(x) log(x+1)) %>%
  #   mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), 0, .x))) %>%
  #   mutate(count = all_var$count)
  # 
  # # save
  # all_var_scaled %>%
  #   write.csv(paste0(processed_dir, "/all_var_joined_scaled.csv"), row.names = F)

  # # pairwise correlation for count and segmentation result
  # if (city=="London"){
  #   ss_var_list <- c("ss_vegetation", "ss_bike_lane", "ss_bike_rack", "ss_curb", "ss_curb_cut", "ss_parking", "ss_pothole", "ss_street_light")
  # } else if (city=="Montreal"){
  #   ss_var_list <- c("ss_vegetation", "ss_guard_rail", "ss_pedestrian_area", "ss_sidewalk", "ss_street_light", "ss_bench")
  # }
  # count_seg <- all_var %>% 
  #   dplyr::select(c("count", "slope", ss_var_list))
  # # count_seg_scaled <- all_var_scaled %>% 
  # #   dplyr::select(c("count_log", "slope_log", ss_var_list))
  # pair_corr <- function(data,title,file_path){
  #   # pdf(file_path, height = 7, width = 7)
  #   scatter_plot <- function(data, mapping, ...) {
  #     p <- ggplot(data = data, mapping=mapping) +
  #       stat_bin_2d(bins=50) +
  #       scale_fill_gradient(low = "#312146",
  #                           high = "#cabadf")
  #     return(p)
  #   }
  #   g <- ggpairs(data, lower=list(continuous=scatter_plot), title=title)+
  #     theme_ipsum()+
  #     theme(axis.text.x = element_text(size = 1),
  #           axis.text.y = element_text(size = 1))
  #   ggsave(plot=g,file_path,
  #          width = 7,
  #          height = 7,
  #          units = c("in"))
  #   # dev.off()
  # }
  # pair_corr(count_seg, "Pair-wise correlation matrix", paste0(figure_dir, "/pair_wise_correlation.png"))
  # pair_corr(count_seg_scaled, "Pair-wise correlation matrix", paste0(figure_dir, "/pair_wise_correlation_scaled.png"))
  # # convert treatment into binary
  # convert_to_binary <- function(array, percentile){
  #   tile <- ntile(array,10)
  #   # return TRUE to those above threshold
  #   return(ifelse(tile>percentile,1,0))
  # }
  # # function to create multiple binary columns with different thresholds
  # create_binary <- function(data,colname){
  #   for (i in seq(1,9)){
  #     data <- data %>% 
  #       mutate("{{colname}}_binary_{i*10}_percentile" := convert_to_binary({{colname}},i))
  #   }
  #   return(data)
  # }
  # # apply function for each of ss_var_list with walk
  # all_var_scaled_binary_treatment <- all_var_scaled
  # for (ss_var in ss_var_list){
  #   all_var_scaled_binary_treatment <- create_binary(all_var_scaled_binary_treatment, ss_var)
  # }
  # all_var_scaled_binary_treatment %>% 
  #   write.csv(paste0(processed_dir, "/all_var_joined_scaled_binary.csv"),row.names = FALSE)


  # map greenery and cycling ------------------------------------------------
  count_station <- read_csv(paste0(external_dir, "/count_station_clean.csv")) %>% 
    st_as_sf(.,coords=c("longitude","latitude"),crs=4326) 

  all_var_map <- read.csv(paste0(processed_dir, "/all_var_joined.csv")) %>% 
    left_join(.,count_station,by="count_point_id") %>% 
    st_as_sf()

  hex_grid <- count_station %>% 
    st_transform(3857) %>% 
    st_make_grid(cellsize=1000,square=F) %>% 
    st_as_sf() %>% 
    st_transform(4326) %>% 
    mutate(grid_id = row_number()) 

  hex_grid_summarized <- hex_grid %>% 
    st_join(.,all_var_map) %>% 
    st_drop_geometry() %>% 
    st_drop_geometry() %>% 
    group_by(grid_id) %>% 
    dplyr::summarize(across(everything(), .f = list(mean), na.rm = TRUE)) %>% 
    rename_with(.fn=function(x){str_remove(x,"_1")})
    
  hex_grid_joined <- hex_grid %>% 
    left_join(.,hex_grid_summarized,by="grid_id") %>% 
    drop_na(count,ss_vegetation) %>% 
    rename(geometry=x) %>% 
    st_transform(3857)

  map_bivariate(hex_grid_joined,
                "ss_vegetation", 
                "count",
                bivariate_color_scale,
                map_token = Sys.getenv("MAP_TOKEN"),
                axis1="More greenery →", 
                axis2="More cyclists →",
                title=paste0("Greenery and count in ", city),
                subtitle="between 2008-2020 (1km grid)",
                caption="")

  hex_grid <- count_station %>%
    st_transform(3857) %>%
    st_make_grid(cellsize=1000,square=F) %>%
    st_as_sf() %>%
    st_transform(4326) %>%
    mutate(grid_id = row_number())

  hex_grid_summarized <- hex_grid %>%
    st_join(.,all_var_map) %>%
    st_drop_geometry() %>%
    mutate(year_group = cut(year, breaks = c(2007, 2010, 2017, 2023),
                            labels = c("2008-2010", "2011-2017", "2018-2020"))) %>%
    relocate(year_group, .after = year) %>%
    group_by(grid_id, year_group) %>%
    dplyr::summarize(across(everything(), .f = list(mean), na.rm = TRUE)) %>%
    rename_with(.fn=function(x){str_remove(x,"_1$")}) %>%
    filter(year_group %in%  c("2008-2010", "2018-2020")) %>%
    dplyr::select(grid_id, year_group, count) %>%
    pivot_wider(names_from = year_group, values_from = count) %>%
    mutate(change = `2018-2020` - `2008-2010`)

  hex_grid_joined <- hex_grid %>%
    left_join(.,hex_grid_summarized,by="grid_id") %>%
    drop_na(change) %>%
    rename(geometry=x) %>%
    st_transform(3857)

  # set up color and breaks
  my_palette <- colorRampPalette(c("#62428b", "#FFFFFF", "#5d9242"))
  # Calculate quantiles for the value column
  quantiles <- quantile(hex_grid_joined$change, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1))
  # here I define custom labels (the default ones would be ugly)
  labels <- c()

  for(idx in 1:length(quantiles)){
    labels <- c(labels, paste0(round(quantiles[idx], 2),
                              " — ",
                              round(quantiles[idx + 1], 2)))
  }
  # I need to remove the last label
  # because that would be something like "66.62 - NA"
  labels <- labels[1:length(labels)-1]

  # Create the color scale
  color_scale <- setNames(my_palette(length(quantiles)-1), labels)

  # here I actually create a new
  # variable on the dataset with the quantiles
  hex_grid_joined$change_quantile <- cut(hex_grid_joined$change,
                                      breaks = quantiles,
                                      labels = labels,
                                      include.lowest = T)
  subtitle <- case_when(
    city == "London" ~ "between 2008-2010 and 2018-2020",
    city == "Montreal" ~ "between 2008-2010 and 2018-2023"
  )
  flush_cache()
  map <- basemap_ggplot(st_bbox(hex_grid_joined), map_service="carto",
                   map_type = "light_no_labels",map_res = 1,
                   force=T) +
    new_scale_fill() + # Add this line to introduce a new fill scale
    geom_sf(
      data=hex_grid_joined,
      mapping = aes(fill = change_quantile),
      color = "black",
      size = 0.05) +
    scale_fill_manual(values = color_scale) +
    # add titles
    labs(x = NULL,
        y = NULL,
        fill = "Change in count",
        title = "Change in the average count over time",
        subtitle = subtitle,
        caption = "Basemap: carto") +
    guides(fill = guide_legend(reverse = TRUE)) +
    # add the theme
    theme_map()

  ggsave(plot = map,
        filename = paste0(figure_dir, "/map_grid_count_change.png"),
        width = 7,
        height = 7,
        units = c("in"))
}
