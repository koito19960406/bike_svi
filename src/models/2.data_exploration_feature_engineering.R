pacman::p_load(tidyverse, Hmisc, GGally, corrplot, RColorBrewer, ggplot2, 
               hrbrthemes,stargazer,plotly, sf, basemaps, magrittr,cowplot,dotenv)
extrafont::loadfonts()

# data exploration --------------------------------------------------------
load_dot_env()
root_dir <- Sys.getenv("ROOT_DIR")
root_dir <- "/Volumes/ExFAT/bike_svi"
if (!(file.exists(root_dir))){
  root_dir <- "/Volumes/Extreme SSD/bike_svi"
}
all_var <- read.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined.csv")) %>% 
  relocate(pedal_cycles) %>% 
  mutate(ss_visual_complexity = ifelse(ss_visual_complexity>1,1,ss_visual_complexity),
         age_0_19 = X0_9 + X10_19,
         age_20_39 = X20_29 + X30_39,
         age_40_59 = X40_49 + X50_59,
         age_60_90 = X60_69 + X70_79 + X80_89 + X90,
         lu_residential_community = (lu_community_service + lu_residential)/100,
         lu_commerce_developed = (lu_industry_commerce + lu_transport_utilities + lu_unknown_developed_use)/100,
         lu_others = (lu_agriculture + lu_forest_open_land_water + lu_outdoor_recreation 
          + lu_residential_gardens + lu_defence + lu_minerals_landfill + lu_undeveloped_land + lu_vacant)/100
         ) %>% 
  select(-c(count_point_id, all_ages, period, X0_9, X10_19,
            X20_29, X30_39, X40_49, X50_59,
            X60_69, X70_79, X80_89, X90,
            lu_community_service, lu_residential, lu_residential_gardens,
            lu_industry_commerce, lu_transport_utilities, lu_unknown_developed_use,
            lu_agriculture, lu_forest_open_land_water, lu_outdoor_recreation,
            lu_defence, lu_minerals_landfill, lu_undeveloped_land, lu_vacant
            )) %>% 
  drop_na()

# summary stats
summary_stats <- describe(all_var)
capture.output(summary_stats, file= paste0("models/summary_stats.txt"))
summary_stats_latetx <- all_var %>% 
  mutate(year = as.numeric(year)) %>% 
  drop_na() %>% 
  stargazer()
capture.output(summary_stats_latetx, file= paste0("models/summary_stats_latetx.txt"))

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
pdf("reports/figures/correlation_mat.pdf", height = 7, width = 7)
corrplot(corrmatrix,method = "square",  tl.col = "black", tl.cex = 0.6, 
         p.mat = p.mat, sig.level = 0.05, insig = "pch", pch.cex = 1, col = col1(100),
         title = "Correlation matrix of all variables",
         mar=c(0,0,1,0))
dev.off()

# create df to remove variables over 0.6 correlation
corrdf <- corrmatrix %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Var1") %>%
  gather("Var2", "value", -Var1) 
  
corrdf %>% filter(value>=0.6|value<=-0.6)
# data normalizatioon -----------------------------------------------------
max_over_x <- function(value){
  if (is.numeric(value)){
    if (max(value,na.rm = T)>1){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  else{
    return(FALSE)
  }
}
all_var_scaled <- all_var %>% 
  mutate(year = as.character(year)) %>% 
  # rename_at(vars(contains('count')), ~paste0(., "_log")) %>% 
  rename_if(max_over_x, list(~paste0(., "_log"))) %>% 
  mutate_at(vars(contains("_log")), log) %>% 
  mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), 0, .x))) %>% 
  mutate(pedal_cycles = all_var$pedal_cycles)

# save
all_var_scaled %>% 
  write.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined_scaled.csv"), row.names = F)

# pairwise correlation for pedal_cycles and segmentation result
pedal_seg <- all_var %>% 
  select(c("pedal_cycles","ss_vegetation","ss_sidewalk", "slope"))
pedal_seg_scaled <- all_var_scaled %>% 
  select(c("pedal_cycles_log","ss_vegetation","ss_sidewalk", "slope_log"))
pair_corr <- function(data,title,file_path){
  # pdf(file_path, height = 7, width = 7)
  scatter_plot <- function(data, mapping, ...) {
    p <- ggplot(data = data, mapping=mapping) +
      stat_bin_2d(bins=50) +
      scale_fill_gradient(low = "#312146",
                          high = "#cabadf")
    return(p)
  }
  g <- ggpairs(data, lower=list(continuous=scatter_plot), title=title)+
    theme_ipsum()+
    theme(axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6))
  ggsave(plot=g,file_path)
  # dev.off()
}
pair_corr(pedal_seg, "Pair-wise correlation matrix", "reports/figures/pair_wise_correlation.png")
pair_corr(pedal_seg_scaled, "Pair-wise correlation matrix", "reports/figures/pair_wise_correlation_scaled.png")
# convert treatment into binary
convert_to_binary <- function(array, percentile){
  tile <- ntile(array,10)
  # return TRUE to those above threshold
  return(ifelse(tile>percentile,1,0))
}
# function to create multiple binary columns with different thresholds
create_binary <- function(data,colname){
  for (i in seq(1,9)){
    data <- data %>% 
      mutate("{{colname}}_binary_{i*10}_percentile" := convert_to_binary({{colname}},i))
  }
  return(data)
}
# apply function
all_var_scaled_binary_treatment <- all_var_scaled %>% 
  create_binary(., ss_vegetation) %>% 
  create_binary(., ss_sidewalk) %>% 
  create_binary(., slope_log) %>% 
  drop_na()
all_var_scaled_binary_treatment %>% 
  write.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined_scaled_binary.csv"),row.names = FALSE)


# map greenery and cycling ------------------------------------------------
count_station <- read_csv(paste0(root_dir,"/data/external/cities/London/count_station.csv")) %>% 
  st_as_sf(.,coords=c("longitude","latitude"),crs=4326) 

all_var_map <- read.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined.csv")) %>% 
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
  drop_na(pedal_cycles,ss_vegetation) %>% 
  rename(geometry=x)

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
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
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
         filename = "reports/figures/map_grid.png",
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

map_bivariate(hex_grid_joined,
              "ss_vegetation", 
              "pedal_cycles",
              bivariate_color_scale,
              map_token = Sys.getenv("MAP_TOKEN"),
              axis1="More greenery →", 
              axis2="More cyclists →",
              title="Greenery and cyclists count in London",
              subtitle="between 2008-2020 (1km grid)",
              caption="")




