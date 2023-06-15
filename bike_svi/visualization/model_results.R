pacman::p_load(tidyverse, ggplot2, hrbrthemes, extrafont, sf)

# stepwise model results --------------------------------------------------
# define a function to plot
plot_step <- function(file_path, ind_var_name){
  df <- read.csv(file_path) %>% 
    mutate(variable = as.factor(variable)) %>% 
    gather(., type, value, point_estimate:p_value, factor_key=TRUE)
  print(df)
  print(summary(df))
  plot <- ggplot(data=df, aes(x=variable, y= value, fill = type)) +
    geom_col(position="dodge") +
    scale_y_continuous(name = "Point Estimate",
                       sec.axis = sec_axis(~., name="P Value"),
                       minor_breaks = NULL) +
    scale_fill_manual("",values=c("#7B52AE", "#74B652")) +
    theme_ipsum() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title=paste0("Step-wise model result for ", ind_var_name))
  ggsave(paste0("reports/figures/", ind_var_name, "_",str_replace(basename(file_path),".csv",".png")), width = 8, height = 4)
}
ind_var_name_list <- c("ss_vegetation","ss_sidewalk","slope_log")
model_list <- c("negbin")
for (ind_var_name in ind_var_name_list){
  for (model in model_list){
    file_path <- paste0("models/",ind_var_name,"/", "year_zero_inflated_",model,"_result.csv")
    plot_step(file_path, ind_var_name)
  }
}


# causal forest feature importance ----------------------------------------
plot_importance <- function(file_path, ind_var_name, top_n=10){
  df <- read.csv(file_path) %>% 
    slice_max(variable_importance,n=top_n)
  print(df)
  ggplot(data=df, aes(x=reorder(var_name, variable_importance), y=variable_importance)) +
    geom_bar(stat="identity", fill = "#7B52AE", width=0.4)+
    coord_flip() +
    theme_ipsum() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size=10),
          plot.title = element_text(size=10,hjust=0.5),
          plot.title.position="plot") +
    labs(title=paste0("Variable importance \n when estimating ", ind_var_name, " (top ", as.character(top_n), ")"),
         x="Variable names",
         y="Variable importance")
  ggsave(paste0("reports/figures/", ind_var_name, "_",str_replace(basename(file_path),".csv",".png")), width = 4, height = 5)
}
ind_var_name_list <- c("ss_vegetation","ss_sidewalk","slope_log")
for (ind_var_name in ind_var_name_list){
  file_path <- paste0("models/",ind_var_name,"/", "binary_causal_forest_var_imp.csv")
  plot_importance(file_path, ind_var_name)
}

# plot the overall variable importance
df <- read.csv(paste0("models/binary_causal_forest_var_imp.csv"))%>% 
  slice_max(variable_importance,n=10)
print(df)
plot <-ggplot(data=df, aes(x=reorder(var_name, variable_importance), y=variable_importance)) +
  geom_bar(stat="identity", fill = "#7B52AE", width=0.4) +
  coord_flip() +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size=10),
        plot.title = element_text(size=10, hjust=0.5),
        plot.title.position="plot") +
  labs(title=paste0("Overall variable importance", " (top ", as.character(10), ")"),
       x="Variable names",
       y="Variable importance")
ggsave(paste0("reports/figures/overall_variable_importance.png"), width = 4, height = 5)


# plot CATE average heatmap -----------------------------------------------
plot_cate_rank <- function(forest.ate){
  # Plotting the point estimate of average treatment effect
  # and 95% confidence intervals around it.
  ggplot(forest.ate) + 
    aes(x = ranking, y = estimate) +
    geom_linerange(aes(ymin=estimate-2*std.err, ymax=estimate+2*std.err, xmin = ranking, xmax= ranking),  position=position_dodge(0.6), size = 1)+
    geom_point(position=position_dodge(0.6), size = 3) +
    # scale_y_continuous(breaks = seq(-0.5, -0.2, 0.1),
    #                    labels = seq(-0.5, -0.2, 0.1), 
    #                    limits= c(-0.5, NA)) + 
    # scale_color_manual(values = cb_colors[1:2]) + 
    ylab("") + xlab("")  +
    labs(title=paste0("Average CATT within each ranking \n for ", str_remove(ind_var_name,"_binary_70_percentile")))+
    theme_ipsum() +
    theme(plot.title = element_text(hjust=0.5),
          plot.title.position="plot")
  ggsave(paste0("reports/figures/", str_remove(ind_var_name,"_binary_70_percentile"),"_rank_catt.png"), width = 5, height = 5)
}
plot_cate_heatmap <- function(df){
  # plot heatmap
  ggplot(df) +
    aes(ranking, covariate) +
    geom_tile(aes(fill = scaling), color = "white", size =1) +
    geom_text(aes(label = labels), size = 12/.pt) +
    scale_fill_gradient(low = "#FFFFFF", high = "#7B52AE", limits =c(0,1), breaks = seq(0, 1, .25)) +
    ggtitle(paste0()) +
    labs(title = paste0("Average covariate values within group \n for ",str_remove(ind_var_name,"_binary_70_percentile")),
         x = "CATE estimate ranking",
         y = "",
         fill= "Scaling") +
    theme_ipsum() +
    theme(axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          plot.title = element_text(size=45, hjust=0.5),
          plot.title.position="plot")
  ggsave(paste0("reports/figures/", str_remove(ind_var_name,"_binary_70_percentile"),"_rank_cate_covariates.png"), width = 10, height = 15)
}
ind_var_name_list <- c("ss_vegetation","ss_sidewalk","slope_log")
for (ind_var_name in ind_var_name_list){
  df <- read_csv(paste0("models/", ind_var_name,"/rank_cate_covariates.csv"))
  forest.ate <- read_csv(paste0("models/", str_remove(ind_var_name,"_binary_70_percentile"),"/rank_cate.csv"))
  plot_cate_rank(forest.ate)
  plot_cate_heatmap(df)
}


# plot HTE by covariate ---------------------------------------------------
plot_hte_covariate <- function(ind_var_name){
  # load the results
  data_pred <- read.csv(paste0("models/", str_remove(ind_var_name,"_binary_70_percentile"),"/hte_by_covariate.csv")) %>% 
    mutate(ci_low= estimate - (qnorm(0.975) * std.err),
           ci_high = estimate + (qnorm(0.975) * std.err),
           category= as.factor(category))
    # dplyr::select(covariate, category,ci_low,ci_high) %>% 
    # pivot_wider(names_from = category, values_from = c(ci_low, ci_high),names_sep = ".") %>%
    # filter(((ci_high.high>ci_high.low)&(ci_low.high>ci_high.low))|
    #           ((ci_high.low>ci_high.high)&(ci_low.low>ci_high.high))) %>%  # filter out those without significant differences
    # pivot_longer(., 
    #              -covariate,
    #              cols = starts_with("ci"), 
    #              names_to = c(".value", "category"), 
    #              names_sep = ".") %>% 
    # print()
  # manually selected interesting results
  print(ind_var_name)
  if (ind_var_name == "ss_vegetation"){
    data_pred <- data_pred %>%
      dplyr::filter(covariate %in% c("age_40_59", "housing_price_log",
                                     "poi_log", "lu_commerce_developed"))
  } 
  if (ind_var_name == "slope_log"){
    data_pred <- data_pred %>%
      dplyr::filter(covariate %in% c("age_0_19", "housing_price_log", 
                                     "poi_log", "ss_sky"))
  }

  # Plot predictions for each group and 95% confidence intervals around them.
  ggplot(data_pred) +
    geom_point(aes(x=category, y=estimate), color="#7B52AE") +
    geom_errorbar(aes(x=category, ymin=ci_low, ymax=ci_high, width=.2), color="#7B52AE") +
    geom_text(aes(x = category, y = estimate, label = round(estimate,2)), hjust = .5, vjust = -.5) +
    coord_flip() +
    labs(title = paste0("Subgroup CATT for ", str_remove(ind_var_name,"_binary_70_percentile")),
         x = "category",
         y = "",
         fill= "") +
    # scale_x_continuous("polviews", breaks=covariate.grid, labels=signif(covariate.grid, 2)) +
    theme_ipsum() +
    theme(axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10),
          plot.title = element_text(size=30, hjust=0.5),
          plot.title.position="plot",
          strip.text.y.right = element_text(angle = 0)) +
    facet_grid(covariate ~ .)
  ggsave(paste0("reports/figures/", str_remove(ind_var_name,"_binary_70_percentile"),"_hte_by_covariate.png"), width = 10, height = 6)
}

ind_var_name_list <- c("ss_vegetation","ss_sidewalk","slope_log")
for (ind_var_name in ind_var_name_list){
  plot_hte_covariate(ind_var_name)
}


# plot HTE by ranking -----------------------------------------------------
plot_hte_ranking <- function(ind_var_name){
  cf_preds <- read.csv(paste0("models/",sub("_binary.*", "", ind_var_name),"/", treatment, "_predictions.csv"))
  ggplot(mapping = aes(
    x = rank(cf_preds$predictions), 
    y = cf_preds$predictions)) + 
    geom_errorbar(
      mapping = aes(
        ymin = cf_preds$predictions + 1.96 * sqrt(cf_preds$variance.estimates),
        ymax = cf_preds$predictions - 1.96 * sqrt(cf_preds$variance.estimates)), 
      width = 0.05, colour = "grey") +
    geom_point(size = 0.5, colour = "#7B52AE") +
    geom_hline(yintercept = c(0), colour = "black", linetype = "dashed", size = 1) +
    labs(title = paste0("Heterogeneous Treatment Effects by Ranking for ", str_remove(ind_var_name,"_binary_70_percentile")),
         x = "Rank", 
         y = "Estimated Treatment Effect") +
    theme_ipsum() +
    theme(axis.text = element_text(size = 40), 
          axis.title = element_text(size = 40), 
          plot.title = element_text(size = 60),
          #plot.margin = unit(c(0.1,0.1,0.1,0.1),"inch")
          ) 
  # theme(panel.grid.major.x = element_blank(),
  #       panel.grid.minor.x = element_blank(),
  #       panel.grid.major.y = element_blank(),
  #       panel.grid.minor.y = element_blank())
  ggsave(paste0("reports/figures/", str_remove(ind_var_name,"_binary_70_percentile"),"_hte_by_ranking.png"), width = 30, height = 10)
}
ind_var_name_list <- c("ss_vegetation_binary_70_percentile","ss_sidewalk","slope_log_binary_70_percentile")
for (ind_var_name in ind_var_name_list){
  plot_hte_ranking(ind_var_name)
}



# # propensity score in map -------------------------------------------------
# count_station <- read_csv(paste0(root_dir,"/data/external/cities/London/count_station.csv")) %>% 
#   st_as_sf(.,coords=c("longitude","latitude"),crs=4326) 
# 
# ps_vegetation <- read.csv("models/ss_vegetation/ss_vegetation_binary_70_percentile_propensity_score.csv") %>% 
#   left_join(.,count_station,by="count_point_id") %>% 
#   st_as_sf()
# 
# hex_grid <- count_station %>% 
#   st_transform(3857) %>% 
#   st_make_grid(cellsize=1000,square=F) %>% 
#   st_as_sf() %>% 
#   st_transform(4326) %>% 
#   mutate(grid_id = row_number()) 
# 
# hex_grid_summarized <- hex_grid %>% 
#   st_join(.,ps_vegetation) %>% 
#   st_drop_geometry() %>% 
#   st_drop_geometry() %>% 
#   group_by(grid_id) %>% 
#   dplyr::summarize(across(everything(), .f = list(mean), na.rm = TRUE)) %>% 
#   rename_with(.fn=function(x){str_remove(x,"_1")})
# 
# hex_grid_joined <- hex_grid %>% 
#   left_join(.,hex_grid_summarized,by="grid_id") %>% 
#   drop_na(pedal_cycles,pr_score) %>% 
#   rename(geometry=x)
# 
# # reference: https://timogrossenbacher.ch/2019/04/bivariate-maps-with-ggplot2-and-sf/#define-a-map-theme
# theme_map <- function(...,
#                       default_font_color = "#4e4d47",
#                       default_background_color = "#f5f5f2",
#                       default_font_family = "Ubuntu Regular"
# ) {
#   theme_ipsum() +
#     theme(
#       text = element_text(family = default_font_family,
#                           color = default_font_color),
#       # remove all axes
#       axis.line = element_blank(),
#       axis.text.x = element_blank(),
#       axis.text.y = element_blank(),
#       axis.ticks = element_blank(),
#       # add a subtle grid
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       # background colors
#       # plot.background = element_rect(fill = default_background_color,
#       #                                color = NA),
#       # panel.background = element_rect(fill = default_background_color,
#       #                                 color = NA),
#       # legend.background = element_rect(fill = default_background_color,
#       #                                  color = NA),
#       # borders and margins
#       plot.margin = unit(c(.5, .5, .2, .5), "cm"),
#       panel.border = element_blank(),
#       panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
#       # titles
#       legend.title = element_text(size = 11),
#       legend.text = element_text(size = 9, hjust = 0,
#                                  color = default_font_color),
#       plot.title = element_text(size = 15, hjust = 0.5,
#                                 color = default_font_color),
#       plot.subtitle = element_text(size = 10, hjust = 0.5,
#                                    color = default_font_color,
#                                    margin = margin(b = -0.1,
#                                                    t = -0.1,
#                                                    l = 2,
#                                                    unit = "cm"),
#                                    debug = F),
#       # captions
#       plot.caption = element_text(size = 7,
#                                   hjust = .5,
#                                   margin = margin(t = 0.2,
#                                                   b = 0,
#                                                   unit = "cm"),
#                                   color = "#939184"),
#       ...
#     )
# }
# 
# prep_data <- function(sf, col1, col2, bivariate_color_scale){
#   # create 3 buckets for gini
#   quantiles1 <- sf %>%
#     pull({{col1}}) %>%
#     quantile(probs = seq(0, 1, length.out = 4), na.rm=T)
#   
#   # create 3 buckets for mean income
#   quantiles2 <- sf %>%
#     pull({{col2}}) %>%
#     quantile(probs = seq(0, 1, length.out = 4), na.rm=T)
#   
#   # create color scale that encodes two variables
#   # red for gini and blue for mean income
#   # the special notation with gather is due to readibility reasons
#   bivariate_color_scale %<>%
#     gather("group", "fill")
#   # cut into groups defined above and join fill
#   sf %<>%
#     mutate(
#       centroid = map(geometry, st_centroid),
#       coords = map(centroid, st_coordinates),
#       coords_x = map_dbl(coords, 1),
#       coords_y = map_dbl(coords, 2)
#     ) %>% 
#     mutate(
#       quantiles1_col = cut(
#         .data[[{{col1}}]],
#         breaks = quantiles1,
#         include.lowest = TRUE
#       ),
#       quantiles2_col = cut(
#         .data[[{{col2}}]],
#         breaks = quantiles2,
#         include.lowest = TRUE
#       ),
#       # by pasting the factors together as numbers we match the groups defined
#       # in the tibble bivariate_color_scale
#       group = paste(
#         as.numeric(quantiles1_col), "-",
#         as.numeric(quantiles2_col)
#       )
#     ) %>%
#     # we now join the actual hex values per "group"
#     # so each municipality knows its hex value based on the his gini and avg
#     # income value
#     left_join(bivariate_color_scale, by = "group") %>% 
#     mutate(fill=replace_na(fill,"#5A5A5A")) %>% 
#     st_transform(3857)
#   return(sf)
# }
# 
# create_legend <- function(bivariate_color_scale, col1, col2, axis1="", axis2=""){
#   # separate the groups
#   bivariate_color_scale %<>%
#     gather("group", "fill") %>%
#     separate("group", into = c({{col1}}, {{col2}}), sep = " - ") %>%
#     mutate("{{col1}}" := as.integer(.data[[{{col1}}]]),
#            "{{col2}}" := as.integer(.data[[{{col2}}]]))
#   legend <- ggplot() +
#     geom_tile(
#       data = bivariate_color_scale,
#       mapping = aes(
#         x = .data[[{{col1}}]],
#         y = .data[[{{col2}}]],
#         fill = fill)
#     ) +
#     scale_fill_identity() +
#     labs(x = axis1,
#          y = axis2) +
#     theme_map() +
#     # make font small enough
#     theme(
#       axis.title = element_text(size = 6)
#     ) +
#     # quadratic tiles
#     coord_fixed()
#   return(legend)
# }
# 
# map_bivariate <- function(sf, 
#                           col1, 
#                           col2,
#                           bivariate_color_scale,
#                           map_token="",
#                           axis1="", axis2="",
#                           title="",
#                           subtitle="",
#                           caption=""){
#   clean_sf <- prep_data(sf, col1, col2, bivariate_color_scale)
#   legend_custom <- create_legend(bivariate_color_scale, col1, col2, axis1=axis1,axis2=axis2)
#   map <- basemap_ggplot(st_bbox(clean_sf), map_service="carto", 
#                         map_type = "light_no_labels",map_res = 1,
#                         force=T) +
#     # color municipalities according to their combination
#     geom_sf(
#       data=clean_sf,
#       aes(fill = fill),
#       # use thin white stroke for municipalities
#       color = "white",
#       size = 0.1) +
#     # as the sf object clean_sf has a column with name "fill" that
#     # contains the literal color as hex code for each municipality, we can use
#     # scale_fill_identity here
#     scale_fill_identity() +
#     # add titles
#     labs(x = NULL,
#          y = NULL,
#          title = title,
#          subtitle = subtitle,
#          caption = caption) +
#     # add the theme
#     theme_map()
#   final_plot <- ggdraw() +
#     draw_plot(map, 0, 0, 1, 1) +
#     draw_plot(legend_custom, 0.75, 0.15, 0.18, 0.18)
#   ggsave(plot = final_plot, 
#          filename = "reports/figures/map_grid_ps_vegetation.png",
#          width = 7,
#          height = 7,
#          units = c("in"))
# }
# 
# bivariate_color_scale <- tibble(
#   "3 - 3" = "#383b38", # high-high 
#   "2 - 3" = "#54436c",
#   "1 - 3" = "#704b9e", # low-high
#   "3 - 2" = "#516f41",
#   "2 - 2" = "#797f7f", # medium-medium
#   "1 - 2" = "#a18eb9",
#   "3 - 1" = "#6aa64b", # high-low
#   "2 - 1" = "#9fbd91",
#   "1 - 1" = "#d3d3d3" # low-low
# ) 
# 
# map_bivariate(hex_grid_joined,
#               "distance", 
#               "pedal_cycles",
#               bivariate_color_scale,
#               map_token = Sys.getenv("MAP_TOKEN"),
#               axis1="More greenery →", 
#               axis2="More cyclists →",
#               title="Propensity score of greenery and cyclists count in London",
#               subtitle="between 2008-2020 (1km grid)",
#               caption="")
# 
# 
# 
# 
# 
