library(ggplot2)
library(tidyr)
library(dplyr)

# Function to round to n significant digits
round_to_n <- function(x, n = 2) {
  shift <- 10^(n - 1 - floor(log10(abs(x))))
  return(round(x * shift) / shift)
}

clean_var_name <- function(var_name){
    # Removing _binary, ss_, and _
    cleaned_var_name <- gsub("_binary", "", var_name)
    cleaned_var_name <- gsub("ss_", "", cleaned_var_name)
    cleaned_var_name <- gsub("_", " ", cleaned_var_name) 
    return(cleaned_var_name)
}

plot_step <- function(file_path, ind_var_name, figure_dir) {
  df <- read.csv(file_path) %>%
    mutate(variable = as.factor(variable)) %>%
    gather(., type, value, point_estimate:p_value, factor_key = TRUE)

  # Determine the dynamic adjustment factor based on the maxima of both groups
  max_point_estimate <- max(abs(df$value[df$type == "point_estimate"]), na.rm = TRUE)
  max_p_value <- max(df$value[df$type == "p_value"], na.rm = TRUE)
  adjustment_factor <- max_point_estimate / max_p_value

  # Adjust the value only for p_value type using the dynamic factor
  df <- df %>%
    mutate(value_adj = ifelse(type == "p_value", value * adjustment_factor, value))

  # Calculate raw interval
  raw_interval <- max_p_value / 4
  
  # Round the raw interval to the nearest significant figure
  rounding_factor <- 10^(floor(-log10(raw_interval)))
  by_value <- round(raw_interval * rounding_factor) / rounding_factor

  # If by_value is very close to zero, adjust it
  if (by_value < .Machine$double.eps) {
    by_value <- max_p_value / 3
  }

  # Ensure by_value does not exceed max_p_value
  by_value <- min(by_value, max_p_value)

  # Create breaks using the adjusted by_value
  breaks <- seq(0, max_p_value, by = by_value)

  # If max_p_value is not part of the sequence, append it
  if (tail(breaks, 1) != max_p_value) {
    breaks <- c(breaks, max_p_value)
  }

  # Round breaks to two significant digits
  breaks <- round_to_n(breaks)

  # Breaks and labels for the secondary axis
  sec_breaks <- breaks * adjustment_factor
  sec_labels <- format(breaks, nsmall = max(-floor(log10(by_value)), 0))

  plot <- ggplot(data = df, aes(x = variable, y = value_adj, fill = type)) +
    geom_col(position = "dodge") +
    scale_y_continuous(
      name = "Point Estimate",
      sec.axis = sec_axis(trans = ~ ., name = "P Value", breaks = sec_breaks, labels = sec_labels),
      minor_breaks = NULL
    ) +
    scale_fill_manual("", values = c("#7B52AE", "#74B652")) +
    theme_ipsum() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(title = paste0("Step-wise model result for ", clean_var_name(ind_var_name)))

  ggsave(paste0(figure_dir, "/", ind_var_name, "/", gsub(".csv", ".png", basename(file_path))), width = 8, height = 4)
}

plot_importance <- function(file_path, ind_var_name, figure_dir, top_n=10){
  df <- read.csv(file_path) %>% 
    slice_max(variable_importance,n=top_n)
  ggplot(data=df, aes(x=reorder(var_name, variable_importance), y=variable_importance)) +
    geom_bar(stat="identity", fill = "#7B52AE", width=0.4)+
    coord_flip() +
    theme_ipsum() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size=10),
          plot.title = element_text(size=10,hjust=0.5),
          plot.title.position="plot") +
    labs(title=paste0("Variable importance \n when estimating ", clean_var_name(ind_var_name), " (top ", as.character(top_n), ")"),
         x="Variable names",
         y="Variable importance")
  ggsave(paste0(figure_dir, "/", ind_var_name, "/",str_replace(basename(file_path),".csv",".png")), width = 4, height = 5)
}

plot_cate_rank <- function(ind_var_name, forest.ate, figure_dir){
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
    labs(title=paste0("Average CATT within each ranking \n for ", clean_var_name(ind_var_name)))+
    theme_ipsum() +
    theme(plot.title = element_text(hjust=0.5),
          plot.title.position="plot")
  ggsave(paste0(figure_dir, "/", ind_var_name, "/rank_catt.png"), width = 5, height = 5)
}

plot_cate_heatmap <- function(df, ind_var_name, figure_dir) {
  # plot heatmap
  ggplot(df) +
    aes(ranking, covariate) +
    geom_tile(aes(fill = scaling), color = "white", size = 1) +
    geom_text(aes(label = labels), size = 12 / .pt) +
    scale_fill_gradient(low = "#FFFFFF", high = "#7B52AE", limits = c(0, 1), breaks = seq(0, 1, .25)) +
    ggtitle(paste0()) +
    labs(
      title = paste0("Average covariate values within group \n for ", ind_var_name),
      x = "CATE estimate ranking",
      y = "",
      fill = "Scaling"
    ) +
    theme_ipsum() +
    theme(
      axis.text.x = element_text(size = 20),
      axis.text.y = element_text(size = 20),
      plot.title = element_text(size = 45, hjust = 0.5),
      plot.title.position = "plot"
    )
  ggsave(paste0(figure_dir, "/", ind_var_name, "/rank_cate_covariates.png"), width = 10, height = 15)
}

plot_hte_covariate <- function(ind_var_name, model_dir, figure_dir) {
  # load the results
  hte_df <- read.csv(paste0(model_dir, "/", ind_var_name, "/hte_by_covariate.csv"))
  data_pred <- hte_df %>%
    mutate(
      ci_low = estimate - (qnorm(0.95) * std.err),
      ci_high = estimate + (qnorm(0.95) * std.err),
      category = as.factor(category)
    )

  # filter covariates
  covariates_selected <- top_n_diff(hte_df)
  data_pred_all <- data_pred
  data_pred <- data_pred %>%
    dplyr::filter(covariate %in% covariates_selected)

  # Define colors for the three groups
  group_colors <- c("0-33%" = "#7B52AE", "33-66%" = "#FFC107", "66-100%" = "#74B652")

  # Plot predictions for each group and 95% confidence intervals around them.
  ggplot(data_pred) +
    geom_point(aes(x = category, y = estimate, color = category), size = 1) +
    geom_errorbar(aes(x = category, ymin = ci_low, ymax = ci_high, color = category, width = .2), size = 1) +
    geom_text(aes(x = category, y = estimate, label = round(estimate, 2)),
      hjust = .5, vjust = -.5, size = 4.7
    ) +
    coord_flip() +
    labs(
      title = paste0("Subgroup CATE for ", clean_var_name(ind_var_name)),
      x = NULL,
      y = "",
      fill = ""
    ) +
    theme_ipsum() +
    theme(
      axis.text.x = element_text(size = 14),
      axis.text.y = element_blank(),
      plot.title = element_text(size = 20, hjust = 0.5),
      plot.title.position = "plot",
      strip.text.y.left = element_text(angle = 0, size = 14),
      legend.text = element_text(size = 14), # Increase size of legend text
      legend.title = element_text(size = 14), # Increase size of legend title
      legend.key.size = unit(0.5, "cm")
    ) +
    scale_color_manual(values = group_colors) +
    facet_grid(covariate ~ ., switch = "y")
  ggsave(paste0(figure_dir, "/", ind_var_name, "/hte_by_covariate.png"), width = 6, height = 7.5)

  # Plot predictions for all groups and 95% confidence intervals around them.
  ggplot(data_pred_all) +
    geom_point(aes(x = category, y = estimate, color = category), size = 1) +
    geom_errorbar(aes(x = category, ymin = ci_low, ymax = ci_high, color = category, width = .2), size = 1) +
    geom_text(aes(x = category, y = estimate, label = round(estimate, 2)),
      hjust = .5, vjust = -.5, size = 2
    ) +
    coord_flip() +
    labs(
      title = paste0("Subgroup CATE for ", ind_var_name),
      x = NULL,
      y = "",
      fill = ""
    ) +
    theme_ipsum() +
    theme(
      axis.text.x = element_text(size = 14),
      axis.text.y = element_blank(),
      plot.title = element_text(size = 20, hjust = 0.5),
      plot.title.position = "plot",
      strip.text.y.left = element_text(angle = 0, size = 14),
      legend.text = element_text(size = 14), # Increase size of legend text
      legend.title = element_text(size = 14), # Increase size of legend title
      legend.key.size = unit(0.5, "cm")
    ) +
    scale_color_manual(values = group_colors) +
    facet_grid(covariate ~ ., switch = "y")
  ggsave(paste0(figure_dir, "/", ind_var_name, "/hte_by_covariate_all.png"), width = 6, height = 15)
}


plot_hte_ranking <- function(ind_var_name, model_dir, figure_dir) {
  cf_preds <- read.csv(paste0(model_dir, "/", ind_var_name, "/", "binary_predictions.csv"))
  ggplot(mapping = aes(
    x = rank(cf_preds$predictions),
    y = cf_preds$predictions
  )) +
    geom_errorbar(
      mapping = aes(
        ymin = cf_preds$predictions + 1.96 * sqrt(cf_preds$variance.estimates),
        ymax = cf_preds$predictions - 1.96 * sqrt(cf_preds$variance.estimates)
      ),
      width = 0.05, colour = "grey"
    ) +
    geom_point(size = 2, colour = "#7B52AE") +
    # geom_hline(yintercept = c(0), colour = "black", linetype = "dashed", size = 3) +
    geom_segment(aes(x = 0, xend = max(rank(cf_preds$predictions)), y = 0, yend = 0),
      colour = "black", linetype = "dashed", size = 3
    ) +
    labs(
      title = paste0("Heterogeneous Treatment Effects by Ranking for ", ind_var_name),
      x = "Rank",
      y = "Estimated Treatment Effect"
    ) +
    theme_ipsum() +
    theme(
      axis.text.x = element_text(size = 50),
      axis.text.y = element_text(size = 50),
      axis.title.x = element_text(size = 40),
      axis.title.y = element_text(size = 40, vjust = 3),
      plot.title = element_text(size = 60, hjust = 0.5),
      # plot.margin = unit(c(0.1,0.1,0.1,0.1),"inch")
    )
  # theme(panel.grid.major.x = element_blank(),
  #       panel.grid.minor.x = element_blank(),
  #       panel.grid.major.y = element_blank(),
  #       panel.grid.minor.y = element_blank())
  ggsave(paste0(figure_dir, "/", ind_var_name, "/hte_by_ranking.png"), width = 30, height = 10)
}

top_n_diff <- function(hte_df, top_n = 4, max_standard_error = 5) {
  # Group by covariate and get max std.err and filter out those with max std.err > max_standard_error
  hte_df <- hte_df %>%
    group_by(covariate) %>%
    mutate(max_std_err = max(std.err)) %>%
    ungroup() %>%
    filter(max_std_err <= max_standard_error)
  
  # Pivot the data for estimates
  estimate_df <- hte_df %>%
      dplyr::select(c("covariate", "category", "estimate")) %>%
      spread(key = category, value = estimate)

  # Pivot the data for standard errors
  std_err_df <- hte_df %>%
      dplyr::select(covariate, category, std.err) %>%
      spread(key = category, value = std.err)

  # Calculate differences and SE for pairs
  combined_df <- left_join(estimate_df, std_err_df, by = "covariate", suffix = c("_est", "_se")) %>%
      ungroup() %>%
      mutate(
          diff_0_33_33_66 = `33-66%_est` - `0-33%_est`,
          SE_diff_0_33_33_66 = sqrt(`0-33%_se`^2 + `33-66%_se`^2),
          diff_0_33_66_100 = `66-100%_est` - `0-33%_est`,
          SE_diff_0_33_66_100 = sqrt(`0-33%_se`^2 + `66-100%_se`^2),
          diff_33_66_66_100 = `66-100%_est` - `33-66%_est`,
          SE_diff_33_66_66_100 = sqrt(`33-66%_se`^2 + `66-100%_se`^2)
      )
  
  # Calculate p-values for differences
  combined_df <- combined_df %>%
      rowwise() %>%
      mutate(
          p_0_33_33_66 = 2 * (1 - pnorm(abs(diff_0_33_33_66 / SE_diff_0_33_33_66))),
          p_0_33_66_100 = 2 * (1 - pnorm(abs(diff_0_33_66_100 / SE_diff_0_33_66_100))),
          p_33_66_66_100 = 2 * (1 - pnorm(abs(diff_33_66_66_100 / SE_diff_33_66_66_100)))
      )

  # Rank covariates by smallest p-value and select top n
  top_n_covariates <- combined_df %>%
      dplyr::select(covariate, starts_with("p_")) %>%
      gather(key = "p_value_type", value = "p_value", starts_with("p_")) %>%
      group_by(covariate) %>%
      summarise(min_p_value = min(p_value)) %>%
      arrange(min_p_value) %>%
      head(top_n)
  print(top_n_covariates)
  return(top_n_covariates$covariate)
}


# Function to determine color for the cell
determine_color <- function(before, after, metric_name) {
  if (metric_name == "variance ratio") {
    if (abs(after - 1) < abs(before - 1)) {
      return("\\cellcolor{customgreen}")
    }
  } else {
    if ((is.na(after)) | (is.na(before))) {
      return("")
    } else if (after < before) {
      return("\\cellcolor{customgreen}")
    }
  }
  return("\\cellcolor{custompurple}")
}

# Function to generate LaTeX table rows
generate_latex_row <- function(city, variable, metrics) {
  before <- metrics$before
  after <- metrics$after
  latex_rows <- c()
  for (metric_name in names(before)) {
    color <- determine_color(before[[metric_name]], after[[metric_name]], metric_name)
    row <- sprintf("%s & %s & %s & %.4f & %s %.4f \\\\", city, variable, metric_name, before[[metric_name]], color, after[[metric_name]])
    latex_rows <- c(latex_rows, row)
    city <- "" # Clear city for subsequent rows
    variable <- "" # Clear variable for subsequent rows
  }
  return(latex_rows)
}

# Function to extract metrics from content
extract_values <- function(content) {
  before_matching <- list()
  after_matching <- list()

  # Locate the "Summary of Balance for All Data" and "Summary of Balance for Matched Data" sections
  before_section_start <- which(grepl("Summary of Balance for All Data:", content))
  after_section_start <- which(grepl("Summary of Balance for Matched Data:", content))

  # Extract values from the relevant sections
  distance_line_before <- which(grepl("distance", content[before_section_start:after_section_start])) + before_section_start - 1
  distance_line_after <- which(grepl("distance", content[after_section_start:length(content)])) + after_section_start - 1

  # If multiple occurrences, take the first one
  distance_line_before <- distance_line_before[1]
  distance_line_after <- distance_line_after[1]

  # If distance_line_before is empty, then fill before_matching with NAs
  if (length(distance_line_before) == 0) {
    before_matching$`standard mean difference` <- NA
    before_matching$`variance ratio` <- NA
    before_matching$`eCDF mean` <- NA
    before_matching$`eCDF max` <- NA
  } else {
    values_before <- unlist(str_extract_all(content[distance_line_before], "\\d+\\.\\d+"))
    before_matching$`standard mean difference` <- as.numeric(values_before[3])
    before_matching$`variance ratio` <- as.numeric(values_before[4])
    before_matching$`eCDF mean` <- as.numeric(values_before[5])
    before_matching$`eCDF max` <- as.numeric(values_before[6])
  }

  values_after <- unlist(str_extract_all(content[distance_line_after], "\\d+\\.\\d+"))
  after_matching$`standard mean difference` <- as.numeric(values_after[3])
  after_matching$`variance ratio` <- as.numeric(values_after[4])
  after_matching$`eCDF mean` <- as.numeric(values_after[5])
  after_matching$`eCDF max` <- as.numeric(values_after[6])

  return(list(before = before_matching, after = after_matching))
}

# Function to determine asterisks based on p-value
get_asterisks <- function(p_value) {
  if (p_value < 0.01) {
    return("***")
  } else if (p_value < 0.05) {
    return("**")
  } else if (p_value < 0.1) {
    return("*")
  } else {
    return("")
  }
}

# Updated function to extract values and include asterisks
extract_cf_values_from_content <- function(content) {
  values <- str_split(content, " ")[[2]]
  values <- values[values != ""] # Remove any empty strings

  estimate <- signif(as.numeric(values[1]), 3)
  std_err <- signif(as.numeric(values[2]), 3)
  p_value <- as.numeric(values[3])
  asterisks <- get_asterisks(p_value)
  return(list(estimate = paste0(estimate, asterisks), std_err = std_err, p_value = p_value))
}

# Function to extract AUTOC and 95% CI from content
extract_autoc_and_ci_R <- function(content_line) {
  # Split by spaces and filter out non-numeric values
  parts <- unlist(strsplit(content_line, " "))
  numbers <- as.numeric(gsub("[^0-9.]", "", parts[grep("[0-9.]", parts)]))
  autoc <- numbers[2]
  ci <- numbers[3]

  return(list(autoc = autoc, ci = ci))
}

map_grid <- function(grid, value, figure_dir) {
  # set up color and breaks
  my_palette <- colorRampPalette(c("#62428b", "#FFFFFF", "#5d9242"))

  # Create the color scale
  color_scale <- my_palette(100)
  # map grid with random values as fill
  map_grid <- ggplot(grid) +
    geom_sf(aes(fill = .data[[value]]), color = NA) +
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
  ggsave(paste0(figure_dir, "/", ind_var_name, "/grid_map.png"), map_grid, width = 6, height = 6)
}