pacman::p_load(tidyverse, ggplot2, hrbrthemes, extrafont, sf, dotenv, here, progress)
source(here("bike_svi/visualization/utils.R"))

# load data ---------------------------------------------------------------
load_dot_env()
root_dir <- Sys.getenv("ROOT_DIR")
if (!file.exists(root_dir)){
  root_dir <- here()
}
# set seed
set.seed(1234)
# load external/city_list.txt to get the list of cities
city_list <- read.csv(paste0(root_dir,"/data/external/city_list.txt"), header = FALSE, sep = "\t")
city_list <- city_list %>% 
  rename(city = V1) %>% 
  mutate(city = str_replace_all(city, " ", "_")) %>% 
  as.vector() %>% 
  unlist()

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
  
  # load data
  all_var <- read.csv(paste0(processed_dir, "/all_var_joined.csv")) %>% 
    mutate(
      count=as.integer(count),
      year=as.factor(year)) %>% 
    dplyr::select(-c(age_60_90,lu_others,ss_vegetation, ss_bike_lane, 
                     ss_bike_rack, ss_curb, ss_curb_cut, ss_parking,
                     ss_pothole, ss_street_light, ss_guard_rail, 
                     ss_pedestrian_area, ss_sidewalk, ss_bench
    )) %>% 
    drop_na()
  
  # set treatment variable
  # pairwise correlation for count and segmentation result
  if (city=="London"){
    ss_var_list <- c("ss_vegetation_binary", "ss_bike_lane_binary",  "ss_parking_binary",  "ss_street_light_binary")
  } else if (city=="Montreal"){
    ss_var_list <- c("ss_vegetation_binary", "ss_guard_rail_binary",  "ss_sidewalk_binary",  "ss_street_light_binary")
  }
  treatment_var_list <- c(ss_var_list, "slope_binary")
  
  
#   # latex table for PSM balance ---------------------------------------------
#   all_latex_rows <- c()
#   count <- 0
#   for (variable_name in treatment_var_list) {
#     file_path <- paste0(model_dir,"/", variable_name, "/", variable_name, "_summary_balance.txt")
#     content <- readLines(file_path)
#     metrics <- extract_values(content)
#     variable_display_name <- gsub("ss_", "", variable_name) # Removing the 'ss_' prefix for display
#     variable_display_name <- gsub("_binary", "", variable_display_name) # Removing the '_binary' suffix for display
#     variable_display_name <- gsub("_", " ", variable_display_name) # Replaceing the '_' with ' '
#     if (count > 0){
#       all_latex_rows <- c(all_latex_rows, generate_latex_row("", variable_display_name, metrics))
#     }
#     else{
#       all_latex_rows <- c(all_latex_rows, generate_latex_row(city, variable_display_name, metrics))
#     }
#     count <- count + 1
#   }
#   
#   # Constructing the full table
#   table_header <- "\\begin{table}[!htp]\\centering
# \\caption{The balance of data distribution between treated and control units before and after matching.}\\label{result:tab:match_result}
# \\scriptsize
# \\begin{tabular}{llrrr}\\toprule
# City&Variables&Metrics&Before the matching &After the matching \\\\\\midrule
# "
#   table_footer <- "\\bottomrule
# \\end{tabular}
# \\end{table}
# "
#   latex_table <- paste(table_header, paste(all_latex_rows, collapse = "\n"), table_footer, sep = "\n")
#   
#   cat(latex_table)
#   # Save the generated LaTeX table to a .tex file
#   write(latex_table, paste0(model_dir,"/psm_balance_table.tex"))
  

  # # latex table for causal forest results -----------------------------------
  # # Extract values for each variable and generate LaTeX table rows
  # latex_rows <- c()
  # count <- 0
  # for (variable_name in treatment_var_list) {
  #   file_path <- paste0(model_dir, "/", variable_name, "/binary_causal_forest_cate.txt")
  #   if (file.exists(file_path)) {
  #     content <- readLines(file_path)
  #     values <- extract_cf_values_from_content(content)
  #     # Create LaTeX row for the variable
  #     if (count > 0){
  #       city_temp <- ""
  #     } else{
  #       city_temp <- city
  #     }
  #     latex_row <- sprintf("%s & %s & %s & %s \\\\",
  #                          city_temp,
  #                          gsub("ss_", "", gsub("_binary", "", gsub("_", " ", variable_name))),
  #                          values$estimate, values$std_err)
  #     latex_rows <- c(latex_rows, latex_row)
  #     count <- count + 1
  #   }
  # }
  # 
  # # Generate the final LaTeX table
  # latex_table <- paste(
  #   "\\begin{table}[!htp]\\centering",
  #   "\\caption{CATT of the treatments. The values were rounded to three significant figures.}\\label{result:tab:cf}",
  #   "\\scriptsize",
  #   "\\begin{tabular}{lrrrr}\\toprule",
  #   "City&Variable&Estimate &Standard error \\\\ \\midrule",
  #   paste(latex_rows, collapse = "\n"),
  #   "\\hline",
  #   "\\hline \\\\[-1.8ex]",
  #   "\\textit{Note:}  & \\multicolumn{3}{r}{$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01} \\\\",
  #   "\\end{tabular}",
  #   "\\end{table}",
  #   sep = "\n"
  # )
  # 
  # cat(latex_table)
  # # Save the generated LaTeX table to a .tex file
  # write(latex_table, paste0(model_dir,"/causal_forest_table.tex"))
  


  # # latex table for autoc ---------------------------------------------------
  # # Initialize an empty list to hold the rows for the LaTeX table
  # latex_rows <- c()
  # count <- 0
  # # Loop through the files, extract values, and create LaTeX rows
  # for (variable_name in treatment_var_list) {
  #     file_path <- paste0(model_dir, "/", variable_name, "/binary_autoc.txt")
  #     if (file.exists(file_path)) {
  #       content <- readLines(file_path, , n = 1)
  #       values <- extract_autoc_and_ci_R(content)
  #       # Create LaTeX row for the variable
  #       if (count > 0){
  #         city_temp <- ""
  #       } else{
  #         city_temp <- city
  #       }
  #       latex_row <- sprintf("%s & %s & %.2f & %.2f \\\\", city_temp, str_replace(str_replace(str_replace(variable_name, "ss_", ""), 
  #                                                                                             "_binary", ""), "_", " "), values$autoc, values$ci)
  #       latex_rows <- c(latex_rows, latex_row)
  #       count <- count + 1
  #     }
  #   }
  # 
  # # Combine the rows to form the final LaTeX table
  # latex_table_R <- paste(
  #   "\\begin{table}[!htp]\\centering",
  #   "\\caption{This table shows the area under targeting operator characteristics (AUTOC) and 95\% confidence interval (CI).}",
  #   "\\label{result:tab:autoc}",
  #   "\\scriptsize",
  #   "\\begin{tabular}{llrr}\\toprule",
  #   "City & Variable & AUTOC & 95\% CI \\\\\\midrule",
  #   paste(latex_rows, collapse = "\n"),
  #   "\\bottomrule",
  #   "\\end{tabular}",
  #   "\\end{table}",
  #   sep = "\n"
  # )
  # cat(latex_table_R)
  # # Save the LaTeX table to a .tex file
  # write(latex_table_R, file = paste0(model_dir, "/autoc.tex"))
  
  # # plot the overall variable importance --------------------------------------
  # df <- read.csv(paste0(model_dir, "/binary_causal_forest_var_imp.csv"))%>%
  #   slice_max(variable_importance,n=10)
  # plot <-ggplot(data=df, aes(x=reorder(var_name, variable_importance), y=variable_importance)) +
  #   geom_bar(stat="identity", fill = "#7B52AE", width=0.4) +
  #   coord_flip() +
  #   theme_ipsum() +
  #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
  #         axis.text.y = element_text(size=10),
  #         plot.title = element_text(size=10, hjust=0.5),
  #         plot.title.position="plot") +
  #   labs(title=paste0("Overall variable importance", " (top ", as.character(10), ")"),
  #        x="Variable names",
  #        y="Variable importance")
  # ggsave(paste0(figure_dir, "/overall_variable_importance.png"), width = 4, height = 5)

  pb <- progress_bar$new(total = length(treatment_var_list))
  for (treatment_var in treatment_var_list){
    print(treatment_var)
    pb$tick()

    # # stepwise model results --------------------------------------------------
    # file_path <- paste0(model_dir, "/",treatment_var,"/", "year_negative_binomial_result.csv")
    # plot_step(file_path, treatment_var, figure_dir)

    # # causal forest feature importance ----------------------------------------
    # file_path <- paste0(model_dir, "/",treatment_var,"/", "binary_causal_forest_var_imp.csv")
    # plot_importance(file_path, treatment_var, figure_dir)
    #
    # # plot CATE average heatmap -----------------------------------------------
    # forest.ate <- read_csv(paste0(model_dir, "/",treatment_var,"/rank_cate.csv"))
    # plot_cate_rank(treatment_var, forest.ate, figure_dir)
    #
    # # plot cate heatmap -------------------------------------------------------
    # df <- read_csv(paste0(model_dir, "/",treatment_var,"/rank_cate_covariates.csv"))
    # plot_cate_heatmap(df, treatment_var, figure_dir)
    #
    # plot HTE by covariate ---------------------------------------------------
    plot_hte_covariate(treatment_var, model_dir, figure_dir)
    #
    # plot HTE by ranking -----------------------------------------------------
  #   plot_hte_ranking(treatment_var, model_dir, figure_dir)
  }
}