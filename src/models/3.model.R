pacman::p_load(tidyverse, stats, plm, utils,pglm,progress,MatchIt,lmtest,sandwich,pscl)


# fe poisson by step ------------------------------------------------------
root_dir <- "/Volumes/ExFAT/bike_svi"
if (!(file.exists(root_dir))){
  root_dir <- "/Volumes/Extreme SSD/bike_svi"
}
all_var_scaled_binary_treatment <- read.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined_scaled_binary.csv")) %>% 
  mutate(year=as.factor(year)) %>% 
  drop_na()
run_fe_poisson <- function(data, dep_var_name, ind_var_name,){
  # create dir
  dir.create(paste0("bike_svi/models/",ind_var_name))
  # run base model
  formula <- as.formula(paste(dep_var_name, " ~ ", ind_var_name))
  print(formula)
  model_year_fe_poisson <- model_year_fe_poisson <- glm(formula, family = poisson(), data = data)
  model_year_fe_poisson_summary <- summary(model_year_fe_poisson)
  capture.output(model_year_fe_poisson_summary, file= paste0("bike_svi/models/",ind_var_name,"/", "year_fe_poisson_base.txt"))
  # add other covariates one by one
  data_covar <- data %>% 
    select(-c(ind_var_name,dep_var_name)) %>% 
    select(1:ncol(.))
  cov_names <- names(data_covar)
  names_agg <- c(ind_var_name)
  pb <- progress_bar$new(total = length(cov_names))
  for (cov_name in cov_names){
    pb$tick()
    names_agg <- c(names_agg,cov_name)
    right_side <- paste(names_agg, collapse = " + ")
    formula <- as.formula(paste(dep_var_name, " ~ ", right_side))
    model_year_fe_poisson <- glm(formula, family = poisson(), data = data)
    model_year_fe_poisson_summary <- summary(model_year_fe_poisson)
    capture.output(model_year_fe_poisson_summary, file= paste0("bike_svi/models/",ind_var_name,"/", "year_fe_poisson", as.character(length(names_agg)),".txt"))
  } 
}
run_zero_inflated <- function(data, dep_var_name, ind_var_name, control_var_vec, count_model){
  # create dir
  dir.create(paste0("bike_svi/models/",ind_var_name))
  # run base model
  formula <- as.formula(paste(dep_var_name, " ~ ", ind_var_name))
  print(formula)
  model_year_zero_inflated <- zeroinfl(formula, dist = count_model, link = "logit", data = data)
  model_year_zero_inflated_summary <- summary(model_year_zero_inflated)
  capture.output(model_year_zero_inflated_summary, file= paste0("bike_svi/models/",ind_var_name,"/", "year_zero_inflated_",count_model,"_base.txt"))
  # # add other covariates one by one
  # data_covar <- data %>% 
  #   select(-c(ind_var_name,dep_var_name, control_var_vec)) %>% 
  #   select(1:ncol(.))
  # cov_names <- names(data_covar)
  # names_agg <- c(ind_var_name, control_var_vec)
  # pb <- progress_bar$new(total = length(cov_names))
  # for (cov_name in cov_names){
  #   pb$tick()
  #   names_agg <- c(names_agg,cov_name)
  #   right_side <- paste(names_agg, collapse = " + ")
  #   formula <- as.formula(paste(dep_var_name, " ~ ", right_side))
  #   model_year_zero_inflated <- zeroinfl(formula, dist = count_model, link = "logit", data = data)
  #   model_year_zero_inflated_summary <- summary(model_year_zero_inflated)
  #   capture.output(model_year_zero_inflated_summary, file= paste0("bike_svi/models/",ind_var_name,"/", "year_zero_inflated", as.character(length(names_agg)),".txt"))
  # } 
}
ind_var_name_list <- c("building","sky","vegetation","visual_complexity")
control_var_vec <- c("year","X0_9", "X10_19", "X20_29", "X30_39", "X40_49", "X50_59", "X60_69", "X70_79", "X80_89", "X90")
for (ind_var_name in ind_var_name_list){
  # run_fe_poisson(all_var_scaled, "pedal_cycles_log",ind_var_name)
  run_zero_inflated(all_var, "pedal_cycles",ind_var_name, control_var_vec, "poisson")
  run_zero_inflated(all_var, "pedal_cycles",ind_var_name, control_var_vec, "negbin")
}

# Propensity score matching -----------------------------------------------
run_psm <- function(data,dep_var_name, ind_var_name){
  # check balance
  covariates_df <- data %>% 
    select(c(2,23:36))
  covariates_names <- paste(names(covariates_df), collapse = " + ")
  formula <- as.formula(paste(ind_var_name, " ~ ", covariates_names))
  match_result <- matchit(formula = formula, data = all_var_scaled_binary_treatment, method = "nearest", distance = "glm")
  summary_match_result <- summary(match_result)
  capture.output(summary_match_result, file= paste0("bike_svi/models/",str_remove(ind_var_name,"_binary"),"/summary_balance.txt"))
  # plot
  if (!(file.exists(paste0("bike_svi/reports/figures/",str_remove(ind_var_name,"_binary"),"_match_result.pdf")))){
    pdf(paste0("bike_svi/reports/figures/",str_remove(ind_var_name,"_binary"),"_match_result.pdf"), height = 7, width = 7)
    plot(match_result, type = "jitter", interactive = FALSE)
    dev.off()
  }
  # model
  match_result_df <- match.data(match_result)
  right_side <- paste0(ind_var_name," + ", covariates_names)
  formula <- as.formula(paste(dep_var_name, " ~ ", right_side))
  model_year_fe_poisson <- glm(formula, family = poisson(), data = match_result_df, weights=weights)
  model_year_fe_poisson_summary <- coeftest(model_year_fe_poisson, vcov. = vcovCL, cluster = ~subclass)
  capture.output(model_year_fe_poisson_summary, file= paste0("bike_svi/models/",str_remove(ind_var_name,"_binary"),"/", "psm_year_fe_poisson.txt"))
}
binary_ind_var_name_list <- paste0(ind_var_name_list,"_binary")
pb <- progress_bar$new(total = length(binary_ind_var_name_list))
for (ind_var_name in binary_ind_var_name_list){
  pb$tick()
  run_psm(all_var_scaled_binary_treatment, "pedal_cycles_log",ind_var_name)
}


