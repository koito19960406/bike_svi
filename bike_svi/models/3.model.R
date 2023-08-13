pacman::p_load(tidyverse, stats, plm, utils,pglm,progress,MatchIt,lmtest,sandwich,
               pscl, cobalt, grf,AER,DiagrammeRsvg,rsvg,stargazer,hrbrthemes,Hmisc,
               WeightIt,gbm,CBPS,caret,car,notifier,corrplot,randomForest,pdp,doMC,
               doParallel, here, DiagrammeR, dotenv)
source(here("bike_svi/models/utils.R"))

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
  
  # set treatment variable
  # pairwise correlation for count and segmentation result
  if (city=="London"){
    ss_var_list <- c("ss_vegetation_binary", "ss_bike_lane_binary",  "ss_parking_binary",  "ss_street_light_binary")
  } else if (city=="Montreal"){
    ss_var_list <- c("ss_vegetation_binary", "ss_guard_rail_binary", "ss_sidewalk_binary",  "ss_street_light_binary")
  }
  treatment_var_list <- c(ss_var_list, "slope_binary")
  treatment_var_list_collapsed <- paste(treatment_var_list, collapse="|")
  
  # load data
  all_var <- read.csv(paste0(processed_dir, "/all_var_joined.csv")) %>% 
    mutate(
      count=as.integer(count),
      year=as.numeric(year)) %>% 
    dplyr::select(-c(age_60_90,lu_others,ss_vegetation, ss_bike_lane, 
                     ss_bike_rack, ss_curb, ss_curb_cut, ss_parking,
                     ss_pothole, ss_street_light, ss_guard_rail, 
                     ss_pedestrian_area, ss_sidewalk, ss_bench
                     )) %>% 
    drop_na() %>% 
    dplyr::select(-one_of(unlist(treatment_var_list)),
                  -contains("_binary"), one_of(treatment_var_list)) %>% 
    remove_highly_correlated(threshold = 0.9) %>% 
    mutate(year=as.factor(year))
  
  # background check --------------------------------------------------------
  # check overdispersion
  covariates <- names(all_var)[!str_detect(names(all_var), 
                                           paste("\\bcount\\b", "count_log", "count_point_id", "year", "binary", sep="|"))]
  # # OD test
  # covariates_pasted <- paste(covariates, collapse = " + ")
  # formula <- as.formula(paste("count", " ~ ", covariates_pasted))
  # print(formula)
  # poisson_base <- glm(formula, data = all_var, family = poisson)
  # od_test <- dispersiontest(poisson_base,trafo=1)
  # capture.output(od_test, file= paste0(model_dir, "/overdispersion_test.txt"))
  # 
  # # run simple OLS
  # simple_ols <- glm(formula, data = all_var)
  # simple_ols_summary <- summary(simple_ols)
  # capture.output(simple_ols_summary, file= paste0(model_dir, "/simple_ols.txt"))
  # 
  # # run pooled ZINB
  # zero_condition_covariates <- paste(covariates[str_detect(covariates,paste("od_","pop_den","poi",sep="|"))],
  #                                   collapse = " + ")
  # formula <- as.formula(paste("count", " ~ ", covariates_pasted,"|",zero_condition_covariates))
  # pooled_zinb <- zeroinfl(formula, dist = "negbin", link = "logit", data = all_var)
  # pooled_zinb_summary <- summary(pooled_zinb)
  # capture.output(pooled_zinb_summary, file= paste0(model_dir, "/pooled_zinb.txt"))

  # run FE ZINB
  covariates <- c("year",covariates)
  # covariates_pasted <- paste(covariates, collapse = " + ")
  # formula <- as.formula(paste("count", " ~ ", covariates_pasted,"|",zero_condition_covariates))
  # fe_zinb <- zeroinfl(formula, dist = "negbin", link = "logit", data = all_var)
  # fe_zinb_summary <- summary(fe_zinb)
  # capture.output(fe_zinb_summary, file= paste0(model_dir, "/fe_zinb.txt"))
  
  # run FE NB
  for (treatment_var in treatment_var_list){
    covariates_pasted <- paste(c(covariates, treatment_var), collapse = " + ")
    
    formula <- as.formula(paste("count", " ~ ", covariates_pasted))
    fe_nb <- glm.nb(formula, data = all_var)
    fe_nb_summary <- summary(fe_nb)
    capture.output(fe_nb_summary, file= paste0(model_dir, "/", treatment_var, "/fe_nb.txt"))
  }
  
  # # fe poisson by step ------------------------------------------------------
  # # list of baseline control variables
  # control_var_vec <- names(all_var)[str_detect(names(all_var), "^age_|year|^lu_")]
  # for (treatment_var in treatment_var_list){
  #   # run_fe_poisson(all_var_scaled, "count_log",ind_var_name, model_dir)
  #   # run_zero_inflated(all_var_scaled, "count",ind_var_name, control_var_vec, "poisson")
  #   # run_zero_inflated(all_var, "count",treatment_var, control_var_vec, "negbin", model_dir)
  #   run_negative_binomial(all_var %>% dplyr::select(-c("slope", "count_log.1")), "count",treatment_var, control_var_vec, model_dir)
  # }

  # # psm ---------------------------------------------------------------------
  # model_list <- list()
  # pb <- progress_bar$new(total = length(treatment_var_list))
  # for (treatment_var in treatment_var_list){
  #   pb$tick()
  #   print(treatment_var)
  #   treat_var_wo_binary <- sub("_binary", "", treatment_var)
  #   covariates_temp <- covariates[!str_detect(covariates, treat_var_wo_binary)]
  #   models <- run_psm_nb(all_var, "count", treatment_var, covariates_temp, model_dir, figure_dir)
  #   model_list[[treatment_var]] <- models
  # }
  # 
  # # create stargazer
  # for (stage in c("first_stage","second_stage")){
  #   print(stage)
  # 
  #   # loop through treatment variables
  #   models_se <- lapply(names(model_list), function(treatment_var) {
  #     model <- model_list[[treatment_var]][[stage]]
  #     if (stage=="second_stage"){
  #       cluster=c("subclass")
  #       model_se <- as.vector(summary(model,cluster = cluster)$coefficients[,"Std. Error"])
  #     } else{
  #       cluster=c()
  #       model_se <- as.vector(summary(model,cluster = cluster)$coefficients[,"Std. Error"])
  #     }
  #     return(list(model=model, se=model_se))
  #   })
  # 
  #   # Extract models and standard errors
  #   models <- lapply(models_se, function(x) x$model)
  #   se <- lapply(models_se, function(x) x$se)
  # 
  #   stargazer(models,
  #             se=se,
  #             single.row = TRUE,
  #             column.sep.width = "1pt",
  #             no.space = TRUE,
  #             font.size = "small",
  #             type = "latex",
  #             out = paste0(model_dir, "/psm_", stage,".tex")
  #   )
  # }
  
  # # random & causal forest -----------------------------------------------------------
  # all_var <- read.csv(paste0(processed_dir, "/all_var_joined.csv")) %>%
  #   mutate(year=as.numeric(year)) %>%
  #   dplyr::select(-c(age_60_90,lu_others)) %>%
  #   drop_na()
  # 
  # pb <- progress_bar$new(total = length(treatment_var_list))
  # for (treatment_var in treatment_var_list){
  #   pb$tick()
  #   treat_var_wo_binary <- sub("_binary", "", treatment_var)
  #   covariates_temp <- covariates[!str_detect(covariates, treat_var_wo_binary)]
  #   # random forest
  #   run_random_forest(all_var, "count_log",treatment_var, covariates_temp, model_dir)
  #   # causal forest
  #   run_causal_forest(all_var, "count_log",treatment_var, covariates_temp, "binary")
  #   compute_rate(all_var, "count_log",treatment_var, covariates_temp, "binary", model_dir, figure_dir)
  #   df_list <- compute_cate_ranking(all_var, "count_log", treatment_var, covariates_temp)
  #   df_list$forest.ate %>%
  #     write.csv(paste0(model_dir, "/", treatment_var,"/rank_cate.csv"), row.names = F)
  #   df_list$df %>%
  #     write.csv(paste0(model_dir, "/", treatment_var ,"/rank_cate_covariates.csv"), row.names = F)
  #   hte_df <- compute_hte_subgroups(all_var, "count_log", treatment_var, covariates_temp)
  #   write.csv(hte_df,paste0(model_dir, "/", treatment_var,"/hte_by_covariate.csv"), row.names = F)
  #   compute_overall_var_imp(all_var, "binary", model_dir)
  # }
}
  