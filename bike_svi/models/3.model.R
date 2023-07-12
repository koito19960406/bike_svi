devtools::install_github("gaborcsardi/notifier")

pacman::p_load(tidyverse, stats, plm, utils,pglm,progress,MatchIt,lmtest,sandwich,
               pscl, cobalt, grf,AER,DiagrammeRsvg,rsvg,stargazer,hrbrthemes,Hmisc,
               WeightIt,gbm,CBPS,caret,car,notifier,corrplot,randomForest,pdp,doMC,
               doParallel)
source("utils.R")

# load data ---------------------------------------------------------------
load_dot_env()
root_dir <- Sys.getenv("ROOT_DIR")

# set seed
set.seed(1234)
# load external/city_list.txt to get the list of cities
city_list <- read.csv(paste0(root_dir,"/data/external/city_list.txt"), header = FALSE, sep = "\t")
city_list <- city_list %>% 
  rename(city = V1) %>% 
  mutate(city = str_replace_all(city, " ", "_"))
# loop through the cities
for (city in city_list){
  # create folders for each city
  model_dir <- paste0(root_dir,"/bike_svi/models/", city)
  if (!dir.exists(model_dir)){
    dir.create(model_dir)
  }
  figure_dir <- paste0(root_dir,"/reports/figures/", city)
  if (!dir.exists(figure_dir)){
    dir.create(figure_dir)
  }
  external_dir <- paste0(root_dir,"/data/external/cities/", city)
  processed_dir <- paste0(root_dir,"/data/processed/cities/", city)

  # load data
  all_var_scaled_binary_treatment <- read.csv(paste0(processed_dir, "/all_var_joined_scaled_binary.csv")) %>% 
    mutate(year=as.factor(year)) %>% 
    dplyr::select(-c(age_60_90,lu_others)) %>% 
    drop_na()
  all_var_scaled <- read.csv(paste0(processed_dir, "/all_var_joined_scaled.csv")) %>% 
    mutate(year=as.factor(year)) %>% 
    dplyr::select(-c(age_60_90,lu_others)) %>% 
    drop_na()

  # set treatment variable
  # pairwise correlation for count and segmentation result
  if (city=="London"){
    ss_var_list <- c("ss_vegetation_binary", "ss_bike_lane_binary",  "ss_bike_rack_binary",  "ss_curb_binary",  "ss_curb_cut_binary",  "ss_parking_binary",  "ss_pothole_binary",  "ss_street_light_binary")
  } else if (city=="Montreal"){
    ss_var_list <- c("ss_vegetation_binary", "ss_guard_rail_binary",  "ss_pedestrian_area_binary",  "ss_sidewalk_binary",  "ss_street_light_binary",  "ss_bench_binary")
  }
  treatment_var <- c(ss_var_list, "slope")

  # background check --------------------------------------------------------
  # check overdispersion
    covariates <- names(all_var_scaled_binary_treatment)[!str_detect(names(all_var_scaled_binary_treatment), 
                                                                    paste("count", "count_point_id","ss_bus","ss_bicycle","motorcycle","ss_person","ss_rider",
                                                                          "ss_train", "ss_truck", "ss_car", "ss_building",
                                                                          "^.*binary((?!70_percentile).)*$","year","slope_log$","vegetation$","sidewalk$",
                                                                          sep="|"))]
    # OD test
    covariates_pasted <- paste(covariates, collapse = " + ")
    formula <- as.formula(paste("count", " ~ ", covariates_pasted))
    print(formula)
    poisson_base <- glm(formula, data = all_var_scaled_binary_treatment, family = poisson)
    od_test <- dispersiontest(poisson_base,trafo=1)
    capture.output(od_test, file= paste0("models/", treatment, "_overdispersion_test.txt"))
    
    # run simple OLS
    simple_ols <- glm(formula, data = all_var_scaled_binary_treatment)
    simple_ols_summary <- summary(simple_ols)
    capture.output(simple_ols_summary, file= paste0("models/", treatment, "_simple_ols.txt"))
    
    # run pooled ZINB
    zero_condition_covariates <- paste(covariates[str_detect(covariates,paste("od_","pop_den","poi",sep="|"))],
                                      collapse = " + ")
    formula <- as.formula(paste("count", " ~ ", covariates_pasted,"|",zero_condition_covariates))
    pooled_zinb <- zeroinfl(formula, dist = "negbin", link = "logit", data = all_var_scaled_binary_treatment)
    pooled_zinb_summary <- summary(pooled_zinb)
    capture.output(pooled_zinb_summary, file= paste0("models/", treatment, "_pooled_zinb.txt"))
    
    # run FE ZINB
    covariates <- c("year",covariates)
    covariates_pasted <- paste(covariates, collapse = " + ")
    formula <- as.formula(paste("count", " ~ ", covariates_pasted,"|",zero_condition_covariates))
    fe_zinb <- zeroinfl(formula, dist = "negbin", link = "logit", data = all_var_scaled_binary_treatment)
    fe_zinb_summary <- summary(fe_zinb)
    capture.output(fe_zinb_summary, file= paste0("models/", treatment, "_fe_zinb.txt"))
    
    # psm
    if (treatment == "binary"){
      # binary_ind_var_name_list <- names(all_var_scaled_binary_treatment)[str_detect(names(all_var_scaled_binary_treatment),"_binary")]
      # hard code the binary independent variables
      binary_ind_var_name_list <- c("ss_vegetation_binary_70_percentile","ss_sidewalk","slope_log_binary_70_percentile")
      model_list <- list()
      vegetation_covariates <- names(all_var_scaled_binary_treatment)[str_detect(names(all_var_scaled_binary_treatment), 
                                                                                paste("^age_","year", "^lu_", "road","sidewalk(?!_binary)", "sky","terrain",
                                                                                      "^od_", "IMD", "poi","slope(?!_binary)","housing_price","pop_den", sep="|"))]
      sidewalk_covariates <- names(all_var_scaled_binary_treatment)[str_detect(names(all_var_scaled_binary_treatment), 
                                                                              paste("year", 
                                                                                    "^age_",
                                                                                    "^lu_",
                                                                                    "building", "road", "pole", 
                                                                                    "traffic.light","traffic.sign","vegetation(?!_binary)", "sky","terrain", 
                                                                                    "^od_", "IMD", "poi","slope(?!_binary)","housing_price","pop_den", sep="|"))]
      slope_covariates <- names(all_var_scaled_binary_treatment)[str_detect(names(all_var_scaled_binary_treatment), 
                                                                            paste("^age_","year", "^lu_","building", "road", "vegetation(?!_binary)", 
                                                                                  "sky","terrain","^od_", "IMD", "poi","housing_price","pop_den", sep="|"))]
      covariates <- names(all_var_scaled_binary_treatment)[!str_detect(names(all_var_scaled_binary_treatment), 
                                                                      paste("count","count_point_id", "binary","count_log","bus","ss_bicycle","motorcycle","ss_person","ss_rider",
                                                                            "ss_train", "ss_truck", "ss_car", "ss_building", sep="|"))]
      pb <- progress_bar$new(total = length(binary_ind_var_name_list))
      for (ind_var_name in binary_ind_var_name_list){
        pb$tick()
        print(ind_var_name)
        if (str_detect(ind_var_name,"vegetation")){
          # covariates_names <- vegetation_covariates
          covariates_names <- covariates[str_detect(covariates,
                                                    "^((?!.*vegetation*).)*$")]
        } else if (str_detect(ind_var_name,"sidewalk")){
          # covariates_names <- sidewalk_covariates
          covariates_names <- covariates[str_detect(covariates,
                                                    "^((?!.*sidewalk*).)*$")]
        } else if (str_detect(ind_var_name,"slope")){
          # covariates_names <- slope_covariates
          covariates_names <- covariates[str_detect(covariates,
                                                    "^((?!.*slope*).)*$")]
        }
        models <- run_psm(all_var_scaled_binary_treatment, "count",ind_var_name, covariates_names, model_dir, figure_dir)
        model_list[[ind_var_name]] <- models
      }
      # # create model objects to pass to stargazer later
      # vegetation_ps <- model_list$ss_vegetation_binary_70_percentile[["second_stage"]]
      # sidewalk_ps <- model_list$ss_sidewalk_binary_70_percentile[["second_stage"]]
      # slope_ps <- model_list$slope_log_binary_70_percentile[["second_stage"]]
      # cluster=c("subclass")
      # vegetation_se <- as.vector(summary(vegetation,cluster = cluster)$coefficients$count[,"Std. Error"])
      # sidewalk_se <- as.vector(summary(sidewalk,cluster = cluster)$coefficients$count[,"Std. Error"])
      # slope_se <- as.vector(summary(slope,cluster = cluster)$coefficients$count[,"Std. Error"])
      # se_list <- list(vegetation_se,sidewalk_se,slope_se)
    }
    
    # ipw
    else{
      # list of independent variables
      ind_var_name_list <- c("ss_vegetation","ss_sidewalk","slope_log")
      # list of covariates
      ipw_covariates <- names(all_var_scaled)[!str_detect(names(all_var_scaled), 
                                                          paste("count","count_log","bus","ss_bicycle","motorcycle","ss_person","ss_rider",
                                                                "ss_train", "ss_truck", "ss_car", 
                                                                # "ss_building","ss_fence","ss_pole","ss_road","ss_terrain","ss_visual_complexity","IMD_score","pop_den_log","age_20_39","lu_residential_community","od","year",
                                                                sep="|"))]
      # ipw_covariates <- c("poi_log","pop_den_log","IMD_score","lu_residential_community","lu_commerce_developed",
      #                     "ss_building","ss_road","ss_sky")
      pb <- progress_bar$new(total = length(ind_var_name_list))
      ipw_model_list <- list()
      for (ind_var_name in ind_var_name_list){
        pb$tick()
        print(ind_var_name)
        if (str_detect(ind_var_name,"vegetation")){
          # covariates_names <- vegetation_covariates
          covariates_names <- ipw_covariates[str_detect(ipw_covariates,
                                                        "^((?!.*vegetation*).)*$")]
        } else if (str_detect(ind_var_name,"sidewalk")){
          # covariates_names <- sidewalk_covariates
          covariates_names <- ipw_covariates[str_detect(ipw_covariates,
                                                        "^((?!.*sidewalk*).)*$")]
        } else if (str_detect(ind_var_name,"slope")){
          # covariates_names <- slope_covariates
          covariates_names <- ipw_covariates[str_detect(ipw_covariates,
                                                        "^((?!.*slope*).)*$")]
        }
        print(covariates_names)
        ipw_model <- run_ipw(all_var_scaled, "count",ind_var_name, covariates_names)
        ipw_model_list[[ind_var_name]] <- ipw_model
      }
      # create stargazer 
      # need to rename models to avoid error: Error in if (is.na(s)) { : the condition has length > 1
      vegetation_ps <- ipw_model_list$ss_vegetation
      sidewalk_ps <- ipw_model_list$ss_sidewalk
      slope_ps <- ipw_model_list$slope
      se_list <- list(NA,NA,NA,NA,NA,NA)
    }
    
    
    # create stargazer
    # need to rename models to avoid error: Error in if (is.na(s)) { : the condition has length > 1
    for (stage in c("first_stage","second_stage")){
      #TODO: loop through treatment variables
      vegetation <- model_list$ss_vegetation_binary_70_percentile[[stage]]
      sidewalk <- model_list$ss_sidewalk_binary_70_percentile[[stage]]
      slope <- model_list$slope_log_binary_70_percentile[[stage]]
      if (stage=="second_stage"){
        cluster=c("subclass")
        vegetation_se <- as.vector(summary(vegetation,cluster = cluster)$coefficients$count[,"Std. Error"])
        # sidewalk_se <- as.vector(summary(sidewalk,cluster = cluster)$coefficients$count[,"Std. Error"])
        slope_se <- as.vector(summary(slope,cluster = cluster)$coefficients$count[,"Std. Error"])
      } else{
        cluster=c()
        vegetation_se <- as.vector(summary(vegetation,cluster = cluster)$coefficients[,"Std. Error"])
        # sidewalk_se <- as.vector(summary(sidewalk,cluster = cluster)$coefficients[,"Std. Error"])
        slope_se <- as.vector(summary(slope,cluster = cluster)$coefficients[,"Std. Error"])
      }
      print(stage)
      stargazer(vegetation,
                # sidewalk,
                slope,
                se=list(vegetation_se,sidewalk_se,slope_se),
                single.row = TRUE,
                column.sep.width = "1pt",
                no.space = TRUE,
                font.size = "small",
                type = "latex", 
                out = paste0("models/psm_", stage,".tex")
      )
    }
    
    
    # # summarize results in stargazer
    # model_result <- stargazer(vegetation_ps,
    #           sidewalk_ps,
    #           slope_ps,
    #           keep = covariates[str_detect(covariates,
    #                                        paste("vegetation","sidewalk","slope",sep="|"))],
    #           se = se_list,
    #           single.row = TRUE,
    #           column.sep.width = "1pt",
    #           no.space = TRUE,
    #           font.size = "small")
    # # save in txt file
    # capture.output(model_result, file= paste0("models/psm_all_treatment.txt"))
  }


  # fe poisson by step ------------------------------------------------------
  # list of independent variables
  ind_var_name_list <- c("ss_vegetation","ss_sidewalk","slope_log")
  # list of baseline control variables
  control_var_vec <- names(all_var_scaled)[str_detect(names(all_var_scaled), "^age_|year|^lu_")]
  for (ind_var_name in ind_var_name_list){
    # run_fe_poisson(all_var_scaled, "count_log",ind_var_name, model_dir)
    # run_zero_inflated(all_var_scaled, "count",ind_var_name, control_var_vec, "poisson")
    run_zero_inflated(all_var_scaled %>% dplyr::select(-c("count_log")), "count",ind_var_name, control_var_vec, "negbin", model_dir)
  }

  # Propensity score matching -----------------------------------------------
  # # create stargazer 
  # # need to rename models to avoid error: Error in if (is.na(s)) { : the condition has length > 1
  # for (stage in c("first_stage","second_stage")){
  #   vegetation <- model_list$ss_vegetation_binary_70_percentile[[stage]]
  #   sidewalk <- model_list$ss_sidewalk_binary_70_percentile[[stage]]
  #   slope <- model_list$slope_log_binary_70_percentile[[stage]]
  #   if (stage=="second_stage"){
  #     cluster=c("subclass")
  #     vegetation_se <- as.vector(summary(vegetation,cluster = cluster)$coefficients$count[,"Std. Error"])
  #     sidewalk_se <- as.vector(summary(sidewalk,cluster = cluster)$coefficients$count[,"Std. Error"])
  #     slope_se <- as.vector(summary(slope,cluster = cluster)$coefficients$count[,"Std. Error"])
  #   } else{
  #     cluster=c()
  #     vegetation_se <- as.vector(summary(vegetation,cluster = cluster)$coefficients[,"Std. Error"])
  #     sidewalk_se <- as.vector(summary(sidewalk,cluster = cluster)$coefficients[,"Std. Error"])
  #     slope_se <- as.vector(summary(slope,cluster = cluster)$coefficients[,"Std. Error"])
  #   }
  #   print(stage)
  #   stargazer(vegetation,
  #             sidewalk,
  #             slope,
  #             se=list(vegetation_se,sidewalk_se,slope_se),
  #             single.row = TRUE,
  #             column.sep.width = "1pt",
  #             no.space = TRUE,
  #             font.size = "small"
  #   )
  # }


  # Inverse probability weighting -------------------------------------------

  # # create stargazer 
  # # need to rename models to avoid error: Error in if (is.na(s)) { : the condition has length > 1
  # vegetation <- model_list$ss_vegetation
  # sidewalk <- model_list$ss_sidewalk
  # slope <- model_list$slope
  # 
  # print(stage)
  # stargazer(vegetation,
  #           sidewalk,
  #           slope,
  #           se=list(vegetation_se,sidewalk_se,slope_se),
  #           single.row = TRUE,
  #           column.sep.width = "1pt",
  #           no.space = TRUE,
  #           font.size = "small"
  # )

  # causal forest -----------------------------------------------------------
  all_var_scaled_binary_treatment <- read.csv(paste0(processed_dir, "/all_var_joined_scaled_binary.csv")) %>% 
    mutate(year=as.numeric(year),
          ss_sidewalk=as.numeric(ss_sidewalk)) %>% 
    dplyr::select(-c(age_60_90,lu_others)) %>% 
    drop_na()
  all_var_scaled <- read.csv(paste0(processed_dir, "/all_var_joined_scaled.csv")) %>% 
    mutate(year=as.numeric(year)) %>% 
    dplyr::select(-c(age_60_90,lu_others)) %>% 
    drop_na()

  covariates <- names(all_var_scaled_binary_treatment)[!str_detect(names(all_var_scaled_binary_treatment), 
                                                                  paste("binary","count","bus","ss_bicycle","motorcycle","ss_person","ss_rider",
                                                                        "ss_train", "ss_truck", "ss_car", "ss_building",
                                                                        sep="|"))]
  binary_ind_var_name_list <- c("ss_vegetation_binary_70_percentile","ss_sidewalk","slope_log_binary_70_percentile")

  pb <- progress_bar$new(total = length(binary_ind_var_name_list))
  for (ind_var_name in binary_ind_var_name_list){
    pb$tick()
    if (str_detect(ind_var_name,"vegetation")){
      # covariates_names <- vegetation_covariates
      covariates_names <- covariates[str_detect(covariates,
                                                "^((?!.*vegetation*).)*$")]
    } else if (str_detect(ind_var_name,"sidewalk")){
      # covariates_names <- sidewalk_covariates
      covariates_names <- covariates[str_detect(covariates,
                                                "^((?!.*sidewalk*).)*$")]
    } else if (str_detect(ind_var_name,"slope")){
      # covariates_names <- slope_covariates
      covariates_names <- covariates[str_detect(covariates,
                                                "^((?!.*slope*).)*$")]
    }
    print(covariates_names)
    run_causal_forest(all_var_scaled_binary_treatment, "count_log",ind_var_name, covariates_names, "binary")
    compute_rate(all_var_scaled_binary_treatment, "count_log",ind_var_name, covariates_names, "binary", model_dir, figure_dir)
    df_list <- compute_cate_ranking(all_var_scaled_binary_treatment, "count_log", ind_var_name, covariates_names)
    df_list$forest.ate %>%
      write.csv(paste0("models/", str_remove(ind_var_name,"_binary_70_percentile"),"/rank_cate.csv"), row.names = F)
    df_list$df %>%
      write.csv(paste0("models/", str_remove(ind_var_name,"_binary_70_percentile"),"/rank_cate_covariates.csv"), row.names = F)
    hte_df <- compute_hte_subgroups(all_var_scaled_binary_treatment, "count_log",ind_var_name, covariates_names[ !covariates_names == 'ss_sidewalk'])
    write.csv(hte_df,paste0("models/", str_remove(ind_var_name,"_binary_70_percentile"),"/hte_by_covariate.csv"), row.names = F)
    compute_overall_var_imp(all_var_scaled_binary_treatment, "binary", model_dir)
  }


  # random forest -----------------------------------------------------------
  covariates <- names(all_var_scaled_binary_treatment)[!str_detect(names(all_var_scaled_binary_treatment), 
                                                                  paste("binary","count","bus","ss_bicycle","motorcycle","ss_person","ss_rider",
                                                                        "ss_train", "ss_truck", "ss_car", "ss_building",
                                                                        sep="|"))]
  ind_var_name_list <- c("ss_vegetation","slope_log")

  pb <- progress_bar$new(total = length(ind_var_name_list))
  for (ind_var_name in ind_var_name_list){
    pb$tick()
    if (str_detect(ind_var_name,"vegetation")){
      # covariates_names <- vegetation_covariates
      covariates_names <- covariates[str_detect(covariates,
                                                "^((?!.*vegetation*).)*$")]
    } else if (str_detect(ind_var_name,"sidewalk")){
      # covariates_names <- sidewalk_covariates
      covariates_names <- covariates[str_detect(covariates,
                                                "^((?!.*sidewalk*).)*$")]
    } else if (str_detect(ind_var_name,"slope")){
      # covariates_names <- slope_covariates
      covariates_names <- covariates[str_detect(covariates,
                                                "^((?!.*slope*).)*$")]
    }
    print(covariates_names)
    run_random_forest(all_var_scaled_binary_treatment, "count_log",ind_var_name, covariates_names, model_dir)
  }
}
  