pacman::p_load(
  tidyverse, stats, plm, utils, pglm, progress, MatchIt, lmtest, sandwich,
  pscl, cobalt, grf, AER, DiagrammeRsvg, rsvg, stargazer, hrbrthemes, Hmisc,
  WeightIt, gbm, CBPS, caret, car, notifier, corrplot, randomForest, pdp, doMC,
  doParallel, here, DiagrammeR, dotenv, spdep, spatialreg, MASS, glmmfields
)
source(here("bike_svi/models/utils.R"))

# load data ---------------------------------------------------------------
load_dot_env()
root_dir <- Sys.getenv("ROOT_DIR")
if (!file.exists(root_dir)) {
  root_dir <- here()
}

# set seed
set.seed(1234)
# load external/city_list.txt to get the list of cities
city_list <- read.csv(paste0(root_dir, "/data/external/city_list.txt"), header = FALSE, sep = "\t")
city_list <- city_list %>%
  rename(city = V1) %>%
  mutate(city = str_replace_all(city, " ", "_")) %>%
  as.vector() %>%
  unlist()

# loop through the cities
for (city in city_list) {
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
  processed_dir <- paste0(root_dir, "/data/processed/cities/", city)

  # set treatment variable
  # pairwise correlation for count and segmentation result
  if (city == "London") {
    ss_var_list <- c("ss_vegetation_binary", "ss_bike_lane_binary", "ss_parking_binary", "ss_street_light_binary", 
      "ss_vegetation_binary_60percent", "ss_vegetation_binary_80percent")
  } else if (city == "Montreal") {
    ss_var_list <- c("ss_vegetation_binary", "ss_guard_rail_binary", "ss_sidewalk_binary", "ss_street_light_binary",
      "ss_vegetation_binary_60percent", "ss_vegetation_binary_80percent", "ss_sidewalk_binary_60percent", "ss_sidewalk_binary_80percent")
  }
  treatment_var_list <- c(ss_var_list, "slope_binary", "slope_binary_60percent", "slope_binary_80percent")
  treatment_var_list_collapsed <- paste(treatment_var_list, collapse = "|")

  # load data
  all_var <- read.csv(paste0(processed_dir, "/all_var_joined.csv")) %>%
    mutate(
      count = as.integer(count),
      year = as.numeric(year),
      month = as.numeric(month)
    ) %>%
    dplyr::select(-c(
      age_60_90, lu_others, ss_vegetation, ss_bike_lane,
      ss_bike_rack, ss_curb, ss_curb_cut, ss_parking,
      ss_pothole, ss_street_light, ss_guard_rail,
      ss_pedestrian_area, ss_sidewalk, ss_bench
    )) %>%
    drop_na() %>%
    dplyr::select(
      -one_of(unlist(treatment_var_list)),
      -contains("_binary"), one_of(treatment_var_list)
    ) %>%
    remove_highly_correlated(threshold = 0.9) %>%
    mutate(
      year = as.factor(year),
      month = as.factor(month) 
    ) 
    # remove od_person_count if city is Montreal. And remove od_bicycle_count if city is London
  # Apply conditional logic based on city value
  if (city == "Montreal") {
    all_var <- all_var %>% dplyr::select(-contains("od_person_count"))
  } else if (city == "London") {
    all_var <- all_var %>% dplyr::select(-contains("od_bicycle_count"))
  }
  count_station <- read.csv(paste0(external_dir, "/count_station_clean.csv"))
  all_var_spatial <- all_var %>%
    left_join(count_station, by = c("count_point_id" = "count_point_id")) %>%
    drop_na() %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  # all_var <- read.csv(paste0(processed_dir, "/all_var_joined.csv")) %>%
  #   mutate(
  #     count = as.integer(count),
  #     year = as.numeric(year),
  #     month = as.numeric(month)
  #   ) %>%
  #   dplyr::select(-c(
  #     age_60_90, lu_others
  #   )) %>%
  #   drop_na() %>%
  #   dplyr::select(
  #     -one_of(unlist(treatment_var_list)),
  #     -contains("_binary"), one_of(treatment_var_list)
  #   ) %>%
  #   remove_highly_correlated(threshold = 0.9) %>%
  #   mutate(
  #     year = as.factor(year),
  #     month = as.factor(month) 
  #   )

  # background check --------------------------------------------------------
  # check overdispersion
  covariates <- names(all_var)[!str_detect(
    names(all_var),
    paste("\\bcount\\b", "count_log", "count_point_id", "year", "month", "binary", sep = "|")
  )]
  # OD test
  covariates_pasted <- paste(covariates, collapse = " + ")
  formula <- as.formula(paste("count", " ~ ", covariates_pasted))
  print(formula)
  poisson_base <- glm(formula, data = all_var, family = poisson)
  od_test <- dispersiontest(poisson_base, trafo = 1)
  capture.output(od_test, file = paste0(model_dir, "/overdispersion_test.txt"))

  # run simple OLS
  simple_ols <- glm(formula, data = all_var)
  simple_ols_summary <- summary(simple_ols)
  capture.output(simple_ols_summary, file = paste0(model_dir, "/simple_ols.txt"))

  # run VIF test
  vif_test <- vif(simple_ols)
  capture.output(vif_test, file = paste0(model_dir, "/vif_test.txt"))

  # run pooled ZINB
  zero_condition_covariates <- paste(covariates[str_detect(covariates, paste("od_", "pop_den", "poi", sep = "|"))],
    collapse = " + "
  )
  formula <- as.formula(paste("count", " ~ ", covariates_pasted, "|", zero_condition_covariates))
  pooled_zinb <- zeroinfl(formula, dist = "negbin", link = "logit", data = all_var)
  pooled_zinb_summary <- summary(pooled_zinb)
  capture.output(pooled_zinb_summary, file = paste0(model_dir, "/pooled_zinb.txt"))

  # run FE ZINB
  covariates <- c("year", "month", covariates)
  covariates_pasted <- paste(covariates, collapse = " + ")
  formula <- as.formula(paste("count", " ~ ", covariates_pasted, "|", zero_condition_covariates))
  fe_zinb <- zeroinfl(formula, dist = "negbin", link = "logit", data = all_var)
  fe_zinb_summary <- summary(fe_zinb)
  capture.output(fe_zinb_summary, file = paste0(model_dir, "/fe_zinb.txt"))

  # run FE NB
  for (treatment_var in treatment_var_list) {
    # create a folder for each treatment variable if it doesn't exist
    if (!dir.exists(paste0(model_dir, "/", treatment_var))) {
      dir.create(paste0(model_dir, "/", treatment_var))
    }
    treatment_var_continous <- str_split_1(treatment_var, "_binary")[1]
    covariates_temp <- covariates[!str_detect(covariates, treatment_var_continous)]
    covariates_pasted <- paste(c(covariates_temp, treatment_var), collapse = " + ")

    formula <- as.formula(paste("count", " ~ ", covariates_pasted))
    fe_nb <- glm.nb(formula, data = all_var)
    # compute g-computation with compare_average
    gcomp_nb <- avg_comparisons(
      fe_nb,
       variables = treatment_var,
      newdata = all_var)
    # capture
    capture.output(gcomp_nb, file = paste0(model_dir, "/", treatment_var, "/fe_nb_gcomp.txt"))
    fe_nb_summary <- summary(fe_nb)
    capture.output(fe_nb_summary, file = paste0(model_dir, "/", treatment_var, "/fe_nb.txt"))

    # run spatial lag model
    neighbors <- knn2nb(knearneigh(st_coordinates(all_var_spatial), k = 2))
    weights <- nb2listw(neighbors, style = "W")
    morans_i <- moran.test(residuals(fe_nb), weights)
    capture.output(morans_i, file = paste0(model_dir, "/", treatment_var, "/morans_i.txt"))
    # run spatial lag model with glmmfields
    formula_spatial <- as.formula(paste("count_log.1", " ~ ", covariates_pasted))
    spatial_model_summary <- summary(lagsarlm(formula_spatial, data = all_var_spatial, listw = weights))
    capture.output(spatial_model_summary, file = paste0(model_dir, "/", treatment_var, "/spatial_model.txt"))
    spatial_test <- lm.LMtests(fe_nb, test = c("LMlag", "LMerr", "RLMlag", "RLMerr", "SARMA"), listw = weights)
    capture.output(spatial_test, file = paste0(model_dir, "/", treatment_var, "/spatial_test.txt"))
  }

  # fe poisson by step ------------------------------------------------------
  # list of baseline control variables
  control_var_vec <- names(all_var)[str_detect(names(all_var), "^age_|year|month|^lu_")]
  for (treatment_var in treatment_var_list) {
    treatment_var_continous <- str_split_1(treatment_var, "_binary")[1]
    # run_fe_poisson(all_var_scaled, "count_log",ind_var_name, model_dir)
    # run_zero_inflated(all_var_scaled, "count",ind_var_name, control_var_vec, "poisson")
    # run_zero_inflated(all_var, "count",treatment_var, control_var_vec, "negbin", model_dir)
    run_negative_binomial(all_var %>% dplyr::select(-tidyselect::any_of(c("count_log", treatment_var_continous))), "count", treatment_var, control_var_vec, model_dir)
  }

  # psm ---------------------------------------------------------------------
  model_list <- list()
  model_gcomp_list <- list()
  pb <- progress_bar$new(total = length(treatment_var_list))
  for (treatment_var in treatment_var_list) {
    pb$tick()
    print(treatment_var)
    treatment_var_continous <- str_split_1(treatment_var, "_binary")[1]
    covariates_temp <- covariates[!str_detect(covariates, treatment_var_continous)]
    # if treatment variable is slope, remove "ss_street_light" from covariates: due to data issue
    if (str_detect(treatment_var, "slope_binary")) {
      covariates_temp <- covariates_temp[!str_detect(covariates_temp, "ss_street_light")]
    }
    models <- run_psm_nb(all_var, "count", treatment_var, covariates_temp, model_dir, figure_dir)
    models_gcom_psm <- run_psm_nb(all_var, "count", treatment_var, covariates_temp, model_dir, figure_dir, with_gcomp = TRUE)
    model_list[[treatment_var]] <- models
    model_gcomp_list[[treatment_var]] <- models_gcom_psm
  }

  # create stargazer
  for (stage in c("first_stage", "second_stage")) {
    print(stage)

    # loop through treatment variables
    models_se <- lapply(names(model_list), function(treatment_var) {
      model <- model_list[[treatment_var]][[stage]]
      if (stage == "second_stage") {
        cluster <- c("subclass")
        model_se <- as.vector(summary(model, cluster = cluster)$coefficients[, "Std. Error"])
      } else {
        cluster <- c()
        model_se <- as.vector(summary(model, cluster = cluster)$coefficients[, "Std. Error"])
      }
      return(list(model = model, se = model_se))
    })

    # Extract models and standard errors
    models <- lapply(models_se, function(x) x$model)
    se <- lapply(models_se, function(x) x$se)

    # model results for normal psm
    stargazer(models,
      se = se,
      single.row = TRUE,
      column.sep.width = "1pt",
      no.space = TRUE,
      font.size = "small",
      type = "latex",
      out = paste0(model_dir, "/psm_", stage, ".tex")
    )
    # model results for gcomp psm
    stargazer(model_gcomp_list,
      single.row = TRUE,
      column.sep.width = "1pt",
      no.space = TRUE,
      font.size = "small",
      type = "latex",
      out = paste0(model_dir, "/psm_gcomp.tex")
    )
  }

  # random & causal forest -----------------------------------------------------------
  all_var <- read.csv(paste0(processed_dir, "/all_var_joined.csv")) %>%
    dplyr::select(-c("count_log.1")) %>% 
    mutate(
      year = as.factor(year), # Convert to factor if it's not already
      month = as.factor(month)
    ) %>%
    tidyr::pivot_wider(
      names_from = year,
      values_from = year,
      names_prefix = "year_",
      values_fill = list(year = 0),
      values_fn = list(year = length)
    ) %>%
    tidyr::pivot_wider(
      names_from = month,
      values_from = month,
      names_prefix = "month_",
      values_fill = list(month = 0),
      values_fn = list(month = length)
    ) %>%
    # make sure all the columns are numeric
    mutate_if(is.character, as.numeric) %>%
    drop_na()

  pb <- progress_bar$new(total = length(treatment_var_list))
  for (treatment_var in treatment_var_list) {
    pb$tick()
    treatment_var_continous <- str_split_1(treatment_var, "_binary")[1]
    covariates_temp <- covariates[!str_detect(covariates, treatment_var_continous)]
    # random forest
    # remove "year" and "month" from covariates
    covariates_temp <- covariates_temp[!str_detect(covariates_temp, "year|month")]
    # add columns that contain "year" or "month" to the covariates
    covariates_temp <- c(covariates_temp, names(all_var)[str_detect(names(all_var), "year_|month_")])
    run_random_forest(all_var, "count_log", treatment_var, covariates_temp, model_dir)
    # causal forest
    run_causal_forest(all_var, "count_log", treatment_var, covariates_temp, "binary")
    compute_rate(all_var, "count_log", treatment_var, covariates_temp, "binary", model_dir, figure_dir)
    df_list <- compute_cate_ranking(all_var, "count_log", treatment_var, covariates_temp)
    df_list$forest.ate %>%
      write.csv(paste0(model_dir, "/", treatment_var, "/rank_cate.csv"), row.names = F)
    df_list$df %>%
      write.csv(paste0(model_dir, "/", treatment_var, "/rank_cate_covariates.csv"), row.names = F)
    # remove "year" and "month" from covariates
    covariates_temp <- covariates_temp[!str_detect(covariates_temp, "year|month")]
    hte_df <- compute_hte_subgroups(all_var, "count_log", treatment_var, covariates_temp)
    write.csv(hte_df, paste0(model_dir, "/", treatment_var, "/hte_by_covariate.csv"), row.names = F)
    compute_overall_var_imp(all_var, "binary", model_dir)
  }
}
