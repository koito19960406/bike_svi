pacman::p_load(tidyverse, stats, plm, utils,pglm,progress,MatchIt,lmtest,sandwich,
               pscl, cobalt, grf,AER,DiagrammeRsvg,rsvg,stargazer,hrbrthemes,Hmisc,
               WeightIt,gbm,CBPS,caret,car)

# load data ---------------------------------------------------------------
root_dir <- "/Volumes/ExFAT/bike_svi"
if (!(file.exists(root_dir))){
  root_dir <- "/Volumes/Extreme SSD/bike_svi"
}
# set seed
set.seed(1234)
# load data
all_var_scaled_binary_treatment <- read.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined_scaled_binary.csv")) %>% 
  mutate(year=as.factor(year)) %>% 
  dplyr::select(-c(age_60_90,lu_others)) %>% 
  # select(-c(lu_defence,lu_vacant,lu_undeveloped_land,lu_minerals_landfill,
  #           lu_forest_open_land_water,lu_outdoor_recreation,lu_transport_utilities,
  #           lu_agriculture, lu_community_service,lu_residential_gardens, lu_industry_commerce,lu_unknown_developed_use)) %>% 
  drop_na()
all_var_scaled <- read.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined_scaled.csv")) %>% 
  mutate(year=as.factor(year)) %>% 
  dplyr::select(-c(age_60_90,lu_others)) %>% 
  drop_na()
all_var <- read.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined.csv")) %>% 
  select(-c(count_point_id, X90, period,lu_vacant,lu_undeveloped_land)) %>% 
  mutate(year=as.factor(year)) %>% 
  # rename_at(vars(contains('count')), ~paste0(., "_log")) %>% 
  # mutate_at(vars(contains("_log")), log) %>% 
  # mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), 0, .x))) %>% 
  drop_na()


# define key functions ----------------------------------------------------
# psm
run_psm <- function(data,dep_var_name, ind_var_name, covariates){
  models <- list()
  # check balance
  covariates_pasted <- paste(covariates, collapse = " + ")
  formula <- as.formula(paste(ind_var_name, " ~ ", covariates_pasted))
  match_result <- matchit(formula = formula, data = data, method = "nearest", distance = "glm")
  summary_match_result <- summary(match_result)
  capture.output(summary_match_result, file= paste0("models/",sub("_binary.*", "", ind_var_name),"/", ind_var_name, "_summary_balance.txt"))
  summary_match_model <- summary(match_result$model)
  capture.output(summary_match_model, file= paste0("models/",sub("_binary.*", "", ind_var_name),"/", ind_var_name, "_first_stage_model.txt"))
  models$first_stage <- match_result$model
  # plot
  # pdf(paste0("reports/figures/",ind_var_name,"_match_result.pdf"), height = 2, width = 6)
  # plot(match_result, type = "density", interactive = FALSE)
  plot <- bal.plot(match_result, var.name = "distance", which = "both",lwd=0.5) +
    scale_fill_manual(values = alpha(c("#7B52AE", "#74B652"), 0.5))+
    labs(title = paste0("Distributional Balance for ", strsplit(ind_var_name, "_binary")[[1]][1]))+
    theme_ipsum()
  ggsave(paste0("reports/figures/",ind_var_name,"_match_result.png"),height = 4, width = 10)
  # print(plot)
  # dev.off()
  # model
  match_result_df <- match.data(match_result)
  match_result_df_cor <- match_result_df %>% 
    dplyr::select({unlist(strsplit(covariates_names," \\+ "))}) %>% 
    dplyr::select(-c(year))
  corrmatrix <- cor(match_result_df_cor,use="complete.obs")
  corrdf <- corrmatrix %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Var1") %>%
    gather("Var2", "value", -Var1) 
  
  print(corrdf %>% 
          filter((value>=0.6|value<=-0.6)&(Var1!=Var2)))
  right_side <- paste0(ind_var_name," + ", covariates_pasted)
  formula <- as.formula(paste(dep_var_name, " ~ ", right_side))
  zero_condition_covariates <- paste(covariates[str_detect(covariates,paste("od_","pop_den","poi",sep="|"))],
                                     collapse = " + ")
  formula <- as.formula(paste(dep_var_name, " ~ ", right_side, "|", zero_condition_covariates))
  model_year_zero_inflated <- zeroinfl(formula, dist = "negbin", link = "logit", data = match_result_df, weights=weights) # check if it needs to be inverse
  model_year_zero_inflated_summary <- summary(model_year_zero_inflated,cluster = c("subclass"))
  capture.output(model_year_zero_inflated_summary, file= paste0("models/",sub("_binary.*", "", ind_var_name),"/", ind_var_name, "_psm_year_fe_zinb.txt"))
  models$second_stage <- model_year_zero_inflated
  # return model object for stargazer later
  return(models)
}

#ipw
run_ipw <- function(data,dep_var_name, ind_var_name, covariates){
  # check balance
  covariates_pasted <- paste(covariates, collapse = " + ")
  formula <- as.formula(paste(ind_var_name, " ~ ", covariates_pasted))
  weight_result <- weightit(formula = formula, data = data,  estimand = "ATT", method = "ps")
  weight_result <- trim(weight_result, at = 0.95, lower=TRUE)
  summary_weight_result <- bal.tab(weight_result, stats = c("c", "m"), un = TRUE,
                                   thresholds = c(cor = .1))
  capture.output(summary_weight_result, file= paste0("models/",sub("_binary.*", "", ind_var_name),"/", ind_var_name, "_ipw_summary_balance.txt"))
  # plot
  plot <- love.plot(weight_result, stats = c("c"),
                    thresholds = c(cor = .1), 
                    abs = TRUE, wrap = 20,
                    var.order = "unadjusted", line = TRUE) +
    scale_colour_manual(values = alpha(c("#7B52AE", "#74B652"), 0.5))+
    # labs(title = paste0("Distributional Balance for ", strsplit(ind_var_name, "_binary")[[1]][1]))+
    theme_ipsum()
  ggsave(paste0("reports/figures/",ind_var_name,"_weight_result.png"),height = 4, width = 10)
  # print(plot)
  # dev.off()
  # model
  right_side <- paste0(ind_var_name," + ", covariates_pasted)
  zero_condition_covariates <- paste(covariates[str_detect(covariates,paste("od_","pop_den","poi",sep="|"))],
                                     collapse = " + ")
  formula <- as.formula(paste(dep_var_name, " ~ ", right_side, "|", zero_condition_covariates))
  print(formula)
  model_year_zero_inflated <- zeroinfl(formula, dist = "negbin", link = "logit", data = data, weights=weight_result$weights) # check if it needs to be inverse
  model_year_zero_inflated_summary <- summary(model_year_zero_inflated,cluster = c("subclass"))
  capture.output(model_year_zero_inflated_summary, file= paste0("models/",sub("_binary.*", "", ind_var_name),"/", ind_var_name, "_ipw_year_fe_zinb.txt"))
  # return model object for stargazer later
  return(model_year_zero_inflated)
}

# causal forest
run_causal_forest <- function(data,dep_var_name, ind_var_name, covariates_names, treatment){
  print(ind_var_name)
  X <- data %>% 
    select(covariates_names) %>% 
    as.matrix() %>% 
    unlist()
  Y <- data %>% 
    select(dep_var_name)%>% 
    as.vector() %>% 
    unlist()
  W <- data %>% 
    select(ind_var_name)%>% 
    as.vector() %>% 
    unlist()
  tau.forest <- causal_forest(X, Y, W, seed=1234)
  
  # Estimate treatment effects for the training data using out-of-bag prediction.
  # tau.hat.oob <- predict(tau.forest)
  # pdf(paste0("reports/figures/",ind_var_name,"_causal_forest.pdf"), height = 7, width = 7)
  # plot <- hist(tau.hat.oob$predictions)
  # print(plot)
  # dev.off()
  
  # variable importance
  forest.Y.varimp <- variable_importance(tau.forest)
  df <- data.frame(var_name=covariates_names, variable_importance=forest.Y.varimp)
  # save the result
  df %>%
    write.csv(paste0("models/",ind_var_name,"/", treatment, "_", "causal_forest_var_imp.csv"), row.names = F)
  
  # conditional average treatment effect
  cate <- average_treatment_effect(tau.forest, target.sample = "all")
  t_score <- cate["estimate"] / cate["std.err"]
  cate["p_value"] <- 2*pt(q=abs(t_score), df=length(Y)-1, lower.tail=FALSE)
  capture.output(cate, file= paste0("models/",ind_var_name,"/", treatment, "_", "causal_forest_cate.txt"))
  
  # Extract the first tree from the fitted forest.
  tau.forest.2 <- causal_forest(X, Y, W, seed=1234, min.node.size=100)
  tree <- get_tree(tau.forest.2, 1)
  # Print the first tree.
  print(tree)
  capture.output(tree, file= paste0("models/",ind_var_name,"/", treatment, "_", "causal_tree.txt"))
  # Plot the first tree.
  tree_plot <- plot(tree)
  tree_plot <- DiagrammeRsvg::export_svg(tree_plot)
  tree_plot <- charToRaw(tree_plot) # flatten
  rsvg::rsvg_pdf(tree_plot, paste0("reports/figures/", treatment, "_", ind_var_name,"_causal_tree.pdf")) # saved graph as png
}

# compute rate
compute_rate <- function(data,dep_var_name, ind_var_name, covariates_names, treatment){
  print(ind_var_name)
  X <- data %>% 
    select(covariates_names) %>% 
    as.matrix() %>% 
    unlist()
  Y <- data %>% 
    select(dep_var_name)%>% 
    as.vector() %>% 
    unlist()
  W <- data %>% 
    select(ind_var_name)%>% 
    as.vector() %>% 
    unlist()
  # plot rate
  train <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.5,0.5))
  train.forest <- causal_forest(X[train, ], Y[train], W[train])
  eval.forest <- causal_forest(X[-train, ], Y[-train], W[-train])
  rate <- rank_average_treatment_effect(eval.forest,
                                        predict(train.forest, X[-train, ])$predictions)
  pdf(paste0("reports/figures/",treatment, "_", ind_var_name,"_rate.pdf"), height = 7, width = 7)
  plot(rate)
  dev.off()
  autoc <- paste("AUTOC:", round(rate$estimate, 2), "+/", round(1.96 * rate$std.err, 2))
  capture.output(autoc, file= paste0("models/",str_remove(ind_var_name,"_binary_70_percentile"),"/",treatment, "_", "autoc.txt"))
}

# compute cate ranking
compute_cate_ranking<-function(data,dep_var_name, ind_var_name, covariates_names){
  # initialize list
  df_list <- list()
  # Valid randomized data and observational data with unconfoundedness+overlap.
  # Note: read the comments below carefully.
  # In randomized settings, do not estimate forest.e and e.hat; use known assignment probs.
  
  # Prepare dataset
  fmla <- formula(paste0("~ 0 + ", paste0(covariates_names, collapse="+")))
  X <- model.matrix(fmla, data)
  W <- data[,ind_var_name]
  Y <- data[,dep_var_name]
  
  # Number of rankings that the predictions will be ranking on
  # (e.g., 2 for above/below median estimated CATE, 5 for estimated CATE quintiles, etc.)
  num.rankings <- 4
  
  # Prepare for data.splitting
  # Assign a fold number to each observation.
  # The argument 'clusters' in the next step will mimick K-fold cross-fitting.
  num.folds <- 10
  folds <- sort(seq(nrow(data)) %% num.folds) + 1
  
  # Comment or uncomment depending on your setting.
  # Observational setting with unconfoundedness+overlap (unknown assignment probs):
  forest <- causal_forest(X, Y, W, clusters = folds, seed=1234)
  # Randomized settings with fixed and known probabilities (here: 0.5).
  # forest <- causal_forest(X, Y, W, W.hat=.3, clusters = folds, seed=1234)
  
  # Retrieve out-of-bag predictions.
  # Predictions for observation in fold k will be computed using
  # trees that were not trained using observations for that fold.
  tau.hat <- predict(forest)$predictions
  
  # Rank observations *within each fold* into quintiles according to their CATE predictions.
  ranking <- rep(NA, nrow(data))
  for (fold in seq(num.folds)) {
    tau.hat.quantiles <- quantile(tau.hat[folds == fold], probs = seq(0, 1, by=1/num.rankings))
    ranking[folds == fold] <- cut(tau.hat[folds == fold], tau.hat.quantiles, include.lowest=TRUE,labels=seq(num.rankings))
  }
  
  # Computing AIPW scores.
  tau.hat <- predict(forest)$predictions
  e.hat <- forest$W.hat # P[W=1|X]
  m.hat <- forest$Y.hat # E[Y|X]
  
  # Estimating mu.hat(X, 1) and mu.hat(X, 0) for obs in held-out sample
  # Note: to understand this, read equations 6-8 in this vignette:
  # https://grf-labs.github.io/grf/articles/muhats.html
  mu.hat.0 <- m.hat - e.hat * tau.hat        # E[Y|X,W=0] = E[Y|X] - e(X)*tau(X)
  mu.hat.1 <- m.hat + (1 - e.hat) * tau.hat  # E[Y|X,W=1] = E[Y|X] + (1 - e(X))*tau(X)
  
  # AIPW scores
  aipw.scores <- tau.hat + W / e.hat * (Y -  mu.hat.1) - (1 - W) / (1 - e.hat) * (Y -  mu.hat.0)
  ols <- lm(aipw.scores ~ 0 + factor(ranking))
  forest.ate <- data.frame("aipw", paste0("Q", seq(num.rankings)), coeftest(ols, vcov=vcovHC(ols, "HC2"))[,1:2])
  colnames(forest.ate) <- c("method", "ranking", "estimate", "std.err")
  rownames(forest.ate) <- NULL # just for display
  df_list$forest.ate <- forest.ate
  
  df <- mapply(function(covariate) {
    # Looping over covariate names
    # Compute average covariate value per ranking (with correct standard errors)
    fmla <- formula(paste0(covariate, "~ 0 + ranking"))
    ols <- lm(fmla, data=transform(data, ranking=factor(ranking)))
    ols.res <- coeftest(ols, vcov=vcovHC(ols, "HC2"))
    
    # Retrieve results
    avg <- ols.res[,1]
    stderr <- ols.res[,2]
    
    # Tally up results
    data.frame(covariate, avg, stderr, ranking=paste0("Q", seq(num.rankings)),
               # Used for coloring
               scaling=pnorm((avg - mean(avg))/sd(avg)),
               # We will order based on how much variation is 'explain' by the averages
               # relative to the total variation of the covariate in the data
               variation=sd(avg) / sd(data[,covariate]),
               # String to print in each cell in heatmap below
               labels=paste0(signif(avg, 3), "\n", "(", signif(stderr, 3), ")"))
  }, covariates_names, SIMPLIFY = FALSE)
  df <- do.call(rbind, df)
  
  # a small optional trick to ensure heatmap will be in decreasing order of 'variation'
  df$covariate <- reorder(df$covariate, order(df$variation))
  
  df_list$df <- df
  return(df_list)
}

# compute hte by covariate
compute_hte_subgroups<-function(data,dep_var_name, ind_var_name, covariates_names){
  # run causal forest
  X <- data %>%
    dplyr::select(covariates_names) %>%
    as.matrix() %>%
    unlist()
  Y <- data %>%
    dplyr::select(dep_var_name)%>%
    as.vector() %>%
    unlist()
  W <- data %>%
    dplyr::select(ind_var_name)%>%
    as.vector() %>%
    unlist()
  tau.forest <- causal_forest(X, Y, W, seed=1234)
  fmla <- formula(paste0("~ 0 + ", paste0(covariates_names, collapse="+")))
  # create a dataframe to store the results
  hte_df <- data.frame(matrix(ncol = 4, nrow = 0))
  col_names <- c("covariate", "percentile", "tau_hat","tau_hat_se")
  colnames(hte_df) <- col_names
  for (selected_covariate in covariates_names){
    print(selected_covariate)
    # calculate 25th and 75th percentiles
    percentiles <- c(.1,.25,.5,.75,.9)
    grid.size <- length(percentiles)
    covariate.grid <- quantile(data[,selected_covariate], probs=percentiles)
    # calculate medians for other covariates
    other_covariates <- covariates_names[-which(covariates_names %in% selected_covariate)]
    medians <- apply(data[, other_covariates, F], 2, median)
    # Construct a dataset
    data.grid <- data.frame(sapply(medians, function(x) rep(x, grid.size)), covariate.grid)
    colnames(data.grid) <- c(other_covariates, selected_covariate)
    X.grid <- model.matrix(fmla, data.grid)
    # Point predictions of the CATE and standard errors 
    forest.pred <- predict(tau.forest, newdata = X.grid, estimate.variance=TRUE)
    tau.hat <- forest.pred$predictions
    tau.hat.se <- sqrt(forest.pred$variance.estimates)
    selected_covariate_vec <- rep(selected_covariate,grid.size)
    df_temp <- cbind(selected_covariate_vec,percentiles,tau.hat,tau.hat.se)
    colnames(df_temp) <- col_names
    # concatenate with hte_df
    hte_df <- rbind(hte_df,df_temp)
  }
  return(hte_df)
}

# compute overall variable importance
compute_overall_var_imp <- function(treatment){
  # run a normal causal forest just to check feature importance
  X <- all_var_scaled_binary_treatment %>%
    dplyr::select(-contains("pedal_cycles","binary")) %>% 
    as.matrix() %>% 
    unlist()
  
  Y <-all_var_scaled_binary_treatment %>%
    dplyr::select("pedal_cycles_log") %>% 
    as.vector() %>% 
    unlist()
  
  regression_forest <- regression_forest(X,Y)
  varimp <- variable_importance(regression_forest)
  var_name <- all_var_scaled %>%
    dplyr::select(-contains("pedal_cycles")) %>% 
    names()
  df <- data.frame(var_name=var_name, variable_importance=varimp)
  df %>% 
    write.csv(paste0("models/", treatment, "_", "causal_forest_var_imp.csv"), row.names = F)
}

# background check --------------------------------------------------------
# check overdispersion
for (treatment in c("continuous","binary")){
  # set covariates
  if (treatment == "continuous"){
    covariates <- names(all_var_scaled_binary_treatment)[!str_detect(names(all_var_scaled_binary_treatment), 
                                                                     paste("pedal_cycles", "ss_bus","ss_bicycle","motorcycle","ss_person","ss_rider",
                                                                           "ss_train", "ss_truck", "ss_car", "ss_building",
                                                                           "binary","year",
                                                                           sep="|"))]
  }
  else{
    covariates <- names(all_var_scaled_binary_treatment)[!str_detect(names(all_var_scaled_binary_treatment), 
                                                                     paste("pedal_cycles", "ss_bus","ss_bicycle","motorcycle","ss_person","ss_rider",
                                                                           "ss_train", "ss_truck", "ss_car", "ss_building",
                                                                           "^.*binary((?!70_percentile).)*$","year","slope_log$","vegetation$","sidewalk$",
                                                                           sep="|"))]
  }
  # OD test
  covariates_pasted <- paste(covariates, collapse = " + ")
  formula <- as.formula(paste("pedal_cycles", " ~ ", covariates_pasted))
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
  formula <- as.formula(paste("pedal_cycles", " ~ ", covariates_pasted,"|",zero_condition_covariates))
  pooled_zinb <- zeroinfl(formula, dist = "negbin", link = "logit", data = all_var_scaled_binary_treatment)
  pooled_zinb_summary <- summary(pooled_zinb)
  capture.output(pooled_zinb_summary, file= paste0("models/", treatment, "_pooled_zinb.txt"))
  
  # run FE ZINB
  covariates <- c("year",covariates)
  covariates_pasted <- paste(covariates, collapse = " + ")
  formula <- as.formula(paste("pedal_cycles", " ~ ", covariates_pasted,"|",zero_condition_covariates))
  fe_zinb <- zeroinfl(formula, dist = "negbin", link = "logit", data = all_var_scaled_binary_treatment)
  fe_zinb_summary <- summary(fe_zinb)
  capture.output(fe_zinb_summary, file= paste0("models/", treatment, "_fe_zinb.txt"))
  
  # psm
  if (treatment == "binary"){
    binary_ind_var_name_list <- names(all_var_scaled_binary_treatment)[str_detect(names(all_var_scaled_binary_treatment),"_binary")]
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
                                                                     paste("pedal_cycles","binary","pedal_cycles_log","bus","ss_bicycle","motorcycle","ss_person","ss_rider",
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
      models <- run_psm(all_var_scaled_binary_treatment, "pedal_cycles",ind_var_name, covariates_names)
      model_list[[ind_var_name]] <- models
    }
    # create model objects to pass to stargazer later
    vegetation_ps <- model_list$ss_vegetation_binary_70_percentile[["second_stage"]]
    sidewalk_ps <- model_list$ss_sidewalk_binary_70_percentile[["second_stage"]]
    slope_ps <- model_list$slope_log_binary_70_percentile[["second_stage"]]
    cluster=c("subclass")
    vegetation_se <- as.vector(summary(vegetation,cluster = cluster)$coefficients$count[,"Std. Error"])
    sidewalk_se <- as.vector(summary(sidewalk,cluster = cluster)$coefficients$count[,"Std. Error"])
    slope_se <- as.vector(summary(slope,cluster = cluster)$coefficients$count[,"Std. Error"])
    se_list <- list(NA, NA, NA, vegetation_se,sidewalk_se,slope_se)
  }
  
  # ipw
  else{
    # list of independent variables
    ind_var_name_list <- c("ss_vegetation","ss_sidewalk","slope_log")
    # list of covariates
    ipw_covariates <- names(all_var_scaled)[!str_detect(names(all_var_scaled), 
                                                        paste("pedal_cycles","pedal_cycles_log","bus","ss_bicycle","motorcycle","ss_person","ss_rider",
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
      ipw_model <- run_ipw(all_var_scaled, "pedal_cycles",ind_var_name, covariates_names)
      ipw_model_list[[ind_var_name]] <- ipw_model
    }
    # create stargazer 
    # need to rename models to avoid error: Error in if (is.na(s)) { : the condition has length > 1
    vegetation_ps <- ipw_model_list$ss_vegetation
    sidewalk_ps <- ipw_model_list$ss_sidewalk
    slope_ps <- ipw_model_list$slope
    se_list <- list(NA,NA,NA,NA,NA,NA)
  }
  
  
  
  
  
  # summarize results in stargazer
  stargazer(simple_ols,
            pooled_zinb,
            fe_zinb,
            vegetation_ps,
            sidewalk_ps,
            slope_ps,
            keep = covariates[str_detect(covariates,
                                         paste("vegetation","sidewalk","slope",sep="|"))],
            se = se_list,
            single.row = TRUE,
            column.sep.width = "1pt",
            no.space = TRUE,
            font.size = "small"
  )
}









# fe poisson by step ------------------------------------------------------
# define functions
run_fe_poisson <- function(data, dep_var_name, ind_var_name){
  # create dir
  dir.create(paste0("models/",ind_var_name))
  # run base model
  formula <- as.formula(paste(dep_var_name, " ~ ", ind_var_name))
  print(formula)
  model_year_fe_poisson <- model_year_fe_poisson <- glm(formula, family = poisson(), data = data)
  model_year_fe_poisson_summary <- summary(model_year_fe_poisson)
  capture.output(model_year_fe_poisson_summary, file= paste0("models/",ind_var_name,"/", "year_fe_poisson_base.txt"))
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
    capture.output(model_year_fe_poisson_summary, file= paste0("models/",ind_var_name,"/", "year_fe_poisson", as.character(length(names_agg)),".txt"))
  } 
}
run_zero_inflated <- function(data, dep_var_name, ind_var_name, control_var_vec, count_model){
  # list to store result
  name_list <- list()
  estimate_list <- list()
  p_val_list <- list()
  # create dir
  dir.create(paste0("models/",ind_var_name))
  # run base model
  right_side <- paste(c(ind_var_name,control_var_vec), collapse = " + ")
  formula <- as.formula(paste(dep_var_name, " ~ ", right_side))
  print(formula)
  model_year_zero_inflated <- zeroinfl(formula, dist = count_model, link = "logit", data = data)
  model_year_zero_inflated_summary <- summary(model_year_zero_inflated)
  capture.output(model_year_zero_inflated_summary, file= paste0("models/",ind_var_name,"/", "year_zero_inflated_",count_model,"_base.txt"))
  # add other covariates one by one
  data_covar <- data %>%
    select(-c(ind_var_name,dep_var_name, control_var_vec)) %>%
    select(1:ncol(.))
  cov_names <- names(data_covar)
  names_agg <- c(ind_var_name, control_var_vec)
  pb <- progress_bar$new(total = length(cov_names))
  counter <- 1
  for (cov_name in cov_names){
    print(cov_name)
    pb$tick()
    right_side <- paste(c(names_agg,cov_name), collapse = " + ")
    formula <- as.formula(paste(dep_var_name, " ~ ", right_side))
    print(formula)
    model_year_zero_inflated <- zeroinfl(formula, dist = count_model, link = "logit", data = data)
    model_year_zero_inflated_summary <- summary(model_year_zero_inflated)
    print(model_year_zero_inflated_summary)
    # append the model result
    # add covariate's name, point estimate, and p-value
    point_est <- model_year_zero_inflated_summary[["coefficients"]][["count"]][2,1]
    p_val <- model_year_zero_inflated_summary[["coefficients"]][["count"]][2,4]
    name_list <- append(name_list, cov_name)
    estimate_list <- append(estimate_list, point_est)
    p_val_list <- append(p_val_list, p_val)
    # save to txt file
    capture.output(model_year_zero_inflated_summary, file= paste0("models/",ind_var_name,"/", "year_zero_inflated", as.character(counter),".txt"))
    counter <- counter + 1
  }
  # convert model_result_list to df and save as csv
  model_result_list <- list(variable = name_list,
                            point_estimate = estimate_list,
                            p_value = p_val_list)
  df <- as.data.frame(do.call(cbind, model_result_list))
  print(df)
  df %>% 
    as.matrix() %>% 
    write.csv(paste0("models/",ind_var_name,"/", "year_zero_inflated_",count_model,"_result.csv"), row.names = F)
}
# list of independent variables
ind_var_name_list <- c("ss_vegetation","ss_sidewalk","slope")
# list of baseline control variables
control_var_vec <- names(all_var)[str_detect(names(all_var), "^age_|year|^lu_")]
for (ind_var_name in ind_var_name_list){
  # run_fe_poisson(all_var_scaled, "pedal_cycles_log",ind_var_name)
  # run_zero_inflated(all_var_scaled, "pedal_cycles",ind_var_name, control_var_vec, "poisson")
  run_zero_inflated(all_var_scaled %>% select(-c("pedal_cycles_log")), "pedal_cycles",ind_var_name, control_var_vec, "negbin")
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
all_var_scaled_binary_treatment <- read.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined_scaled_binary.csv")) %>% 
  select(-c(lu_defence,lu_vacant,lu_undeveloped_land,lu_minerals_landfill,
            lu_forest_open_land_water,lu_outdoor_recreation,lu_transport_utilities,
            lu_agriculture, lu_community_service,lu_residential_gardens, lu_industry_commerce,lu_unknown_developed_use)) %>% 
  mutate(year=as.numeric(year)) %>% 
  drop_na()
all_var_scaled <- read.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined_scaled.csv")) %>% 
  mutate(year=as.numeric(year)) %>% 
  select(-c(age_60_90,lu_others)) %>% 
  drop_na()







covariates <- names(all_var_scaled_binary_treatment)[!str_detect(names(all_var_scaled_binary_treatment), 
                                                                 paste("binary","pedal_cycles","bus","ss_bicycle","motorcycle","ss_person","ss_rider",
                                                                       "ss_train", "ss_truck", "ss_car", "ss_building", sep="|"))]
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
  # run_causal_forest(all_var_scaled, "pedal_cycles_log",ind_var_name, covariates_names)
  # compute_rate(all_var_scaled_binary_treatment, "pedal_cycles_log",paste0(ind_var_name,"_binary_70_percentile"), covariates_names)
  # df_list <- compute_cate_ranking(all_var_scaled_binary_treatment, "pedal_cycles_log",paste0(ind_var_name,"_binary_70_percentile"), covariates_names)
  # df_list$forest.ate %>%
  #   write.csv(paste0("models/", str_remove(ind_var_name,"_binary_70_percentile"),"/rank_cate.csv"), row.names = F)
  # df_list$df %>%
  #   write.csv(paste0("models/", str_remove(ind_var_name,"_binary_70_percentile"),"/rank_cate_covariates.csv"), row.names = F)
  hte_df <- compute_hte_subgroups(all_var_scaled, "pedal_cycles_log",ind_var_name, covariates_names)
  write.csv(hte_df,paste0("models/", str_remove(ind_var_name,"_binary_70_percentile"),"/hte_by_covariate.csv"), row.names = F)
}

  
  