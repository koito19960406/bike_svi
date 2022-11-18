pacman::p_load(tidyverse, stats, plm, utils,pglm,progress,MatchIt,lmtest,sandwich,
               pscl, cobalt, grf,AER,DiagrammeRsvg,rsvg,stargazer,hrbrthemes)


# load data ---------------------------------------------------------------
root_dir <- "/Volumes/ExFAT/bike_svi"
if (!(file.exists(root_dir))){
  root_dir <- "/Volumes/Extreme SSD/bike_svi"
}
# load data
all_var_scaled_binary_treatment <- read.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined_scaled_binary.csv")) %>% 
  mutate(year=as.factor(year)) %>% 
  select(-c(lu_community_service,lu_vacant,lu_undeveloped_land)) %>% 
  drop_na()
all_var_scaled <- read.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined_scaled.csv")) %>% 
  mutate(year=as.numeric(year)) %>% 
  drop_na()
all_var <- read.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined.csv")) %>% 
  select(-c(count_point_id, X90, period,lu_vacant,lu_undeveloped_land)) %>% 
  mutate(year=as.factor(year)) %>% 
  # rename_at(vars(contains('count')), ~paste0(., "_log")) %>% 
  # mutate_at(vars(contains("_log")), log) %>% 
  # mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), 0, .x))) %>% 
  drop_na()


# background check --------------------------------------------------------
# check overdispersion
poisson_base <- glm(pedal_cycles ~ ., data = all_var, family = poisson)
od_test <- dispersiontest(poisson_base,trafo=1)
capture.output(od_test, file= paste0("bike_svi/models/overdispersion_test.txt"))

# run simple OLS
all_var_wo_year <- all_var_scaled %>% 
  select(-c("pedal_cycles_log")) %>% 
  dplyr::select(contains(c("pedal_cycles", "vegetation", "IMD_score", "housing_price", "poi", "slope", "pop_den","lu_")))
simple_ols <- glm(pedal_cycles ~ ., data = all_var_wo_year)
simple_ols_summary <- summary(simple_ols)
capture.output(simple_ols_summary, file= "bike_svi/models/simple_ols.txt")
stargazer(simple_ols,
          single.row = TRUE,
          column.sep.width = "1pt",
          no.space = TRUE,
          font.size = "small"
)

# fe poisson by step ------------------------------------------------------
# define functions
run_fe_poisson <- function(data, dep_var_name, ind_var_name){
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
  # list to store result
  name_list <- list()
  estimate_list <- list()
  p_val_list <- list()
  # create dir
  dir.create(paste0("bike_svi/models/",ind_var_name))
  # run base model
  right_side <- paste(c(ind_var_name,control_var_vec), collapse = " + ")
  formula <- as.formula(paste(dep_var_name, " ~ ", right_side))
  print(formula)
  model_year_zero_inflated <- zeroinfl(formula, dist = count_model, link = "logit", data = data)
  model_year_zero_inflated_summary <- summary(model_year_zero_inflated)
  capture.output(model_year_zero_inflated_summary, file= paste0("bike_svi/models/",ind_var_name,"/", "year_zero_inflated_",count_model,"_base.txt"))
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
    capture.output(model_year_zero_inflated_summary, file= paste0("bike_svi/models/",ind_var_name,"/", "year_zero_inflated", as.character(counter),".txt"))
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
    write.csv(paste0("bike_svi/models/",ind_var_name,"/", "year_zero_inflated_",count_model,"_result.csv"), row.names = F)
}
# list of independent variables
ind_var_name_list <- c("ss_vegetation","ss_sidewalk","slope")
# list of baseline control variables
control_var_vec <- names(all_var)[str_detect(names(all_var), "^X[0-9]*|year|^lu_")]
for (ind_var_name in ind_var_name_list){
  # run_fe_poisson(all_var_scaled, "pedal_cycles_log",ind_var_name)
  # run_zero_inflated(all_var_scaled, "pedal_cycles",ind_var_name, control_var_vec, "poisson")
  run_zero_inflated(all_var_scaled %>% select(-c("pedal_cycles_log")), "pedal_cycles",ind_var_name, control_var_vec, "negbin")
}

# Propensity score matching -----------------------------------------------
run_psm <- function(data,dep_var_name, ind_var_name, covariates_names){
  models <- list()
  # check balance
  covariates_names <- paste(covariates_names, collapse = " + ")
  formula <- as.formula(paste(ind_var_name, " ~ ", covariates_names))
  match_result <- matchit(formula = formula, data = all_var_scaled_binary_treatment, method = "nearest", distance = "glm")
  summary_match_result <- summary(match_result)
  capture.output(summary_match_result, file= paste0("bike_svi/models/",sub("_binary.*", "", ind_var_name),"/", ind_var_name, "_summary_balance.txt"))
  summary_match_model <- summary(match_result$model)
  capture.output(summary_match_model, file= paste0("bike_svi/models/",sub("_binary.*", "", ind_var_name),"/", ind_var_name, "_first_stage_model.txt"))
  models$first_stage <- match_result$model
  # plot
  # pdf(paste0("bike_svi/reports/figures/",ind_var_name,"_match_result.pdf"), height = 2, width = 6)
  # plot(match_result, type = "density", interactive = FALSE)
  plot <- bal.plot(match_result, var.name = "distance", which = "both",lwd=0.5) +
    scale_fill_manual(values = alpha(c("#7B52AE", "#74B652"), 0.5))+
    labs(title = paste0("Distributional Balance for ", strsplit(ind_var_name, "_binary")[[1]][1]))+
    theme_ipsum()
  ggsave(paste0("bike_svi/reports/figures/",ind_var_name,"_match_result.png"),height = 4, width = 10)
  # print(plot)
  # dev.off()
  # model
  match_result_df <- match.data(match_result)
  right_side <- paste0(ind_var_name," + ", covariates_names)
  formula <- as.formula(paste(dep_var_name, " ~ ", right_side))
  model_year_zero_inflated <- zeroinfl(formula, dist = "negbin", link = "logit", data = match_result_df, weights=weights) # check if it needs to be inverse
  model_year_zero_inflated_summary <- summary(model_year_zero_inflated)
  capture.output(model_year_zero_inflated_summary, file= paste0("bike_svi/models/",sub("_binary.*", "", ind_var_name),"/", ind_var_name, "_psm_year_fe_zinb.txt"))
  models$second_stage <- model_year_zero_inflated
  # return model object for stargazer later
  return(models)
}

binary_ind_var_name_list <- names(all_var_scaled_binary_treatment)[str_detect(names(all_var_scaled_binary_treatment),"_binary")]
pb <- progress_bar$new(total = length(binary_ind_var_name_list))
model_list <- list()
vegetation_covariates <- names(all_var_scaled_binary_treatment)[str_detect(names(all_var_scaled_binary_treatment), 
  paste("^X[0-9]*","year", "^lu_", "road","sidewalk(?!_binary)", "sky","terrain",
        "^od_", "IMD", "poi","slope(?!_binary)","housing_price","pop_den", sep="|"))]
sidewalk_covariates <- names(all_var_scaled_binary_treatment)[str_detect(names(all_var_scaled_binary_treatment), 
  paste("^X[0-9]*","year", #"^lu_", 
        "building", "road", "pole", 
        "traffic.light","traffic.sign","vegetation(?!_binary)", "sky","terrain", 
        "^od_", "IMD", "poi","slope(?!_binary)","housing_price","pop_den", sep="|"))]
slope_covariates <- names(all_var_scaled_binary_treatment)[str_detect(names(all_var_scaled_binary_treatment), 
  paste("^X[0-9]*","year", "^lu_", "building", "road", "vegetation(?!_binary)", 
        "sky","terrain","^od_", "IMD", "poi","housing_price","pop_den", sep="|"))]
for (ind_var_name in binary_ind_var_name_list){
  pb$tick()
  print(ind_var_name)
  if (str_detect(ind_var_name,"vegetation")){
    covariates_names <- vegetation_covariates
  } else if (str_detect(ind_var_name,"sidewalk")){
    covariates_names <- sidewalk_covariates
  } else if (str_detect(ind_var_name,"slope")){
    covariates_names <- slope_covariates
    }
  models <- run_psm(all_var_scaled_binary_treatment, "pedal_cycles",ind_var_name, covariates_names)
  model_list[[ind_var_name]] <- models
}
# create stargazer 
# need to rename models to avoid error: Error in if (is.na(s)) { : the condition has length > 1
for (stage in c("first_stage","second_stage")){
  vegetation <- model_list$ss_vegetation_binary_70_percentile[[stage]]
  sidewalk <- model_list$ss_sidewalk_binary_70_percentile[[stage]]
  slope <- model_list$slope_binary_70_percentile[[stage]]
  print(stage)
  stargazer(vegetation, 
            sidewalk,
            slope,
            single.row = TRUE,
            column.sep.width = "1pt",
            no.space = TRUE,
            font.size = "small"
  )
}

# causal forest -----------------------------------------------------------
run_causal_forest <- function(data,dep_var_name, ind_var_name, covariates_names){
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
  # pdf(paste0("bike_svi/reports/figures/",ind_var_name,"_causal_forest.pdf"), height = 7, width = 7)
  # plot <- hist(tau.hat.oob$predictions)
  # print(plot)
  # dev.off()
  
  # variable importance
  forest.Y.varimp <- variable_importance(tau.forest)
  df <- data.frame(var_name=covariates_names, variable_importance=forest.Y.varimp)
  # save the result
  df %>% 
    write.csv(paste0("bike_svi/models/",ind_var_name,"/", "causal_forest_var_imp.csv"), row.names = F)
  
  # conditional average treatment effect 
  cate <- average_treatment_effect(tau.forest, target.sample = "all")
  t_score <- cate["estimate"] / cate["std.err"]
  cate["p_value"] <- 2*pt(q=abs(t_score), df=length(Y)-1, lower.tail=FALSE)
  capture.output(cate, file= paste0("bike_svi/models/",ind_var_name,"/", "causal_forest_cate.txt"))
  
  # Extract the first tree from the fitted forest.
  tau.forest.2 <- causal_forest(X, Y, W, seed=1234, min.node.size=100)
  tree <- get_tree(tau.forest.2, 1)
  # Print the first tree.
  print(tree)
  capture.output(tree, file= paste0("bike_svi/models/",ind_var_name,"/", "causal_tree.txt"))
  # Plot the first tree.
  tree_plot <- plot(tree)
  tree_plot <- DiagrammeRsvg::export_svg(tree_plot)
  tree_plot <- charToRaw(tree_plot) # flatten
  rsvg::rsvg_pdf(tree_plot, paste0("bike_svi/reports/figures/",ind_var_name,"_causal_tree.pdf")) # saved graph as png
}
pb <- progress_bar$new(total = length(ind_var_name_list))

for (ind_var_name in ind_var_name_list){
  pb$tick()
  # if (str_detect(ind_var_name,"vegetation")){
  #   covariates_names <- vegetation_covariates
  # } else if (str_detect(ind_var_name,"sidewalk")){
  #   covariates_names <- sidewalk_covariates
  # } else if (str_detect(ind_var_name,"slope")){
  #   covariates_names <- slope_covariates
  # }
  covariates_names <- all_var_scaled %>% 
    dplyr::select(-contains(c("pedal_cycles",{{ind_var_name}}))) %>% 
    names()
  run_causal_forest(all_var_scaled, "pedal_cycles_log",ind_var_name, covariates_names)
  }
# run a normal causal forest just to check feature importance
X <- all_var_scaled %>%
  dplyr::select(-contains("pedal_cycles")) %>% 
  as.matrix() %>% 
  unlist()

Y <-all_var_scaled %>%
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
  write.csv(paste0("bike_svi/models/causal_forest_var_imp.csv"), row.names = F)
  
  