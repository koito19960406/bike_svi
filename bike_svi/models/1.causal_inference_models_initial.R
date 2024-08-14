pacman::p_load(tidyverse, stats, plm, utils,pglm, magrittr, dotenv, car)
source("./bike_svi/models/utils.R")

# load data ---------------------------------------------------------------
load_dot_env()
root_dir <- Sys.getenv("ROOT_DIR")
if (!file.exists(root_dir)){
  root_dir <- here()
}

# load external/city_list.txt to get the list of cities
city_list <- read.csv(paste0(root_dir,"/data/external/city_list.txt"), header = FALSE, sep = "\t") %>% 
  rename(city = V1) %>% 
  mutate(city = str_replace_all(city, " ", "_")) %>% 
  as.vector() %>% 
  unlist()

ss_var_list <- list(
  "London" = c("ss_vegetation", "ss_bike_lane",  "ss_bike_rack",  "ss_curb",  "ss_curb_cut",  "ss_parking",  "ss_pothole",  "ss_street_light"),
  "Montreal" = c("ss_vegetation", "ss_guard_rail",  "ss_pedestrian_area",  "ss_sidewalk",  "ss_street_light",  "ss_bench")
)

# loop through the cities
for (city in city_list){
  if (city == "Montreal"){
    next
  }
  print(paste0("Processing city: ", city))
  
  # create a folder for each city
  model_dir <- paste0(root_dir,"/models/", city)
  if (!dir.exists(model_dir)){
    dir.create(model_dir)
  }
  
  # read in the data
  all_var <- read.csv(paste0(root_dir,"/data/processed/cities/", city, "/all_var_joined.csv"))
  
  # get the variables for this city
  city_vars <- ss_var_list[[city]]
  
  # If city_vars is NULL (i.e., the city is not in ss_var_list), use an empty character vector
  if (is.null(city_vars)) city_vars <- character(0)
  
  # remove variables that are not in city_vars
  all_var_pdf <- all_var %>% 
    dplyr::select(-one_of(unlist(ss_var_list), "lu_others", "age_60_90"),
           -contains("_binary"), one_of(paste0(city_vars, "_binary")),
           -contains("count_log")) %>% 
    remove_highly_correlated(threshold = 0.9)
  
  all_var <- all_var_pdf %>%
    dplyr::select(-c(count_point_id))

  # simplest ols model
  print("Running simplest ols model")
  model_ols <- lm(count ~ ., data = all_var)
  model_ols_summary <- summary(model_ols)
  capture.output(model_ols_summary, file= paste0(model_dir, "/ols.txt"))
  vif_result <- vif(model_ols)
  print(vif_result)
  # normal poisson
  print("Running normal poisson model")
  model_poisson <- glm(count ~ ., family = poisson(), data = all_var)
  model_poisson_summary <- summary(model_poisson)
  capture.output(model_poisson_summary, file= paste0(model_dir, "/poisson.txt"))

  # year fixed effect ols model
  print("Running year fixed effect ols model")
  all_var_pdf <- pdata.frame(all_var_pdf, index=c("year"))
  model_year_fe_ols <- plm(count ~ ., model = "within", effect = "time", data = all_var_pdf)
  model_year_fe_ols_summary <- summary(model_year_fe_ols)
  capture.output(model_year_fe_ols_summary, file= paste0(model_dir, "/year_fe_ols.txt"))
  
  # # year fixed effect poisson model
  # print("Running year fixed effect poisson model")
  # model_year_fe_poisson <- pglm(count ~ ., model = "within", effect = "time", family = poisson(), data = all_var_pdf)
  # model_year_fe_poisson_summary <- summary(model_year_fe_poisson)
  # capture.output(model_year_fe_poisson_summary, file= paste0(model_dir, "/year_fe_poisson.txt"))
}

