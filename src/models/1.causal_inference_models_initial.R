pacman::p_load(tidyverse, stats, plm, utils,pglm)


# load data ---------------------------------------------------------------
root_dir <- "/Volumes/exfat/bike_svi"
if (!(file.exists(root_dir))){
  root_dir <- "/Volumes/Extreme SSD/bike_svi"
}
all_var <- read.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined.csv")) %>% 
  select(-c(panoid, count_point_id, pid))

# simplest ols model
model_ols <- lm(pedal_cycles ~ ., data = all_var)
model_ols_summary <- summary(model_ols)
capture.output(model_ols_summary, file= paste0("bike_svi/models/ols.txt"))

# normal poisson
model_poisson <- glm(pedal_cycles ~ ., family = poisson(), data = all_var)
model_poisson_summary <- summary(model_poisson)
capture.output(model_poisson_summary, file= paste0("bike_svi/models/poisson.txt"))

# year fixed effect ols model
all_var_pdf <- pdata.frame(all_var, index=c("year"))
model_year_fe_ols <- plm(pedal_cycles ~ bicycle+building+bus+car+fence+motorcycle+person+pole+rider+road+sidewalk+sky+terrain+traffic.light+traffic.sign+train+truck+vegetation+wall, model = "within", effect = "time", data = all_var_pdf) #TODO ask the prof about model and effect here
model_year_fe_ols_summary <- summary(model_year_fe_ols)
capture.output(model_year_fe_ols_summary, file= paste0("bike_svi/models/year_fe_ols.txt"))

# year fixed effect poisson model
model_year_fe_poisson <- pglm(pedal_cycles ~ bicycle+building+bus+car+fence+motorcycle+person+pole+rider+road+sidewalk+sky+terrain+traffic.light+traffic.sign+train+truck+vegetation+wall, model = "within", effect = "time", family = poisson(), data = all_var_pdf)
model_year_fe_poisson_summary <- summary(model_year_fe_poisson)
capture.output(model_year_fe_poisson_summary, file= paste0("bike_svi/models/year_fe_poisson.txt"))
