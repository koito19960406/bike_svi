

``` r
# Bike SVI Analysis R Script

# Load required libraries
pacman::p_load(
  tidyverse, stats, plm, utils, pglm, magrittr, dotenv, car,
  Hmisc, GGally, corrplot, RColorBrewer, ggplot2,
  hrbrthemes, stargazer, plotly, sf, basemaps, cowplot,
  ggnewscale, here, ggspatial, lwgeom, ggimage, cropcircles, ggrepel, osmdata,
  ggridges, MASS, progress, MatchIt, lmtest, sandwich,
  pscl, cobalt, grf, AER, DiagrammeRsvg, rsvg, WeightIt, gbm, CBPS, caret,
  notifier, randomForest, pdp, doMC, doParallel, marginaleffects, extrafont
)
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependencies 'miscTools', 'rbibutils', 'bdsmatrix', 'collapse', 'zoo', 'sandwich', 'lmtest', 'maxLik', 'Rdpack'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'lmtest' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'plm' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'plm'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependencies 'lmtest', 'plm', 'statmod'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'lmtest' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'statmod' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'plm' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'pglm' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'pglm'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependencies 'Deriv', 'microbenchmark', 'doBy', 'SparseM', 'MatrixModels', 'carData', 'pbkrtest', 'quantreg'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'microbenchmark' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'SparseM' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'doBy' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'quantreg' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'pbkrtest' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'car' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'car'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependency 'gdtools'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'gdtools' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'hrbrthemes' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'hrbrthemes'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependency 'magick'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'magick' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'basemaps' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'basemaps'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
## also installing the dependency 'magick'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'magick' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'ggimage' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'ggimage'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
## also installing the dependency 'magick'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'magick' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'cropcircles' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'cropcircles'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependencies 'chk', 'RcppProgress'
```

```
## 
## MatchIt installed
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## Warning in utils::install.packages(package, ...): installation of package 'lmtest' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'lmtest'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## 
## pscl installed
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## 
## cobalt installed
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependencies 'DiceKriging', 'lmtest'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'lmtest' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'grf' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'grf'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependencies 'microbenchmark', 'doBy', 'SparseM', 'pbkrtest', 'quantreg', 'car', 'lmtest'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'microbenchmark' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'SparseM' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'lmtest' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'doBy' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'quantreg' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'pbkrtest' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'car' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'AER' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'AER'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## 
## DiagrammeRsvg installed
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## Warning in utils::install.packages(package, ...): installation of package 'rsvg' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'rsvg'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## 
## WeightIt installed
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## 
## gbm installed
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## 
## CBPS installed
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependencies 'SQUAREM', 'diagram', 'lava', 'prodlim', 'clock', 'gower', 'hardhat', 'ipred', 'timeDate', 'ModelMetrics', 'pROC', 'recipes', 'reshape2'
```

```
## 
## caret installed
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## Warning: package 'notifier' is not available for this version of R
## 
## A version of this package for your version of R might be available elsewhere,
## see the ideas at
## https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'notifier'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## Warning in utils::install.packages(package, ...): installation of package 'randomForest' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'randomForest'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## 
## pdp installed
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## 
## doMC installed
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## 
## doParallel installed
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependency 'insight'
```

```
## 
## marginaleffects installed
```

```
## Warning in pacman::p_load(tidyverse, stats, plm, utils, pglm, magrittr, : Failed to install/load:
## plm, pglm, car, hrbrthemes, basemaps, ggimage, cropcircles, lmtest, grf, AER, rsvg, notifier, randomForest
```

``` r
# Source utility functions
source(here("bike_svi/models/utils.R"))
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependency 'lmtest'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'lmtest' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'plm' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'plm'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependencies 'lmtest', 'plm', 'statmod'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'lmtest' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'statmod' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'plm' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'pglm' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'pglm'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## Warning in utils::install.packages(package, ...): installation of package 'lmtest' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'lmtest'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependency 'lmtest'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'lmtest' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'grf' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'grf'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependencies 'microbenchmark', 'doBy', 'SparseM', 'pbkrtest', 'quantreg', 'car', 'lmtest'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'microbenchmark' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'SparseM' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'lmtest' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'doBy' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'quantreg' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'pbkrtest' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'car' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'AER' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'AER'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## Warning in utils::install.packages(package, ...): installation of package 'rsvg' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'rsvg'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependency 'gdtools'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'gdtools' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'hrbrthemes' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'hrbrthemes'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependencies 'microbenchmark', 'doBy', 'SparseM', 'pbkrtest', 'quantreg'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'microbenchmark' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'SparseM' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'doBy' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'quantreg' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'pbkrtest' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'car' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'car'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## Warning: package 'notifier' is not available for this version of R
## 
## A version of this package for your version of R might be available elsewhere,
## see the ideas at
## https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#Installing-packages
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'notifier'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## Warning in utils::install.packages(package, ...): installation of package 'randomForest' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'randomForest'
```

```
## Warning in pacman::p_load(tidyverse, stats, plm, utils, pglm, progress, : Failed to install/load:
## plm, pglm, lmtest, grf, AER, rsvg, hrbrthemes, car, notifier, randomForest
```

``` r
source(here("bike_svi/visualization/utils.R"))
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependency 'gdtools'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'gdtools' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'hrbrthemes' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'hrbrthemes'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
```

```
## also installing the dependency 'magick'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'magick' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'basemaps' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'basemaps'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
## also installing the dependency 'magick'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'magick' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'ggimage' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'ggimage'
```

```
## Installing package into '/home/koichi/R/x86_64-pc-linux-gnu-library/4.4'
## (as 'lib' is unspecified)
## also installing the dependency 'magick'
```

```
## Warning in utils::install.packages(package, ...): installation of package 'magick' had non-zero exit status
```

```
## Warning in utils::install.packages(package, ...): installation of package 'cropcircles' had non-zero exit status
```

```
## Warning in p_install(package, character.only = TRUE, ...):
```

```
## Warning in library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE, : there is no package called 'cropcircles'
```

```
## Warning in pacman::p_load(tidyverse, Hmisc, GGally, corrplot, RColorBrewer, : Failed to install/load:
## hrbrthemes, basemaps, ggimage, cropcircles
```

``` r
# Load environment variables
load_dot_env()
```

```
## Error in load_dot_env(): dot-env file does not exist
```

``` r
root_dir <- Sys.getenv("ROOT_DIR")
if (!file.exists(root_dir)) {
  root_dir <- here()
}

# Set seed for reproducibility
set.seed(1234)

# Load city list
city_list <- read.csv(paste0(root_dir, "/data/external/city_list.txt"), header = FALSE, sep = "\t") %>%
  rename(city = V1) %>%
  mutate(city = str_replace_all(city, " ", "_")) %>%
  as.vector() %>%
  unlist()

# Main analysis loop
for (city in city_list) {
  print(paste("Processing city:", city))
  
  # Set up directories
  model_dir <- paste0(root_dir, "/models/", city)
  figure_dir <- paste0("reports/figures/", city)
  external_dir <- paste0(root_dir, "/data/external/cities/", city)
  processed_dir <- paste0(root_dir, "/data/processed/cities/", city)
  
  dir.create(model_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(figure_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Load data
  all_var <- read.csv(paste0(processed_dir, "/all_var_joined.csv")) %>%
    mutate(
      count = as.integer(count),
      year = as.factor(year)
    ) %>%
    dplyr::select(-c(age_60_90, lu_others, ss_vegetation, ss_bike_lane,
                     ss_bike_rack, ss_curb, ss_curb_cut, ss_parking,
                     ss_pothole, ss_street_light, ss_guard_rail,
                     ss_pedestrian_area, ss_sidewalk, ss_bench)) %>%
    drop_na()
  
  # Set treatment variables
  if (city == "London") {
    ss_var_list <- c("ss_vegetation_binary", "ss_bike_lane_binary", "ss_parking_binary", "ss_street_light_binary")
  } else if (city == "Montreal") {
    ss_var_list <- c("ss_vegetation_binary", "ss_guard_rail_binary", "ss_sidewalk_binary", "ss_street_light_binary")
  }
  treatment_var_list <- c(ss_var_list, "slope_binary")
  
  # Causal Inference Models
  for (treatment_var in treatment_var_list) {
    print(paste("Running causal inference models for", treatment_var))
    
    # Run PSM
    psm_models <- run_psm(all_var, "count", treatment_var, covariates, model_dir, figure_dir)
    
    # Run Negative Binomial
    run_negative_binomial(all_var, "count", treatment_var, covariates, model_dir)
    
    # Additional models can be added here
  }
  
  # Data Exploration and Feature Engineering
  print("Performing data exploration and feature engineering")
  
  # Example: Correlation plot
  cor_matrix <- cor(all_var %>% select_if(is.numeric))
  corrplot(cor_matrix, method = "color", type = "upper", order = "hclust", 
           tl.col = "black", tl.srt = 45, addCoef.col = "black")
  
  # Example: Distribution plot
  ggplot(all_var, aes(x = count)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    theme_minimal() +
    labs(title = paste("Distribution of Count in", city),
         x = "Count", y = "Frequency")
  
  # Visualization of Model Results
  print("Visualizing model results")
  
  # PSM Balance Table
  create_psm_balance_table(model_dir, treatment_var_list, city)
  
  # Causal Forest Results Table
  create_cf_results_table(model_dir, treatment_var_list, city)
  
  # AUTOC Table
  create_autoc_table(model_dir, treatment_var_list, city)
  
  # Variable Importance Plot
  plot_variable_importance(model_dir, figure_dir)
  
  # Additional visualizations
  for (treatment_var in treatment_var_list) {
    plot_step(paste0(model_dir, "/", treatment_var, "/year_negative_binomial_result.csv"), treatment_var, figure_dir)
    map_propensity_score(city, external_dir, treatment_var, model_dir, figure_dir)
    plot_importance(paste0(model_dir, "/", treatment_var, "/binary_causal_forest_var_imp.csv"), treatment_var, figure_dir)
    plot_cate_rank(treatment_var, read_csv(paste0(model_dir, "/", treatment_var, "/rank_cate.csv")), figure_dir)
    plot_cate_heatmap(read_csv(paste0(model_dir, "/", treatment_var, "/rank_cate_covariates.csv")), treatment_var, figure_dir)
    plot_hte_covariate(treatment_var, model_dir, figure_dir)
    plot_hte_ranking(treatment_var, model_dir, figure_dir)
  }
}
```

```
## [1] "Processing city: London"
## [1] "Running causal inference models for ss_vegetation_binary"
```

```
## Error: object 'covariates' not found
```

``` r
print("Analysis complete for all cities.")
```

```
## [1] "Analysis complete for all cities."
```

