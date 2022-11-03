pacman::p_load(tidyverse, Hmisc, GGally, corrplot, RColorBrewer, ggplot2, hrbrthemes,stargazer)


# data exploration --------------------------------------------------------
root_dir <- "/Volumes/ExFAT/bike_svi"
if (!(file.exists(root_dir))){
  root_dir <- "/Volumes/Extreme SSD/bike_svi"
}
all_var <- read.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined.csv")) %>% 
  select(-c(count_point_id, all_ages, person, X90, period)) %>% 
  relocate(pedal_cycles) %>% 
  drop_na()

# summary stats
summary_stats <- describe(all_var)
capture.output(summary_stats, file= paste0("bike_svi/models/summary_stats.txt"))
summary_stats_latetx <- all_var %>% 
  mutate(year = as.numeric(year)) %>% 
  drop_na() %>% 
  stargazer()
capture.output(summary_stats_latetx, file= paste0("bike_svi/models/summary_stats_latetx.txt"))

# correlation matrix
corrmatrix <- cor(all_var,use="complete.obs")
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(all_var)
col1 <- colorRampPalette(brewer.pal(9,"BrBG"))
pdf("bike_svi/reports/figures/correlation_mat.pdf", height = 7, width = 7)
corrplot(corrmatrix,method = "square",  tl.col = "black", tl.cex = 0.75, 
         p.mat = p.mat, sig.level = 0.05, insig = "pch", pch.cex = 1, col = col1(100),
         title = "Correlation matrix of all variables",
         mar=c(0,0,1,0))
dev.off()

# data normalizatioon -----------------------------------------------------
max_over_100 <- function(value){
  if (is.numeric(value)){
    if (max(value,na.rm = T)>100){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  }
  else{
    return(FALSE)
  }
}
all_var_scaled <- all_var %>% 
  mutate(year = as.character(year)) %>% 
  # rename_at(vars(contains('count')), ~paste0(., "_log")) %>% 
  rename_if(max_over_100, list(~paste0(., "_log"))) %>% 
  mutate_at(vars(contains("_log")), log) %>% 
  mutate(across(.cols = everything(), ~ ifelse(is.infinite(.x), 0, .x))) %>% 
  mutate(pedal_cycles = all_var$pedal_cycles)

# save
all_var_scaled %>% 
  write.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined_scaled.csv"), row.names = F)

# pairwise correlation for pedal_cycles and segmentation result
pedal_seg <- all_var %>% 
  select(c(pedal_cycles,vegetation,sidewalk, slope))
pedal_seg_scaled <- all_var_scaled %>% 
  select(c(pedal_cycles_log,vegetation,sidewalk, slope))
pair_corr <- function(data,title,file_path){
  pdf(file_path, height = 7, width = 7)
  scatter_plot <- function(data, mapping, ...) {
    ggplot(data = data, mapping=mapping) +
      stat_binhex()
  }
  g <- ggpairs(data, lower=list(continuous=scatter_plot), title=title)
  print(g)
  dev.off()
}
pair_corr(pedal_seg, "Pair-wise correlation matrix", "bike_svi/reports/figures/pair_wise_correlation.pdf")
pair_corr(pedal_seg_scaled, "Pair-wise correlation matrix", "bike_svi/reports/figures/pair_wise_correlation_scaled.pdf")

# convert treatment into binary
convert_to_binary <- function(array, percentile){
  tile <- ntile(array,10)
  # return TRUE to those above threshold
  return(ifelse(tile>percentile,1,0))
}
# function to create multiple binary columns with different thresholds
create_binary <- function(data,colname){
  for (i in seq(1,9)){
    data <- data %>% 
      mutate("{{colname}}_binary_{i*10}_percentile" := convert_to_binary({{colname}},i))
  }
  return(data)
}
# apply function
all_var_scaled_binary_treatment <- all_var_scaled %>% 
  create_binary(., vegetation) %>% 
  create_binary(., sidewalk) %>% 
  create_binary(., slope) %>% 
  drop_na()
all_var_scaled_binary_treatment %>% 
  write.csv(paste0(root_dir,"/data/processed/cities/London/all_var_joined_scaled_binary.csv"),row.names = FALSE)

