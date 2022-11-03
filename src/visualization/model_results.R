pacman::p_load(tidyverse, ggplot2, hrbrthemes, extrafont)
extrafont::font_import()

# stepwise model results --------------------------------------------------
# define a function to plot
plot_step <- function(file_path, ind_var_name){
  df <- read.csv(file_path) %>% 
    mutate(variable = as.factor(variable)) %>% 
    gather(., type, value, point_estimate:p_value, factor_key=TRUE)
  print(df)
  plot <- ggplot(data=df, aes(x=X, y= value, color = type)) +
    geom_line() +
    scale_y_continuous(name = "Point Estimate",
                       sec.axis = sec_axis(~., name="P Value"),
                       minor_breaks = NULL) +
    scale_x_continuous(breaks=seq(1,length(unique(df$variable))), labels=unique(df$variable), minor_breaks = NULL) +
    scale_color_manual("",values=c("#7B52AE", "#74B652")) +
    theme_ipsum() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title=paste0("Step-wise model result for ", ind_var_name))
  ggsave(paste0("bike_svi/reports/figures/", ind_var_name, "_",str_replace(basename(file_path),".csv",".png")), width = 8, height = 4)
}
ind_var_name_list <- c("vegetation","sidewalk","slope")
model_list <- c("poisson","negbin")
for (ind_var_name in ind_var_name_list){
  for (model in model_list){
    file_path <- paste0("bike_svi/models/",ind_var_name,"/", "year_zero_inflated_",model,"_result.csv")
    plot_step(file_path, ind_var_name)
  }
}


# causal forest veature importance ----------------------------------------
plot_importance <- function(file_path, ind_var_name){
  df <- df <- read.csv(file_path)
  print(df)
  ggplot(data=df, aes(x=reorder(var_name, variable_importance), y=variable_importance)) +
    geom_bar(stat="identity", fill = "#7B52AE")+
    coord_flip() +
    theme_ipsum() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size=8),
          plot.title = element_text(size=10)) +
    labs(title=paste0("Variable importance when estimating ", ind_var_name),
         x="Variable names",
         y="Variable importance")
  ggsave(paste0("bike_svi/reports/figures/", ind_var_name, "_",str_replace(basename(file_path),".csv",".png")), width = 4, height = 5)
}
ind_var_name_list <- c("vegetation","sidewalk","slope")
for (ind_var_name in ind_var_name_list){
  file_path <- paste0("bike_svi/models/",ind_var_name,"/", "causal_forest_var_imp.csv")
  plot_importance(file_path, ind_var_name)
}

# plot the overall variable importance
df <- df <- read.csv(paste0("bike_svi/models/causal_forest_var_imp.csv"))
print(df)
plot <-ggplot(data=df, aes(x=reorder(var_name, variable_importance), y=variable_importance)) +
  geom_bar(stat="identity", fill = "#7B52AE") +
  coord_flip() +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size=8),
        plot.title = element_text(size=10)) +
  labs(title="Overall variable importance",
       x="Variable names",
       y="Variable importance")
ggsave(paste0("bike_svi/reports/figures/overall_variable_importance.png"), width = 4, height = 5)

