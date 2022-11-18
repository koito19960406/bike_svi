pacman::p_load(tidyverse, ggplot2, hrbrthemes, extrafont)

# stepwise model results --------------------------------------------------
# define a function to plot
plot_step <- function(file_path, ind_var_name){
  df <- read.csv(file_path) %>% 
    mutate(variable = as.factor(variable)) %>% 
    gather(., type, value, point_estimate:p_value, factor_key=TRUE)
  print(df)
  print(summary(df))
  plot <- ggplot(data=df, aes(x=variable, y= value, fill = type)) +
    geom_col(position="dodge") +
    scale_y_continuous(name = "Point Estimate",
                       sec.axis = sec_axis(~., name="P Value"),
                       minor_breaks = NULL) +
    scale_fill_manual("",values=c("#7B52AE", "#74B652")) +
    theme_ipsum() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(title=paste0("Step-wise model result for ", ind_var_name))
  ggsave(paste0("bike_svi/reports/figures/", ind_var_name, "_",str_replace(basename(file_path),".csv",".png")), width = 8, height = 4)
}
ind_var_name_list <- c("ss_vegetation","ss_sidewalk","slope")
model_list <- c("negbin")
for (ind_var_name in ind_var_name_list){
  for (model in model_list){
    file_path <- paste0("bike_svi/models/",ind_var_name,"/", "year_zero_inflated_",model,"_result.csv")
    plot_step(file_path, ind_var_name)
  }
}


# causal forest feature importance ----------------------------------------
plot_importance <- function(file_path, ind_var_name, top_n=10){
  df <- read.csv(file_path) %>% 
    slice_max(variable_importance,n=top_n)
  print(df)
  ggplot(data=df, aes(x=reorder(var_name, variable_importance), y=variable_importance)) +
    geom_bar(stat="identity", fill = "#7B52AE", width=0.4)+
    coord_flip() +
    theme_ipsum() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text.y = element_text(size=10),
          plot.title = element_text(size=10,hjust=0.5),
          plot.title.position="plot") +
    labs(title=paste0("Variable importance \n when estimating ", ind_var_name, " (top ", as.character(top_n), ")"),
         x="Variable names",
         y="Variable importance")
  ggsave(paste0("bike_svi/reports/figures/", ind_var_name, "_",str_replace(basename(file_path),".csv",".png")), width = 4, height = 5)
}
ind_var_name_list <- c("ss_vegetation","ss_sidewalk","slope")
for (ind_var_name in ind_var_name_list){
  file_path <- paste0("bike_svi/models/",ind_var_name,"/", "causal_forest_var_imp.csv")
  plot_importance(file_path, ind_var_name)
}

# plot the overall variable importance
df <- read.csv(paste0("bike_svi/models/causal_forest_var_imp.csv"))%>% 
  slice_max(variable_importance,n=10)
print(df)
plot <-ggplot(data=df, aes(x=reorder(var_name, variable_importance), y=variable_importance)) +
  geom_bar(stat="identity", fill = "#7B52AE", width=0.4) +
  coord_flip() +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.text.y = element_text(size=10),
        plot.title = element_text(size=10, hjust=0.5),
        plot.title.position="plot") +
  labs(title=paste0("Overall variable importance", " (top ", as.character(10), ")"),
       x="Variable names",
       y="Variable importance")
ggsave(paste0("bike_svi/reports/figures/overall_variable_importance.png"), width = 4, height = 5)

