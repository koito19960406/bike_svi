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


# plot CATE average heatmap -----------------------------------------------
plot_cate_rank <- function(forest.ate){
  # Plotting the point estimate of average treatment effect
  # and 95% confidence intervals around it.
  ggplot(forest.ate) + 
    aes(x = ranking, y = estimate) +
    geom_linerange(aes(ymin=estimate-2*std.err, ymax=estimate+2*std.err, xmin = ranking, xmax= ranking),  position=position_dodge(0.6), size = 1)+
    geom_point(position=position_dodge(0.6), size = 3) +
    # scale_y_continuous(breaks = seq(-0.5, -0.2, 0.1),
    #                    labels = seq(-0.5, -0.2, 0.1), 
    #                    limits= c(-0.5, NA)) + 
    # scale_color_manual(values = cb_colors[1:2]) + 
    ylab("") + xlab("")  +
    labs(title=paste0("Average CATE within each ranking \n for ", str_remove(ind_var_name,"_binary_70_percentile")))+
    theme_ipsum() +
    theme(plot.title = element_text(hjust=0.5),
          plot.title.position="plot")
  ggsave(paste0("reports/figures/", str_remove(ind_var_name,"_binary_70_percentile"),"_rank_cate.png"), width = 5, height = 5)
}
plot_cate_heatmap <- function(df){
  # plot heatmap
  ggplot(df) +
    aes(ranking, covariate) +
    geom_tile(aes(fill = scaling), color = "white", size =1) +
    geom_text(aes(label = labels), size = 12/.pt) +
    scale_fill_gradient(low = "#FFFFFF", high = "#7B52AE", limits =c(0,1), breaks = seq(0, 1, .25)) +
    ggtitle(paste0()) +
    labs(title = paste0("Average covariate values within group \n for ",str_remove(ind_var_name,"_binary_70_percentile")),
         x = "CATE estimate ranking",
         y = "",
         fill= "Scaling") +
    theme_ipsum() +
    theme(axis.text.x = element_text(size=20),
          axis.text.y = element_text(size=20),
          plot.title = element_text(size=45, hjust=0.5),
          plot.title.position="plot")
  ggsave(paste0("reports/figures/", str_remove(ind_var_name,"_binary_70_percentile"),"_rank_cate_covariates.png"), width = 10, height = 15)
}
ind_var_name_list <- c("ss_vegetation","ss_sidewalk","slope")
for (ind_var_name in ind_var_name_list){
  df <- read_csv(paste0("models/", ind_var_name,"/rank_cate_covariates.csv"))
  forest.ate <- read_csv(paste0("models/", str_remove(ind_var_name,"_binary_70_percentile"),"/rank_cate.csv"))
  plot_cate_rank(forest.ate)
  plot_cate_heatmap(df)
}


# plot HTE by covariate ---------------------------------------------------
plot_hte_covariate <- function(ind_var_name){
  # load the results
  data_pred <- read.csv(paste0("models/", str_remove(ind_var_name,"_binary_70_percentile"),"/hte_by_covariate.csv")) %>% 
    # filter(percentile==0.25|percentile==0.75) %>% 
    mutate(ci_low=tau_hat - 2*tau_hat_se,
           ci_high = tau_hat + 2*tau_hat_se,
           percentile= as.factor(percentile))
  print(data_pred)
  # Plot predictions for each group and 95% confidence intervals around them.
  ggplot(data_pred) +
    geom_point(aes(x=percentile, y=tau_hat), color="#7B52AE") +
    geom_errorbar(aes(x=percentile, ymin=ci_low, ymax=ci_high, width=.2), color="#7B52AE") +
    coord_flip() +
    labs(title = paste0("Predicted heterogeneous treatment effects for covariates (other variables fixed at median)"),
         x = "Percentile",
         y = "",
         fill= "") +
    # scale_x_continuous("polviews", breaks=covariate.grid, labels=signif(covariate.grid, 2)) +
    theme_ipsum() +
    theme(axis.text.x = element_text(size=10),
          axis.text.y = element_text(size=10),
          plot.title = element_text(size=45, hjust=0.5),
          plot.title.position="plot",
          strip.text.y.right = element_text(angle = 0)) +
    facet_grid(covariate ~ .)
  ggsave(paste0("reports/figures/", str_remove(ind_var_name,"_binary_70_percentile"),"_hte_by_covariate.png"), width = 10, height = 30)
}

ind_var_name_list <- c("ss_vegetation","ss_sidewalk","slope")
for (ind_var_name in ind_var_name_list){
  plot_hte_covariate(ind_var_name)
}


