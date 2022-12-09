library(ggplot2)
library(dplyr)
library(highfrequency)
library(data.table)
source("config.R")
R.utils::sourceDirectory("functions", modifiedOnly = FALSE)

seeds = 1:1000

theta = 0.8
gamma = sqrt(0.01)
lambda = lambda1

future::plan(future::multisession(), workers = future::availableCores() - 2)

MRC_results = MRC_sim_study(seeds, lambda)

HY_results = HY_sim_study(seeds, lambda)





# -------------------------------------------------------------------------



# -------------------------------------------------------------------------
file_names_MRC = list.files(path = "~/Google Drive/Mit drev/AAU/Projekter/P9-projekt/P9_project/results", pattern = "MRC")
file_names_HY = list.files(path = "~/Google Drive/Mit drev/AAU/Projekter/P9-projekt/P9_project/results", pattern = "HY")


theta1 = c(rep(0.2, 9), rep(0.8, 9), rep(2, 9))
gamma1 = rep(c(0, 0, 0, 0.001, 0.001, 0.001, 0.01, 0.01, 0.01), 3)
lambda1 = rep(c("(2,4,6,8,10)","(5,15,30,60,120)","(5,5,5,5,120)"), 9)


get_results_MRC =  function(file_name, theta1, gamma1, lambda1){
  results = readRDS(paste0("results/", file_name))
  bias = results$bias %>% mean()
  RMSE = results$RMSE %>% mean()
  MAE = results$MAE %>% mean()
  CP = results$coverage_prob
  
  data = purrr::map_dfc(
    .x = c(1:5, 7:10, 13:15, 19:20, 25), 
    .f = function(x){ sapply(results$std_norm, "[[", x) } 
  )
  data1 = data %>% tidyr::pivot_longer(cols = everything(), values_to = "x", names_to = "groups")
  
  plot = ggplot(data = data1, aes(x = x)) +
    labs(color = "", x = "Error", y = "Density", title = "Density plot", subtitle = 
           paste0("With theta = ", theta1, ", gamma\U00B2 = ", gamma1, ", lambda = ", lambda1 ))  +
    theme(legend.position = c(0.09, 0.92), legend.background = element_blank()) +
    geom_line(stat = "density", alpha = 0.4, aes(color = groups), show.legend = FALSE) +
    stat_function(fun = dnorm, aes(color = "N(0,1)"), col = "black", geom = "line", size = .5) +
    xlim(c(-15,5))
  print(plot)
  
  ggsave(plot = plot, filename = paste0("plot_", file_name,".pdf"), width = 14, height = 10, units = "cm", path = "~/Mit drev/AAU/Projekter/P9-projekt/P9_project/Plots")
  
  return(list(
    "bias" = bias,
    "RMSE" = RMSE,
    "MAE" = MAE,
    "Coverage prob" = CP
  ))
}


get_results_MRC(file_name = file_names_MRC[5], theta = theta[5], gamma = gamma[5], lambda = lambda[5])


#MRC_results = purrr::pmap(
#  .l = list(setNames(file_names_MRC, file_names_MRC), theta, gamma, lambda),
#  .f = get_results_MRC
#)

get_results_HY =  function(file_name){
  results = readRDS(paste0("results/", file_name))
  bias = results$bias %>% mean()
  RMSE = results$RMSE %>% mean()
  MAE = results$MAE %>% mean()
  
  return(list(
    "bias" = bias,
    "RMSE" = RMSE,
    "MAE" = MAE
  ))
}

HY_results = purrr::map(
  .x = setNames(file_names_HY, file_names_HY),
  .f = get_results_HY
)

bias = purrr::map_dbl(HY_results, "bias") %>% round(digits = 5)
RMSE = purrr::map_dbl(HY_results, "RMSE") %>% round(digits = 3)
MAE = purrr::map_dbl(HY_results, "MAE") %>% round(digits = 3)





