library(ggplot2)
library(dplyr)
library(highfrequency)
library(data.table)
source("config.R")
R.utils::sourceDirectory("functions", modifiedOnly = FALSE)

future::plan(future::multisession(), workers = future::availableCores() - 2)
seeds = 1:1000

theta = 0.8
gamma = sqrt(0.01)
lambda = lambda1

MRC_results = MRC_sim_study(seeds, lambda)







# -------------------------------------------------------------------------

results = readRDS("results/MRC_theta_08_gamma_01_lambda_5555120")




# plots -------------------------------------------------------------------


hist(TF_sum)
hist(MAE, breaks = 50)
hist(MAE, breaks = 10000, xlim = c(0, 5))
hist(RMSE, breaks = 50)
hist(RMSE, breaks = 50000, xlim = c(0, 5))

hist(sapply(std_norm_list, "[[", 1), breaks = 100, probability = TRUE)

hist(sapply(MSE_list, "[[", 1), breaks = 100, probability = F)






