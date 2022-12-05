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

results = readRDS("results/MRC_theta_08_gamma_001_lambda_246810")
sum(results$TF_sum, na.rm = T)/25000
mean(results$MAE)



plot(density(sapply(results$std_norm, "[[", 8), na.rm = TRUE), ylim = c(0, 0.5), xlim = c(-5, 5))
curve(dnorm(x), add = TRUE, col = "red")
# plots -------------------------------------------------------------------


hist(TF_sum)
hist(MAE, breaks = 50)
hist(MAE, breaks = 10000, xlim = c(0, 5))
hist(RMSE, breaks = 50)
hist(RMSE, breaks = 50000, xlim = c(0, 5))

hist(sapply(std_norm_list, "[[", 1), breaks = 100, probability = TRUE)

hist(sapply(MSE_list, "[[", 1), breaks = 100, probability = F)






