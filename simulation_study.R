library(ggplot2)
library(dplyr)
library(highfrequency)
library(xts)
library(data.table)
library(furrr)
library(parallel)
library(pbmcapply)

# configs 
X0 = 1
b = 0.03
beta0 = -5/16
beta1 = 1/8
alpha = -1/40
rho = -0.3
t_max = 1
n = 28801                       # 8 hours in seconds
gamma = sqrt(0.01)              # levels of volatility: 0, 0.001, 0.01
lambda1 = c(3, 5, 10, 30, 60)   # avg. waiting times until new price
#lambda1 = c(2, 2, 2, 2, 2)
lambda2 = lambda1*2
theta = 0.8
#delta = 0.1
#n_assets = 5
R.utils::sourceDirectory("functions", modifiedOnly = FALSE)


sim_study = function(n_assets){
  # W needs to be specified outside simulate_price
  W_increments = sqrt(t_max/n)*rnorm(n+1,0,1)
  W = c(0,cumsum(W_increments))

  prices_and_sigmas = purrr::map(.x = rep(n, n_assets), .f = simulate_price, W = W)
  price_list = purrr::map(prices_and_sigmas, .f = 1)
  sigma_list = purrr::map(prices_and_sigmas, .f = 2)
  df_prices = refreshTime(price_list)

  df_prices = purrr::map_dfc(
    .x = paste0("V", 1:n_assets), 
    .f = function(x) {df_prices %>% dplyr::pull(x) %>% log()}
    )%>% 
    setNames(paste0("V", 1:n_assets)) %>%
    as.data.table() 



  # The pre-averaging window
  kn = floor(theta * nrow(df_prices)^(1/2))

  # The MRC estimator
  mrc = compute_MRC(df_prices, n_assets, kn)
  #mrc = rMRCov(price_list)

  # The asymptotic covariance matrix
  ublb = matrix(create_ublb(n_assets, df_prices, kn), nrow = nrow(mrc), byrow = TRUE)
  
  IV = make_IV(sigma_list, n_assets)
  
  lb = IV - ublb
  ub = IV + ublb

  TF = lb < mrc & mrc < ub

  # The number of "TRUE's" in the matrix TF
  TF_sum = sum(TF)

  # making each entry N(0,1)
  std_norm = (mrc - IV)/((1/1.96)*ublb)

  mse_matrix = (mrc - IV)

  return(list("TF_sum" = TF_sum, "std_norm" = std_norm, "MSE" = mse_matrix, "MRC" = mrc, "QV" = IV))
}


sim_list = pbmclapply(rep(5, 100), sim_study, mc.cores = 8)

TF_sum_list = purrr::map(sim_list, .f = 1)
std_norm_list = purrr::map(sim_list, .f = 2)
MSE_list = purrr::map(sim_list, .f = 3)

sum(unlist(TF_sum_list), na.rm = TRUE)/2500

hist(sapply(std_norm_list, "[[", 1), breaks = 100)
# -------------------------------------------------------------------------




df = tibble::tibble("Time" = wait_times1, "Price" = Y_async)

# Plot
gg = ggplot() +
  geom_line(aes(x = (0:n)/n, y = exp(X), color = "Efficient")) +
  geom_line(aes(x = (0:n)/n, y = exp(Y), color = "Noisy"), linetype = "dashed") +
  geom_line(aes(x = wait_times1, y = exp(Y_async), color = "Non-synchronous")) +
  labs(
    title = "Simulation of prices", 
    subtitle = paste0("Number of observations: ", n),
    x = "Time", y = "Price", color = ""
  ) +
  scale_color_manual(values = c(
      "Efficient" = "tomato", 
      "Noisy" = "steelblue", 
      "Non-synchronous" = "black"
  )) +
  guides(color = guide_legend(
    override.aes = list(linetype = c("solid", "dashed", "solid"))
  ))

print(gg)


