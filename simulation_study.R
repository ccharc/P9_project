library(ggplot2)
library(dplyr)
library(highfrequency)
library(data.table)
# library(pbmcapply)
source("config.R")
R.utils::sourceDirectory("functions", modifiedOnly = FALSE)


MRC_monte_carlo = function(seed){
  
  set.seed(seed)
  
  # W needs to be specified outside simulate_price
  W_increments = sqrt(t_max/n)*rnorm(n+1,0,1)
  W = c(0,cumsum(W_increments))

  prices_and_sigmas = purrr::map(.x = rep(n, n_assets), .f = simulate_price, W = W)
  price_list = purrr::map(prices_and_sigmas, .f = "price")
  sigma_list = purrr::map(prices_and_sigmas, .f = "sigma")
  df_prices = refreshTime(price_list)

  df_prices = purrr::map_dfc(
    .x = paste0("V", 1:n_assets) %>% setNames(paste0("V", 1:n_assets)), 
    .f = function(x) {df_prices %>% dplyr::pull(x) %>% log()}
    ) %>%
    as.data.table() 

  # The pre-averaging window
  kn = floor(theta * nrow(df_prices)^(1/2))

  # The MRC estimator
  mrc = compute_MRC(df_prices, n_assets, kn)
  # mrc = rMRCov(price_list)

  # The asymptotic covariance matrix
  ublb = matrix(create_ublb(n_assets, df_prices, kn), nrow = nrow(mrc), byrow = TRUE)
  
  ublb = abs(ublb)
  
  IV = make_IV(sigma_list, n_assets)
  
  lb = IV - ublb
  ub = IV + ublb

  TF = lb < mrc & mrc < ub

  # The number of "TRUE's" in the matrix TF
  TF_sum = sum(TF)

  # making each entry N(0,1)
  std_norm = (mrc - IV)/((1/1.96)*ublb)

  mse_matrix = (mrc - IV)

  return(
    list(
      "TF_sum" = TF_sum, 
      "std_norm" = std_norm, 
      "MSE" = mse_matrix, 
      "MRC" = mrc, 
      "QV" = IV,
      "bounds" = ublb
    )
  )
}

# sim_list = pbmclapply(rep(5, 100), MRC_monte_carlo, mc.cores = 8)

seeds = 401:600

future::plan(future::multisession(), workers = future::availableCores() - 2)
tictoc::tic()
sim_list = furrr::future_map(
  .x = seeds, 
  .f = MRC_monte_carlo, 
  .options = furrr::furrr_options(seed = TRUE),
  .progress = TRUE
)
tictoc::toc()

purrr::map(sim_list, .f = "bounds")

TF_sum = purrr::map_dbl(sim_list, .f = "TF_sum")
std_norm_list = purrr::map(sim_list, .f = "std_norm")
MSE_list = purrr::map(sim_list, .f = "MSE")

sum(TF_sum, na.rm = TRUE)/(n_assets*n_assets*length(seeds))

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


