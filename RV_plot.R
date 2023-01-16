library(ggplot2)
library(tidyverse)
library(latex2exp)

RV = function(n){
  # Simulating brownian motion
  BM_increments = sqrt( 1 / n ) * rnorm(n, 0, 1)
  BM = c(0,cumsum(BM_increments))

  # Defining grid of time points
  t = seq(0, 1, 1 / n)

  # Specifying drift and diffusion
  mu = 1
  sigma = 0.01

  # geometric brownian motion
  S_t = mu * exp( ( 1 - (sigma^2) / 2) * t + sigma * BM)
  micro_noise = rnorm(n = length(S_t), mean = 0, sd = 0.001)
  S_t_noisy = S_t + micro_noise

  # The realized volatility
  RV = sum((diff(S_t_noisy))^2)
  return(RV)
}

#n_s = c(10000, 100000, 500000, 1000000, 2000000, 5000000)

n_s = seq(1000, 10000000, 1000)

tictoc::tic()
future::plan(future::multisession(), workers = future::availableCores() - 2)
RV_list = furrr::future_map_dbl(.x = n_s, 
                            .f = RV, 
                            .progress = TRUE, 
                            .options = furrr::furrr_options(seed = TRUE))
tictoc::toc()

delta_n = 1/n_s

data = tibble(`Delta`= delta_n, `RV` = RV_list)

p = ggplot(data = data , aes(x = Delta, y = log(RV))) +
  geom_line() +
  xlab(TeX("$\\Delta_n$")) +
  ylab(TeX("$log(RV)$")) +
  theme_bw() + 
  ggtitle(TeX("The log of the realized covariation as a function of $\\Delta_n$"))



