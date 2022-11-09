library(ggplot2)
library(dplyr)
library(highfrequency)
library(xts)
library(data.table)
R.utils::sourceDirectory("functions", modifiedOnly = FALSE)

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
#lambda1 = c(3, 5, 10, 30, 60)  # avg. waiting times until new price
lambda1 = c(2, 2, 2, 2, 2)
lambda2 = lambda1*2
theta = 0.8
delta = 0.1
n_assets = 5
set.seed(100)

# W needs to be specified outside simulate_price
W_increments = sqrt(t_max/n)*rnorm(n+1,0,1)
W = c(0,cumsum(W_increments))


price_list = purrr::map(.x = rep(n, n_assets), .f = function(x, W) as.data.table(simulate_price(x, W)[1]), W = W)
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
mrc = compute_MRC(df_prices)
rMRCov(price_list)

# The asymptotic covariance matrix
avar = matrix(create_ublb(df_prices), nrow = nrow(mrc))

lb = mrc - avar
ub = mrc + avar











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


