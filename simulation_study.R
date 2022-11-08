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
lambda1 = c(3, 5, 10, 30, 60)   # avg. waiting times until new price
lambda2 = lambda1*2
theta = 0.8
delta = 0.1
n_assets = 5
# set.seed(101)

# W needs to be specified outside simulate_price
W_increments = sqrt(t_max/n)*rnorm(n+1,0,1)
W = c(0,cumsum(W_increments))
price_list = purrr::map(.x = rep(n, n_assets), .f = simulate_price, W = W)
df_prices = refreshTime(price_list)

kn = floor(theta * nrow(df_prices)^(1/2 + delta))

compute_MRC(df_prices)


create_Vn = function(g){
  Y_bar = purrr::map_dfc(
    .x = paste0("V", 1:n_assets), 
    .f = function(x, kn, g) {df_prices %>% dplyr::pull(x) %>% preaverage(kn, g)},
    kn = kn, g = g
  ) %>% 
    setNames(paste0("Asset_", 1:n_assets)) %>%
    as.matrix() 

  chi = list()
  for (i in 1:nrow(Y_bar)) {
    chi[[i]] = c(Y_bar[i,] %*% t(Y_bar[i,]))
  }

  s1 = 0
  for (i in 1:(nrow(Y_bar))) {
    s1 = s1 + chi[[i]] %*% t(chi[[i]])
  }
  s2 = 0
  for (i in 1:(nrow(Y_bar)-kn)) {
    s2 = s2 + 1/2 * ( chi[[i]] %*% t(chi[[i+kn]]) + chi[[i+kn]] %*% t(chi[[i]]) )
  }
  Vn = s1 - s2
  return(Vn)
}
create_Vn(gfunction) # example

phi1_g1 = 17/15
phi2_g1 = 1/630
phi1_g2 = 2/15
phi2_g2 = 1/105
phi1_g3 = 34/21
phi2_g3 = 1/630

A = matrix(c(theta^2*phi2_g1, phi1_g1*phi2_g1 , phi1_g1^2/theta^2,
             theta^2*phi2_g2, phi1_g2*phi2_g2 , phi1_g2^2/theta^2,
             theta^2*phi2_g3, phi1_g3*phi2_g3 , phi1_g3^2/theta^2),
           nrow = 3, ncol = 3, byrow = TRUE)

A_inv = solve(A)

C_vec = c((2*theta*(151/80640))/((1/12)^2), (2*(1/96))/((1/12)^2), (2*(1/6))/((1/12)^2*theta^3)) # Skal disse størrelser mon regnes ud fra g eller g1, g2 & g3?
C_weight = C_vec %*% A


# Defining the 
g1 = function(x){
  y <- x*(1-x)^2
  return(y)
}

g2 = function(x){
  y <- x^2*(1-x)
  return(y)
}

g3 = function(x){
  y <- x^2*(1-x)^2
  return(y)
}

# Computing the asymptotic covariance matrix
avar = C_weight[1]*create_Vn(g1) + C_weight[2]*create_Vn(g2) + C_weight[3]*create_Vn(g3)
diag(avar)


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


