library(ggplot2)
library(highfrequency)
library(xts)
library(data.table)
library(dplyr)

# configs 
X0 = 1
b = 0.03
beta0 = -5/16
beta1 = 1/8
alpha = -1/40
rho = -0.3
t_max = 1
n = 28800                       # 8 hours in seconds
gamma = sqrt(0.01)              # levels of volatility: 0, 0.001, 0.01
lambda1 = c(3, 5, 10, 30, 60)   # avg. waiting times until new price
lambda2 = lambda1*2
# set.seed(101)

simulate_price = function(n)
{
  # Brownian motions
  BM_increments = sqrt(t_max/n)*rnorm(n+1,0,1)
  BM = c(0,cumsum(BM_increments))
  W_increments = sqrt(t_max/n)*rnorm(n+1,0,1)
  W = c(0,cumsum(W_increments))
  
  # OU specification for varrho
  varrho = c()
  varrho0 = rnorm(n = 1, mean = 0, sd = (-2*alpha)^(-1))
  varrho[1] = varrho0
  for (i in 1:n) {
    varrho[i+1] = alpha * varrho[i] + (BM[i+1] - BM[i])
  }
  
  # sigma spot volatility
  sigma = exp(beta0 + beta1 * varrho)
  
  # Diffusion components
  s1 = rho * sigma
  s2 = sqrt(1 - rho^2) * sigma
  
  # Simulate efficient log prices X (using Euler scheme)
  X = c()
  X[1] = X0
  delta = t_max / n
  for (i in 1:n) {
    X[i+1] = X[i] + b * delta + 
      s1[i] * (BM[i+1] - BM[i]) +
      s2[i] * (W[i+1] - W[i])
  }
  
  # Noisy log prices Y
  omega = sqrt(gamma^2 * sqrt((1/n) * sum(sigma^4)))
  micro_noise = rnorm(n = length(X), mean = 0, sd = omega^2) # perhaps not squared
  Y = X + micro_noise
  
  # Irregular and non-synchronous data using a poisson process
  get_poisson = function(lambda, n)
  {
    s = c()
    s[1] = 0
    while (sum(s) < 1) {
      iid_exp_var = rexp(50, 1/lambda)
      s = c(s, iid_exp_var/n)
    }
    cumsum(s)[cumsum(s) <= 1]
  }
  
  wait_times1 = get_poisson(lambda1, n)
  wait_times2 = get_poisson(lambda2, n)
  Y_step = stepfun(x = (1:n)/n, y = Y, f = 0, right = 0)
  Y_async = Y_step(wait_times1)
  
  # tibble::tibble("Time" = wait_times1, "Price" = Y_async)
  datetime = as.POSIXct("2022-10-31 09:00:00", tz = "EST") + lubridate::seconds(wait_times1*n)
  data.table("DT" = datetime, "Price" = Y_async)
}

price_list = purrr::map(.x = rep(n, 3), .f = simulate_price)


rMRCov(price_list, makePsd = TRUE)

df_prices = refreshTime(price_list)


preaverage = function(price, theta = 0.8, delta = 0.1)
{
  kn = floor(theta * length(price)^(1/2 + delta))
  Y_bar = c()
  for (i in 1:(length(price)-kn+1)) {
    Y_bar[i] = 1/kn * ( sum(price[(i+floor(kn/2)):(i+kn-1)]) - sum(price[i:(i+floor(kn/2)-1)]) )
  }
  return(Y_bar)
}

purrr::map(
  .x = paste0("V", 1:3), 
  .f = function(x) {df_prices %>% dplyr::pull(x) %>% preaverage()}
)

#--------------------
#   HY estimator
#--------------------


# creating price series
price_list = purrr::map(.x = rep(n, 3), .f = simulate_price)

# changing class from list to data.table
p1 = as.data.table(price_list[1])
p2 = as.data.table(price_list[2])
p3 = as.data.table(price_list[3])

datetime = as.POSIXct("2022-10-31 09:00:00", tz = "EST") + 
  lubridate::seconds(0:n)
p1_equi = c(p1$Price[1], rep(0,n))



for (i in 1:(n+1)){
  if 
}

data.table("DT" = datetime, "Price" = Y_async)

  
# defining window size (here we is the same n for all assets
# we choose min{rows(price_list[i])}) -> unsure about this approach
theta = 0.8

n_ws = min(nrow(p1), nrow(p2), nrow(p3))
kn = floor(theta * sqrt(n_ws))

# defining weight function g = min{x,1-x}
g = function (x) 
{
  x[x > (1 - x)] <- (1 - x)[x > (1 - x)]
  return(x)
}


# pre-averaging
preaverage_hy = function(price)
{
  return_price = diff(price$Price)
  Y_bar = c()

  for (i in 0:(nrow(price)-kn+1)) {
    Y_bar[i+1] = sum( g( 1:(kn-1)/kn ) * return_price[(i+1):(i+kn-1)] )
  }
  return(Y_bar)
}

pre_p1 = preaverage_hy(p1)
pre_p2 = preaverage_hy(p2)
pre_p3 = preaverage_hy(p3)



# example using rHYCOV
# library("xts")
# rHYCov(rData = as.xts(sampleOneMinuteData)["2001-08-05"],
#                period = 5, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)
















