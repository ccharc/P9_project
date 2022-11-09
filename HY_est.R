library(ggplot2)
library(highfrequency)
library(xts)
library(data.table)
library(dplyr)
R.utils::sourceDirectory("functions", modifiedOnly = FALSE)

# configs 
X0 = 1
b = 0.03
beta0 = -5/16
beta1 = 1/8
alpha = -1/40
rho = -0.3
theta = 0.8
t_max = 1
n = 28801                       # 8 hours in seconds
gamma = sqrt(0.01)              # levels of volatility: 0, 0.001, 0.01
lambda1 = c(3, 5, 10, 30, 60)   # avg. waiting times until new price
#lambda1 = c(2, 4, 7, 10, 15)
lambda2 = lambda1*2
set.seed(101)
n_assets = 5

#--------------------
#   HY estimator
#--------------------

# W needs to be specified outside simulate_price
W_increments = sqrt(t_max/n)*rnorm(n+1,0,1)
W = c(0,cumsum(W_increments))
simulate_price(n, W)[1]

# creating price series
prices_and_sigmas = purrr::map(.x = rep(n, n_assets), .f = simulate_price, W = W)
price_list = purrr::map(prices_and_sigmas, .f = 1)
sigma_list = purrr::map(prices_and_sigmas, .f = 2)

# computing log-prices
price_list = purrr::map(
  .x = price_list,
  .f = function(x) {x %>% mutate("Price" = log(Price))}
)

# meantimes
get_meantime = function(pricetable){
  pricetable %>% pull(DT) %>% diff() %>% mean()
}

meantimes = purrr::map_dbl(
  .x = price_list,
  .f = get_meantime
)

# tranforming onto an equidistant grid
get_equi_prices = function(pricetable){ 

  meantime = get_meantime(pricetable)
  
  # price process on equidistant grid
  prices = pricetable %>% pull(Price)
  initial_price = prices[1]
  stepfunc = stepfun(x = pricetable %>% pull(DT), y = c(initial_price, prices), f = 0, right = 0)
  
  datetime = as.POSIXct("2022-10-31 01:00:00", tz = "EST") + lubridate::seconds((0:(nrow(pricetable)-1))*meantime)
  tibble( "DT" =  datetime, "Price" = stepfunc(datetime))
  
}

equi_price_list = purrr::map(
  .x = price_list,
  .f = get_equi_prices
)

# defining window size (here we is the same n for all assets
# we choose min{rows(price_list[i])}) -> unsure about this approach

n_median = purrr::map_dbl(
  .x = equi_price_list,
  .f = nrow
) %>%  
  median()

kn = floor(theta * sqrt(n_median))


#pre-averaging
get_preavg_prices = function(pricetable, kn){
 
  preavg_obs = pricetable %>% 
    pull(Price) %>% 
    preaverage(kn,gfunction)
  
  pricetable %>%
    filter(row_number() <= length(preavg_obs)) %>%  
    mutate(Price = preavg_obs)
   
}

preavg_price_list = purrr::map(
  .x = equi_price_list,
  .f = get_preavg_prices, 
  kn = kn
)


# computing the pre-averaged HY estimator
psi = 1/4

compute_HY_entry = function(
  equi_pricetable1, equi_pricetable2, preavg_pricetable1, preavg_pricetable2
)
{
  DT1 = equi_pricetable1 %>% pull(DT)
  DT2 = equi_pricetable2 %>% pull(DT)
  Price1 = preavg_pricetable1 %>% pull(Price)
  Price2 = preavg_pricetable2 %>% pull(Price)
  
  DT1_tib = tibble("left" = DT1) %>% 
    mutate(right = lead(DT1, kn-1)) %>% 
    filter(row_number() <= length(Price1))
  DT2_tib = tibble("left" = DT2) %>% 
    mutate(right = lead(DT2, kn-1)) %>% 
    filter(row_number() <= length(Price2))
  
  check_condition = function(y_left, y_right){
    DT1_tib$left < y_left & y_left <= DT1_tib$right | 
      DT1_tib$left < y_right & y_right <= DT1_tib$right
  }
  
  condition_df = purrr::map2_dfc(DT2_tib$left, DT2_tib$right, check_condition) %>% 
    mutate(across(everything(), as.integer))
  
  price_matrix = Price1 %*% t(Price2)
  
  1/((psi*kn)^2) * sum(c(price_matrix * as.matrix(condition_df)))
  
  # 
  # HY_entry = 0
  # for (i in 1:(length(Price1))){
  #   for (j in 1:(length(Price2))){
  #     if (as.logical(condition_df[i, j])) {
  #       HY_entry = HY_entry + Price1[i] * Price2[j]
  #     }
  #   }
  #   print(i)
  # }
  # HY_entry
  
}

compute_HY = function(equi_pricetable, preavg_pricetable){
  purrr::map2_dfc(
    .x = equi_pricetable,
    .y = preavg_pricetable,
    .f = function(x, y) {
      purrr::map2_dbl(
        .x = equi_pricetable,
        .y = preavg_pricetable,
        .f = compute_HY_entry,
        equi_pricetable1 = x,
        preavg_pricetable1 = y
      )
    }
  )
}

tictoc::tic()
compute_HY(equi_price_list, preavg_price_list)
tictoc::toc()



# Confidence intervals ----------------------------------------------------


# HY estimator on the interval [0,t], 0 <= t <= T
# t is datetime with time zone EST (e.g. t = as.POSIXct("2022-10-31 04:00:00", tz = "EST")) )
compute_HY_entry_timeinterval = function(
    equi_pricetable1, equi_pricetable2, preavg_pricetable1, preavg_pricetable2, t
)
{
  DT1 = equi_pricetable1 %>% pull(DT)
  DT2 = equi_pricetable2 %>% pull(DT)
  Price1 = preavg_pricetable1 %>% pull(Price)
  Price2 = preavg_pricetable2 %>% pull(Price)
  
  DT1_tib = tibble("left" = DT1) %>% 
    mutate(right = lead(DT1, kn-1)) %>% 
    filter(left <= t) %>%
    filter(row_number() <= nrow(.) - kn + 1)
  DT2_tib = tibble("left" = DT2) %>% 
    mutate(right = lead(DT2, kn-1)) %>% 
    filter(left <= t) %>%
    filter(row_number() <= nrow(.) - kn + 1)
  
  Price1 = Price1[1:(nrow(DT1_tib))]
  Price2 = Price2[1:(nrow(DT2_tib))]
  
  check_condition = function(y_left, y_right){
    DT1_tib$left < y_left & y_left <= DT1_tib$right | 
      DT1_tib$left < y_right & y_right <= DT1_tib$right
  }
  
  condition_df = purrr::map2_dfc(DT2_tib$left, DT2_tib$right, check_condition) %>% 
    mutate(across(everything(), as.integer))
  
  price_matrix = Price1 %*% t(Price2)
  
  1/((psi*kn)^2) * sum(c(price_matrix * as.matrix(condition_df)))
  
}



# define functions used for estimation of conditional covariance matrix 

# define the ration between the number of observations for each asset and 
# the total number of observations i.e. n_k / n (estimate of m_k)
get_mk = function(pricetable, k){
  nrow(pricetable[[k]]) / purrr::map(.x = pricetable, .f = nrow) %>% unlist() %>% sum()
}

# f function (time transformation function) (same for all k = 1,...,d)
ffunction = function(x){
  1/x
}

fprime = function(x){
  -1/x^2
}

# h function 
hfunction = function(pricetable, k, l, x){
  get_mk(pricetable, k) * fprime(x) / get_mk(pricetable, l) * fprime(x)
}

# 








