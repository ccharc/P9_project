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
lambda2 = lambda1*2
# set.seed(101)

#--------------------
#   HY estimator
#--------------------


# creating price series
price_list = purrr::map(.x = rep(n, 3), .f = simulate_price)

# tranforming onto an equidistant grid
get_equi_prices = function(pricetable){ 

  # equidistant grid for each asset
  meantime = pricetable %>% pull(DT) %>% diff() %>% mean()

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
preaverage(equi_price_list[[1]]$Price, kn)

purrr::map(
  .x = equi_price_list,
  .f = preaverage, 
  kn = kn
)




# example using rHYCOV
# library("xts")
# rHYCov(rData = as.xts(sampleOneMinuteData)["2001-08-05"],
#                period = 5, alignBy = "minutes", alignPeriod = 5, makeReturns = TRUE)
















