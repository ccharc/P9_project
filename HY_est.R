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
set.seed(101)

#--------------------
#   HY estimator
#--------------------


# creating price series
price_list = purrr::map(.x = rep(n, 5), .f = simulate_price)

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
    preaverage(kn)
  
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
  
  sum(c(price_matrix * as.matrix(condition_df)))
  
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

compute_HY(equi_price_list, preavg_price_list)




















