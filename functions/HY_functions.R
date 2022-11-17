get_HY_est_int_volatility = function(seed){
  # tictoc::tic()
  
  set.seed(seed)
  
  # W needs to be specified outside simulate_price
  W_increments = sqrt(t_max/n)*rnorm(n+1,0,1)
  W = c(0,cumsum(W_increments))
  
  # creating price series
  prices_and_sigmas = purrr::map(
    .x = rep(n, n_assets) %>% setNames(paste0("Asset", 1:n_assets)), 
    .f = simulate_price, 
    W = W
  )
  price_list = purrr::map(prices_and_sigmas, .f = "price")
  sigma_list = purrr::map(prices_and_sigmas, .f = "sigma")
  
  # computing log-prices
  price_list = purrr::map(
    .x = price_list,
    .f = function(x) {x %>% mutate("Price" = log(Price))}
  )
  
  # meantimes
  meantimes = purrr::map_dbl(
    .x = price_list,
    .f = get_meantime
  )
  
  # tranforming onto an equidistant grid
  equi_price_list = purrr::map(
    .x = price_list,
    .f = get_equi_prices
  )
  
  # defining window size (here we use the same n for all assets
  # we choose median{rows(price_list[i])}) -> unsure about this approach
  n_median = purrr::map_dbl(
    .x = equi_price_list,
    .f = nrow
  ) %>%  
    median()
  
  kn = floor(theta * sqrt(n_median))
  
  #pre-averaging
  preavg_price_list = purrr::map(
    .x = equi_price_list,
    .f = get_preavg_prices, 
    kn = kn
  )
  
  # estimating integrated volatility using the pre-averaged HY estimator
  HY_estimated_int_volatility = compute_HY(equi_price_list, preavg_price_list, kn = kn)
  
  # tictoc::toc()
  
  return(HY_estimated_int_volatility)
}

get_meantime = function(pricetable){
  pricetable %>% pull(DT) %>% diff() %>% mean()
}

get_equi_prices = function(pricetable){ 
  meantime = get_meantime(pricetable)
  
  # price process on equidistant grid
  prices = pricetable %>% pull(Price)
  initial_price = prices[1]
  stepfunc = stepfun(x = pricetable %>% pull(DT), y = c(initial_price, prices), f = 0, right = 0)
  
  datetime = as.POSIXct("2022-10-31 01:00:00", tz = "EST") + lubridate::seconds((0:(nrow(pricetable)-1))*meantime)
  tibble( "DT" =  datetime, "Price" = stepfunc(datetime))
}

get_preavg_prices = function(pricetable, kn){
  preavg_obs = pricetable %>% 
    pull(Price) %>% 
    preaverage(kn, gfunction)
  
  pricetable %>%
    filter(row_number() <= length(preavg_obs)) %>%  
    mutate(Price = preavg_obs)
}

compute_HY_entry = function(
  equi_pricetable1, equi_pricetable2, preavg_pricetable1, preavg_pricetable2, kn
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
  
  condition_df = furrr::future_map2_dfc(DT2_tib$left, DT2_tib$right, check_condition) %>% 
    mutate(across(everything(), as.integer)) %>% suppressMessages()
  
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

compute_HY = function(equi_pricetable, preavg_pricetable, kn){
  furrr::future_map2_dfc(
    .x = equi_pricetable,
    .y = preavg_pricetable,
    .f = function(x, y) {
      furrr::future_map2_dbl(
        .x = equi_pricetable,
        .y = preavg_pricetable,
        .f = compute_HY_entry,
        equi_pricetable1 = x,
        preavg_pricetable1 = y,
        kn = kn,
        .progress = FALSE
      )
    },
    .progress = FALSE
  )
}