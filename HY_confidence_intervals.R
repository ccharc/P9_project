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

# f function (time transformation function) (same for all k = 1,...,d)

# psi functions
psi_function = function(s, delta){
  ofunc = function(x){
    if (x == 1/2){
      return(1/2 * ( 1/4 * ( (1-2*(x-delta))^2 * (1/2 - (x-delta)) / (abs(1/2 - (x-delta))) -1 ) +
                1/4 * ( (1-2*(x+delta))^2 * (1/2 - (x+delta)) / (abs(1/2 - (x+delta))) -1 ) ) )
    }
    else {
      return(1/4 * ( (1-2*x)^2 * (1/2 - x) / (abs(1/2 - x)) -1 ) )
    }
  }
  
  get_integrand1 = function(s,u){
    1/2 * ( u * (2 + ofunc(u+s+1) - ofunc(u+s-1)) ) 
  }
  
  get_integrand2 = function(s,u){
    1/2 * ( (1-u) * (2 + ofunc(u+s+1) - ofunc(u+s-1)) ) 
  }
  
  
  psi = function(s, delta){
    int1 = seq(0,1/2,delta)
    int2 = seq(1/2 + delta,1,delta)
    
    # sum( c(get_integrand1(s,int1)*delta, get_integrand2(s,int2)*delta) )
    
    delta * (sum(purrr::map_dbl(.x = int1, .f = get_integrand1, s = s)) +
      sum(purrr::map_dbl(.x = int2, .f = get_integrand2, s = s))) 
  }
  
  return(psi(s,delta))
}


psibar_function = function(s, delta){
  ofunc = function(x){
    1/2 * (1 - abs(2*x - 1))
  }
  
  get_integrand1 = function(s,u){
    u * (ofunc(u+s+1) - ofunc(u+s-1)) 
  }
  
  get_integrand2 = function(s,u){
    (1-u) * (ofunc(u+s+1) - ofunc(u+s-1)) 
  }
  
  psibar = function(s, delta){
    int1 = seq(0,1/2,delta)
    int2 = seq(1/2 + delta,1,delta)
    
    # sum( c(get_integrand1(s,int1)*delta, get_integrand2(s,int2)*delta) )
    
    delta * (sum(purrr::map_dbl(.x = int1, .f = get_integrand1, s = s)) +
               sum(purrr::map_dbl(.x = int2, .f = get_integrand2, s = s)))
  }
  
  return(psibar(s,delta))
}

psitilde_function = function(s, delta){
  ofunc = function(x){
    1/2 * (1 - abs(2*x - 1))
  }
  
  get_integrand1 = function(s,u){
    ofunc(u+s+1) - ofunc(u+s-1)
  }
  
  get_integrand2 = function(s,u){
    -(ofunc(u+s+1) - ofunc(u+s-1)) 
  }
  
  psitilde = function(s, delta){
    int1 = seq(0,1/2,delta)
    int2 = seq(1/2 + delta,1,delta)
    
    # sum( c(get_integrand1(s,int1)*delta, get_integrand2(s,int2)*delta) )
    
    delta * (sum(purrr::map_dbl(.x = int1, .f = get_integrand1, s = s)) +
               sum(purrr::map_dbl(.x = int2, .f = get_integrand2, s = s)))
  }
  
  return(psitilde(s,delta))
}


# gamma functions
gamma_function = function(u, d, delta){
  
  int = seq(-2,2,delta)
  
  psi = function(u, d, s, delta){
    u * d * psi_function(s, delta) * psi_function(s, delta) * delta
  }
  
  sum(
    purrr::map_dbl(
      .x = int,
      .f = psi,
      delta = delta, d = d, u = u
    )
  )
}

gammabar_function = function(delta){
  
  int = seq(-2,2,delta)
  
  psibar = function(s, delta){
    psibar_function(s, delta) * psibar_function(s, delta) * delta
  }
  
  sum(
    purrr::map_dbl(
      .x = int,
      .f = psibar,
      delta = delta
    )
  )
}

gammatilde_function = function(u,d,delta){
  
  int = seq(-2,2,delta)
  
  psitilde = function(u,d,s,delta){
    - 1/d * 1/u^2 * psitilde_function(s, delta) * psitilde_function(s, delta) * delta
  }
  
  sum(
    purrr::map_dbl(
      .x = int,
      .f = psitilde,
      delta = delta, d = d, u = u
    )
  )
}



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






# define sequence ln
ln = function(n){
  1 / (log(n))^3
}

ln(n_median)

# function values of gamma function used in Riemann sum of the interval [0,1], delta = 0.01
gamma_vec = future::plan(future::multisession(), workers = future::availableCores() - 2)
tictoc::tic()
furrr::future_map(
  .x = seq(0,1-0.01,0.01),
  .f = gamma_function,
  d = n_assets, delta = 0.01, 
  .progress = TRUE
)
tictoc::toc()

gammabar_vec = rep(gammabar_function(0.01), 1/0.01)

gammatilde_vec = future::plan(future::multisession(), workers = future::availableCores() - 2)
tictoc::tic()
furrr::future_map(
  .x = seq(0,1-0.01,0.01),
  .f = gammatilde_function,
  d = n_assets, delta = 0.01, 
  .progress = TRUE
)
tictoc::toc()

# spot volatility estimator (used in the Riemann sum)
HY_ln = compute_HY_entry_timeinterval(equi_pricetable1 = equi_price_list[[1]],equi_pricetable2 = equi_price_list[[1]], 
                              preavg_pricetable1 = preavg_price_list[[1]], preavg_pricetable2 = preavg_price_list[[1]],
                              as.POSIXct("2022-10-31 02:00:00", tz = "EST") + ln(n_median) * 28800)

HY_s = future::plan(future::multisession(), workers = future::availableCores() - 2)
tictoc::tic()
furrr::future_map(
  .x = as.POSIXct("2022-10-31 02:00:00", tz = "EST") + seq(0,1-0.01,0.01)*28800,
  .f = compute_HY_entry_timeinterval,
  equi_pricetable1 = equi_price_list[[1]],equi_pricetable2 = equi_price_list[[1]], 
  preavg_pricetable1 = preavg_price_list[[1]], preavg_pricetable2 = preavg_price_list[[1]],
  .progress = TRUE
)
tictoc::toc()

for (i in 1:length(seq(0,ln(n_median), 0.01))){
  HY_s[i] = HY_ln
}


HY_s_ln = future::plan(future::multisession(), workers = future::availableCores() - 2)
tictoc::tic()
furrr::future_map(
  .x = as.POSIXct("2022-10-31 02:00:00", tz = "EST") + (seq(0,1-0.01,0.01)-ln(n_median))*28800,
  .f = compute_HY_entry_timeinterval,
  equi_pricetable1 = equi_price_list[[1]],equi_pricetable2 = equi_price_list[[1]], 
  preavg_pricetable1 = preavg_price_list[[1]], preavg_pricetable2 = preavg_price_list[[1]],
  .progress = TRUE
)
tictoc::toc()

HY_s_ln[1]

for (i in 1:length(seq(0,ln(n_median), 0.01))){
  HY_s_ln[i] = 0
}

spot_vol = (HY_s - HY_s_ln) / ln(n_median)

# covariance of the noise process estimator (used in Riemann sum)
noisy_cov = - 1/length(price_list[[1]]$Price) * sum(diff(price_list[[1]]$Price)^2)

# conditional covariance matrix 
(0.01 * 1/psi^4 * ( theta * ( gamma_vec * spot_vol^2 + gamma_vec * spot_vol^2 ) +
           4/theta * ( noisy_cov * gammabar_vec * spot_vol ) +
           1/(theta^3) * ( noisy_cov^2 * gammatilde_vec + noisy_cov^2 * gammatilde_vec) ) ) %>% 
  sum()






