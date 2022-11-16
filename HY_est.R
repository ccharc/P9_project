library(ggplot2)
library(highfrequency)
# library(xts)
library(data.table)
library(dplyr)
source("config.R")
R.utils::sourceDirectory("functions", modifiedOnly = FALSE)

# HY estimator ------------------------------------------------------------
psi = 1/4
seed = 101
future::plan(future::multisession(), workers = future::availableCores() - 2)

seeds = 401:405
tictoc::tic()
purrr::map(
  .x = seeds, 
  .f = get_HY_est_int_volatility 
  # .options = furrr::furrr_options(seed = TRUE),
  # .progress = TRUE
)
tictoc::toc()


# Confidence intervals ----------------------------------------------------


# HY estimator on the interval [0,t], 0 <= t <= T
# t is POSIXct with time zone EST 
#   (e.g. t = as.POSIXct("2022-10-31 04:00:00", tz = "EST")) )
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

# psi functions
# we redefine the g function such that g(x) = 0 for x notin [0,1]
gfunc = function(x) 
{
  if (x >= 0 & x <= 1){
    x[x > (1 - x)] <- (1 - x)[x > (1 - x)]
    return(x)
  }
  else return(0)
}

gfunc_prime = function(x) {
  if (x >= 0 & x <= 1/2) {
    return(1)
  }
  else if (x > 1/2 & x <= 1) {
    return(-1)
  }
  else return(0)
}


compute_psi_integrand = function(u,v){
  gfunc(u) * gfunc(v)
}

compute_psibar_integrand = function(u,v){
  gfunc(u) * gfunc_prime(v)
}

compute_psitilde_integrand = function(u,v){
  gfunc_prime(u) * gfunc_prime(v) 
}


psi_function = function(s,x){
  lower = (u)
}

seq(1, 10, 0.1) 








