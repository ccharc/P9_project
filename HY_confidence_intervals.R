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
ffunction = function(x){
  1/x
}

fprime = function(x){
  -1/x^2
}

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
      delta = delta
    )
  )
}

# define sequence ln
ln = function(n){
  1 / (log(n))^3
}

# estimate of the spot volatility Sigma

















