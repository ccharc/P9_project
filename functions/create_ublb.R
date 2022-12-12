# Defining the weigth function g1, g2 and g3 to be used in avar
g1 = function(x){
  y <- sin(1*x*pi)
  return(y)
}

g2 = function(x){
  y <- sin(2*x*pi)
  return(y)
}

g3 = function(x){
  y <- sin(3*x*pi)
  return(y)
}

create_Vn = function(g, n_assets, df_prices, kn){
  Y_bar = purrr::map_dfc(
    .x = paste0("V", 1:n_assets) %>% setNames(paste0("Asset_", 1:n_assets)), 
    .f = function(x, kn, g) {df_prices %>% dplyr::pull(x) %>% preaverage(kn, g)},
    kn = kn, g = g
  ) %>%
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
    s2 = s2 + ( chi[[i]] %*% t(chi[[i+kn]]) + chi[[i+kn]] %*% t(chi[[i]]) )
  }
  
  Vn = s1 - 1/2 * s2
  return(Vn)
}

create_ublb = function(n_assets, df_prices, kn){ 
  # Computing the asymptotic covariance matrix
  avar = C_weight[1] * create_Vn(g1, n_assets, df_prices, kn) +
    C_weight[2] * create_Vn(g2, n_assets, df_prices, kn) +
    C_weight[3] * create_Vn(g3, n_assets, df_prices, kn)
  
  # Only the diagonals are needed when creating conf. interval
  return(list(
    "99" = 2.576 * nrow(df_prices)^(-1/4) * sqrt(abs(diag(avar))),
    "95" = 1.96 * nrow(df_prices)^(-1/4) * sqrt(abs(diag(avar))),
    "90" = 1.645 * nrow(df_prices)^(-1/4) * sqrt(abs(diag(avar))),
    "80" = 1.28 * nrow(df_prices)^(-1/4) * sqrt(abs(diag(avar))),
    "70" = 1.036 * nrow(df_prices)^(-1/4) * sqrt(abs(diag(avar)))
  ))
  
}



