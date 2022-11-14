# Defining the weigth function g1, g2 and g3 to be used in avar
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

create_ublb = function(x){ 
  # Computing the asymptotic covariance matrix
  avar = C_weight[1]*create_Vn(g1) + C_weight[2]*create_Vn(g2) + C_weight[3]*create_Vn(g3)
  
  # Only the diagonals are needed when creating conf. intervals
  1.96 * nrow(x)^(-1/4) * diag(avar)
}


