gfunction = function(x) 
{
  x[x > (1 - x)] <- (1 - x)[x > (1 - x)]
  return(x)
}

preaverage = function(price, kn, g)
{
  Y_bar = c()
  for (i in 1:(length(price)-kn+1)) {
    returns = diff(price)
    Y_bar[i] = sum( g((1:(kn-1))/kn) * returns[(i):(i+kn-2)] )
  }
  return(Y_bar)
}


compute_MRC = function(df_prices, n_assets, kn)
{
  Y_bar = purrr::map_dfc(
    .x = paste0("V", 1:n_assets) %>% setNames(paste0("Asset", 1:n_assets)), 
    .f = function(x, kn, g) {df_prices %>% dplyr::pull(x) %>% preaverage(kn, g)},
    kn = kn, g = gfunction
  ) %>%
    as.matrix() 
  
  # Y_sum = 0
  # for (i in 1:(nrow(df_prices)-kn+1)) {
  #   Y_sum = Y_sum + Y_bar[i,] %*% t(Y_bar[i,])
  # }
  Y_sum = t(Y_bar) %*% Y_bar    # equals the same as the for loop above
  
  psi1kn = kn * sum((gfunction((1:kn)/kn) - gfunction(((1:kn) - 1)/kn))^2)
  psi2kn = 1/kn * sum(gfunction((1:kn)/kn)^2)
  
  Y_sum * (nrow(df_prices)/(nrow(df_prices)-kn+2)) * (1/((1/12)*kn)) - psi1kn * (1/(theta^2 * psi2kn)) * 
    (1/(2*nrow(df_prices))) * t(diff(as.matrix(df_prices))) %*% diff(as.matrix(df_prices))
}

# preaverage = function(price)
# {
#   Y_bar = c()
#   for (i in 1:(length(price)-kn+1)) {
#     Y_bar[i] = 1/kn * ( sum(price[(i+floor(kn/2)):(i+kn-1)]) - sum(price[i:(i+floor(kn/2)-1)]) )
#   }
#   return(Y_bar)
# }