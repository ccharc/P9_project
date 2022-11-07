gfunction = function(x) 
{
  x[x > (1 - x)] <- (1 - x)[x > (1 - x)]
  return(x)
}

preaverage = function(price, kn)
{
  Y_bar = c()
  for (i in 1:(length(price)-kn+1)) {
    returns = diff(price)
    Y_bar[i] = sum( gfunction((1:(kn-1))/kn) * returns[(i):(i+kn-2)] )
  }
  return(Y_bar)
}


compute_MRC = function(df_prices)
{
  Y_bar = purrr::map_dfc(
    .x = paste0("V", 1:n_assets), 
    .f = function(x) {df_prices %>% dplyr::pull(x) %>% preaverage()}
  ) %>% 
    setNames(paste0("Asset_", 1:n_assets)) %>%
    as.matrix() 
  
  # Y_sum = 0
  # for (i in 1:(nrow(df_prices)-kn+1)) {
  #   Y_sum = Y_sum + Y_bar[i,] %*% t(Y_bar[i,])
  # }
  Y_sum = t(Y_bar) %*% Y_bar    # equals the same as the for loop above
  
  Y_sum * (nrow(df_prices)/(nrow(df_prices)-kn+2)) * 1/((1/12)*kn)
}

# preaverage = function(price)
# {
#   Y_bar = c()
#   for (i in 1:(length(price)-kn+1)) {
#     Y_bar[i] = 1/kn * ( sum(price[(i+floor(kn/2)):(i+kn-1)]) - sum(price[i:(i+floor(kn/2)-1)]) )
#   }
#   return(Y_bar)
# }