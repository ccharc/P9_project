MRC_monte_carlo = function(seed, lambda){
  set.seed(seed)
  
  # W needs to be specified outside simulate_price
  W_increments = sqrt(t_max/n)*rnorm(n+1,0,1)
  W = c(0,cumsum(W_increments))
  
  prices_and_sigmas = purrr::map2(
    .x = rep(n, n_assets), 
    .y = lambda,
    .f = simulate_price, 
    W = W
  )
  price_list = purrr::map(prices_and_sigmas, .f = "price")
  sigma_list = purrr::map(prices_and_sigmas, .f = "sigma")
  df_prices = refreshTime(price_list)
  #df_prices = as.data.table(price_list)
  
  df_prices = purrr::map_dfc(
    .x = paste0("V", 1:n_assets) %>% setNames(paste0("V", 1:n_assets)), 
    .f = function(x) {df_prices %>% dplyr::pull(x) %>% log()}
  ) %>%
    as.data.table() 
  
  # The pre-averaging window
  kn = floor(theta * nrow(df_prices)^(1/2))
  
  # The MRC estimator
  mrc = compute_MRC(df_prices, n_assets, kn)
  #mrc = rMRCov(price_list)
  
  # The asymptotic covariance matrix
  ublb = matrix(create_ublb(n_assets, df_prices, kn), nrow = nrow(mrc), byrow = TRUE)
  
  ublb = abs(ublb)
  
  IV = make_IV(sigma_list, n_assets)
  
  lb = IV - ublb
  ub = IV + ublb
  
  TF = lb <= mrc & mrc <= ub
  
  # The number of "TRUE's" in the matrix TF
  TF_sum = sum(TF)
  
  # making each entry N(0,1)
  std_norm = (mrc - IV)/((1/1.96)*ublb)
  
  bias_matrix = (IV - mrc)
  mae = mean(abs(mrc - IV))
  rmse = sqrt(mean((mrc - IV)^2))
  
  return(
    list(
      "TF_sum" = TF_sum, 
      "std_norm" = std_norm, 
      "bias" = bias_matrix,
      "MAE" = mae,
      "RMSE" = rmse
      # "MRC" = mrc, 
      # "QV" = IV
    )
  )
}

MRC_sim_study = function(seeds, lambda){
  tictoc::tic()
  sim_list = furrr::future_map(
    .x = seeds, 
    .f = MRC_monte_carlo,
    lambda = lambda,
    .options = furrr::furrr_options(seed = TRUE),
    .progress = TRUE
  )
  
  TF_sum = purrr::map_dbl(sim_list, .f = "TF_sum")
  std_norm_list = purrr::map(sim_list, .f = "std_norm")
  bias_list = purrr::map(sim_list, .f = "bias")
  MAE = purrr::map_dbl(sim_list, .f = "MAE")
  RMSE = purrr::map_dbl(sim_list, .f = "RMSE")
  # MRC_list = purrr::map(sim_list, .f = "MRC")
  # QV_list = purrr::map(sim_list, .f = "QV")
  
  number_of_na = length(which(is.na(TF_sum)))
  cvg_prob = sum(TF_sum, na.rm = TRUE)/(n_assets^2*length(seeds) - number_of_na*n_assets^2)
  
  results = list(
    "TF_sum" = TF_sum,
    "std_norm" = std_norm_list,
    "bias" = bias_list,
    "MAE" = MAE,
    "RMSE" = RMSE,
    "coverage_prob" = cvg_prob
  )
  
  fs::dir_create("results")
  file_path = paste0(
    "results/MRC_theta_", theta, 
    "_gamma_", gamma, 
    "_lambda_", paste(lambda, sep = "", collapse = "")
  ) %>% 
    stringr::str_replace_all(pattern = "\\.", replacement = "")
  saveRDS(results, file = file_path)
  
  tictoc::toc()
  
  return(results)
}
