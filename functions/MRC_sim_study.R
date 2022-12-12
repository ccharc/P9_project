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
  ublb99 = matrix(create_ublb(n_assets, df_prices, kn)$`99`, nrow = nrow(mrc), byrow = TRUE)
  ublb95 = matrix(create_ublb(n_assets, df_prices, kn)$`95`, nrow = nrow(mrc), byrow = TRUE)
  ublb90 = matrix(create_ublb(n_assets, df_prices, kn)$`90`, nrow = nrow(mrc), byrow = TRUE)
  ublb80 = matrix(create_ublb(n_assets, df_prices, kn)$`80`, nrow = nrow(mrc), byrow = TRUE)
  ublb70 = matrix(create_ublb(n_assets, df_prices, kn)$`70`, nrow = nrow(mrc), byrow = TRUE)
  
  ublb99 = abs(ublb99)
  ublb95 = abs(ublb95)
  ublb90 = abs(ublb90)
  ublb80 = abs(ublb80)
  ublb70 = abs(ublb70)
  
  IV = make_IV(sigma_list, n_assets)
  
  lb99 = IV - ublb99
  ub99 = IV + ublb99
  TF99 = lb99 <= mrc & mrc <= ub99
  
  lb95 = IV - ublb95
  ub95 = IV + ublb95
  TF95 = lb95 <= mrc & mrc <= ub95
  
  lb90 = IV - ublb90
  ub90 = IV + ublb90
  TF90 = lb90 <= mrc & mrc <= ub90
  
  lb80 = IV - ublb80
  ub80 = IV + ublb80
  TF80 = lb80 <= mrc & mrc <= ub80
  
  lb70 = IV - ublb70
  ub70 = IV + ublb70
  TF70 = lb70 <= mrc & mrc <= ub70
  
  # The number of "TRUE's" in the matrix TF
  TF_sum99 = sum(TF99)
  TF_sum95 = sum(TF95)
  TF_sum90 = sum(TF90)
  TF_sum80 = sum(TF80)
  TF_sum70 = sum(TF70)
  
  # making each entry N(0,1)
  std_norm = (mrc - IV)/((1/1.96)*ublb95)
  
  bias = mean(IV - mrc, na.rm = TRUE)
  mae = mean(abs(mrc - IV), na.rm = TRUE)
  rmse = sqrt(mean((mrc - IV)^2, na.rm = TRUE))
  
  return(
    list(
      "TF_sum" = list(
        "TF_sum99" = TF_sum99, 
        "TF_sum95" = TF_sum95,
        "TF_sum90" = TF_sum90,
        "TF_sum80" = TF_sum80,
        "TF_sum70" = TF_sum70), 
      "std_norm" = std_norm, 
      "bias" = bias,
      "MAE" = mae,
      "RMSE" = rmse,
      "MRC" = mrc,
      "QV" = IV
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
  
  TF_sum = purrr::map(sim_list, .f = "TF_sum")
  std_norm_list = purrr::map(sim_list, .f = "std_norm")
  bias = purrr::map_dbl(sim_list, .f = "bias")
  MAE = purrr::map_dbl(sim_list, .f = "MAE")
  RMSE = purrr::map_dbl(sim_list, .f = "RMSE")
  MRC_list = purrr::map(sim_list, .f = "MRC")
  QV_list = purrr::map(sim_list, .f = "QV")

  number_of_na = length(which(is.na(purrr::map(.x = TF_sum, .f = list(1,1)) %>% unlist())))
  cvg_prob99 = sum(purrr::map(.x = TF_sum, .f = list(1,1)) %>% unlist(), na.rm = TRUE)/(n_assets^2*length(seeds) - number_of_na*n_assets^2)
  cvg_prob95 = sum(purrr::map(.x = TF_sum, .f = list(2,1)) %>% unlist(), na.rm = TRUE)/(n_assets^2*length(seeds) - number_of_na*n_assets^2)
  cvg_prob90 = sum(purrr::map(.x = TF_sum, .f = list(3,1)) %>% unlist(), na.rm = TRUE)/(n_assets^2*length(seeds) - number_of_na*n_assets^2)
  cvg_prob80 = sum(purrr::map(.x = TF_sum, .f = list(4,1)) %>% unlist(), na.rm = TRUE)/(n_assets^2*length(seeds) - number_of_na*n_assets^2)
  cvg_prob70 = sum(purrr::map(.x = TF_sum, .f = list(5,1)) %>% unlist(), na.rm = TRUE)/(n_assets^2*length(seeds) - number_of_na*n_assets^2)
  
  results = list(
    "TF_sum" = TF_sum,
    "std_norm" = std_norm_list,
    "bias" = bias,
    "MAE" = MAE,
    "RMSE" = RMSE,
    "coverage_prob" = list(
      "coverage_prob99" = cvg_prob99,
      "coverage_prob95" = cvg_prob95,
      "coverage_prob90" = cvg_prob90,
      "coverage_prob80" = cvg_prob80,
      "coverage_prob70" = cvg_prob70
    ),
    "MRC" = MRC_list,
    "QV" = QV_list
  )
  
  fs::dir_create("results")
  file_path = paste0(
    "results/MRC_theta_", theta, 
    "_gamma_", gamma^2, 
    "_lambda_", paste(lambda, sep = "", collapse = "")
  ) %>% 
    stringr::str_replace_all(pattern = "\\.", replacement = "")
  saveRDS(results, file = file_path)
  
  tictoc::toc()
  
  return(results)
}
