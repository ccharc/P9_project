HY_sim_study = function(seeds, lambda){
  tictoc::tic()
  
  sim_list = furrr::future_map2(
    .x = seeds,
    .y = lambda,
    .f = get_HY_est_int_volatility,
    .options = furrr::furrr_options(seed = TRUE),
    .progress = TRUE
  )
  
  bias = purrr::map_dbl(sim_list, .f = "bias")
  MAE = purrr::map_dbl(sim_list, .f = "MAE")
  RMSE = purrr::map_dbl(sim_list, .f = "RMSE")
  # HY_list = purrr::map(sim_list, .f = "HY")
  # QV_list = purrr::map(sim_list, .f = "QV")
  
  results = list(
    "bias" = bias,
    "MAE" = MAE,
    "RMSE" = RMSE
  )
  
  fs::dir_create("results")
  file_path = paste0(
    "results/HY_theta_", theta, 
    "_gamma_", gamma, 
    "_lambda_", paste(lambda, sep = "", collapse = "")
  ) %>% 
    stringr::str_replace_all(pattern = "\\.", replacement = "")
  saveRDS(results, file = file_path)
  
  tictoc::toc()
  
  return(results)
}