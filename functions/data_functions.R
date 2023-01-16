get_data_df = function(){
  
  tictoc::tic()
  
  months = stringr::str_pad(1:12, width = 2, side = "left", pad = "0")
  
  asset_names = c(
    "EURAUD",
    # "EURCAD",
    # "EURCHF",
    "EURCZK",
    "EURGBP",
    "EURHUF",
    "EURJPY",
    "EURNOK",
    # "EURNZD",
    "EURPLN",
    "EURSEK",
    "EURTRY",
    "EURUSD"
  )
  
  get_asset_file_names = function(asset, months) {
    c(
      paste0("Data/DAT_NT_", asset, "_T_LAST_2018", months, ".csv"),
      paste0("Data/DAT_NT_", asset, "_T_LAST_2019", months[1:3], ".csv")
    )
  }
  
  assets = purrr::map(
    .x = setNames(asset_names, asset_names), 
    .f = get_asset_file_names, 
    months = months
  )
  
  data = furrr::future_map(
    .x = assets,
    .f = function(x) {
      purrr::map_dfr(.x = x, .f = read.csv, sep = ";", header = FALSE) %>% 
        dplyr::mutate(
          V1 = as.POSIXct(V1, format= "%Y%m%d %H%M%S") + lubridate::hours(7)
        ) %>% 
        dplyr::select(DT = V1, Price = V2) %>% 
        # dplyr::filter(as.Date(DT)==as.Date("2018-01-01")) %>% 
        as_tibble()
      # data.table()
    },
    .progress = TRUE
  )
  
  cat("\n")
  tictoc::toc()
  
  return(data)
}

estimate_error_covariance = function(data) {
  diff_prices = purrr::map(.x = data, .f = function(x) {
    x %>% 
      pull(Price) %>% 
      diff()
  })
  
  purrr::map_dbl(.x = diff_prices, .f = function(x) {
    (x %*% x)/(2*length(x))
  }) %>% mean() %>% sqrt()
}

compute_MRC_data = function(df_prices, n_assets)
{
  kn = floor(theta * nrow(df_prices)^(1/2))
  
  Y_bar = purrr::map_dfc(
    .x = asset_names %>% setNames(asset_names), 
    .f = function(x, kn, g) {df_prices %>% dplyr::pull(x) %>% preaverage(kn, g)},
    kn = kn, 
    g = gfunction
  ) %>%
    as.matrix() 
  
  Y_sum = t(Y_bar) %*% Y_bar    # equals the same as the for loop above
  
  psi1kn = kn * sum((gfunction((1:kn)/kn) - gfunction(((1:kn) - 1)/kn))^2)
  psi2kn = 1/kn * sum(gfunction((1:kn)/kn)^2)
  
  Y_sum * (nrow(df_prices)/(nrow(df_prices)-kn+2)) * (1/((1/12)*kn)) - psi1kn * 
    (1/(theta^2 * psi2kn)) * (1/(2*nrow(df_prices))) * 
    t(diff(as.matrix(df_prices))) %*% diff(as.matrix(df_prices))
}

get_MRC_by_date = function(date, data, n_assets) {
  purrr::map(.x = data, .f = function(x) {
    x %>% 
      filter(as.Date(DT) == date) %>% 
      as.data.table()
  }) %>% 
    refreshTime() %>% 
    select(-DT) %>% 
    compute_MRC_data(n_assets = n_assets)
}

compute_daily_returns = function(df, date_seq){
  ref = data.table("DT" = as.POSIXct(date_seq) - lubridate::hours(1))
  returns = setDT(as.data.table(df))[ref, Price, roll = "nearest", on = "DT"] %>% 
    diff()
}

find_weights = function(n_assets, ccov, cmean, target_return){ 
  # ccov and cmean are of class "matrix"
  
  zeros = matrix(rep(0,n_assets))
  ones = matrix(rep(1,n_assets))
  
  r1 = rbind(2*ccov, t(cmean), t(ones))
  r2 = rbind(cmean, matrix(rep(0,2)))
  r3 = rbind(ones,matrix(rep(0,2)))
  
  A = cbind(r1, r2, r3)
  b = c(zeros, target_return, 1)
  
  solve(A,b)
}

compute_portf_return = function(date){
  ret = daily_returns %>% 
    filter(Date == date) %>% 
    select(-Date) %>% 
    as.double()
  
  as.double(weights_list[[date]][1:10] %*% ret)
}