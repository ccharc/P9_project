get_data_df = function(){
  
  tictoc::tic()
  
  months = stringr::str_pad(1:12, width = 2, side = "left", pad = "0")
  
  asset_names = c(
    "AUDNZD",
    "EURNOK",
    "EURPLN",
    "EURUSD",
    "GBPJPY",
    "GBPUSD",
    "NZDCAD",
    "SGDJPY",
    "USDCHF",
    "ZARJPY"
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
  
  Y_sum * (nrow(df_prices)/(nrow(df_prices)-kn+2)) * (1/((1/12)*kn)) - psi1kn * (1/(theta^2 * psi2kn)) * 
    (1/(2*nrow(df_prices))) * t(diff(as.matrix(df_prices))) %*% diff(as.matrix(df_prices))
}