get_data_df = function(){
  
  tictoc::tic()
  
  months = stringr::str_pad(1:12, width = 2, side = "left", pad = "0")
  
  # selected assets
  assets = list(
    "AUDNZD" = c(
      paste0("Data/DAT_NT_AUDNZD_T_LAST_2018", months, ".csv"),
      paste0("Data/DAT_NT_AUDNZD_T_LAST_2019", months, ".csv")
    ),
    "EURNOK" = c(
      paste0("Data/DAT_NT_EURNOK_T_LAST_2018", months, ".csv"),
      paste0("Data/DAT_NT_EURNOK_T_LAST_2019", months, ".csv")
    ),
    "EURPLN" = c(
      paste0("Data/DAT_NT_EURPLN_T_LAST_2018", months, ".csv"),
      paste0("Data/DAT_NT_EURPLN_T_LAST_2019", months, ".csv")
    ),
    "EURUSD" = c(
      paste0("Data/DAT_NT_EURUSD_T_LAST_2018", months, ".csv"),
      paste0("Data/DAT_NT_EURUSD_T_LAST_2019", months, ".csv")
    ),
    "GBPJPY" = c(
      paste0("Data/DAT_NT_GBPJPY_T_LAST_2018", months, ".csv"),
      paste0("Data/DAT_NT_GBPJPY_T_LAST_2019", months, ".csv")
    ),
    "GBPUSD" = c(
      paste0("Data/DAT_NT_GBPUSD_T_LAST_2018", months, ".csv"),
      paste0("Data/DAT_NT_GBPUSD_T_LAST_2019", months, ".csv")
    ),
    "NZDCAD" = c(
      paste0("Data/DAT_NT_NZDCAD_T_LAST_2018", months, ".csv"),
      paste0("Data/DAT_NT_NZDCAD_T_LAST_2019", months, ".csv")
    ),
    "SGDJPY" = c(
      paste0("Data/DAT_NT_SGDJPY_T_LAST_2018", months, ".csv"),
      paste0("Data/DAT_NT_SGDJPY_T_LAST_2019", months, ".csv")
    ),
    "USDCHF" = c(
      paste0("Data/DAT_NT_USDCHF_T_LAST_2018", months, ".csv"),
      paste0("Data/DAT_NT_USDCHF_T_LAST_2019", months, ".csv")
    ),
    "ZARJPY" = c(
      paste0("Data/DAT_NT_ZARJPY_T_LAST_2018", months, ".csv"),
      paste0("Data/DAT_NT_ZARJPY_T_LAST_2019", months, ".csv")
    )
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