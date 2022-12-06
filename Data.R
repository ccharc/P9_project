# library(furrr)
library(data.table)
library(tidyverse)
library(highfrequency)
source("functions/get_data_df.R")

future::plan(future::multisession(), workers = future::availableCores() - 2)
tictoc::tic()
data = get_data_df()
tictoc::toc()

asset_names = names(data)
n_assets = length(asset_names)
theta = 0.8
gamma = 0.001
R.utils::sourceDirectory("functions", modifiedOnly = FALSE)

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

date_seq = data$AUDNZD$DT %>% as.Date() %>% unique()

MRC_series = furrr::future_map(
  .x = date_seq %>% setNames(date_seq),
  .f = get_MRC_by_date,
  data = data,
  n_assets = n_assets,
  .options = furrr::furrr_options(seed = TRUE)
)

