library(furrr)
library(data.table)
library(tidyverse)
library(highfrequency)
library(tictoc)
source("functions/get_data_df.R")

future::plan(future::multisession(), workers = future::availableCores() - 2)
data = get_data_df()

log_data = purrr::map(.x = data, .f = function(x) {
  x %>% 
    mutate(Price = log(Price))
})

asset_names = names(data)
n_assets = length(asset_names)
theta = 0.9
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

date_seq = data$AUDNZD$DT %>% as.Date(tz = "CET") %>% unique()

tic()
MRC_series = furrr::future_map(
  .x = date_seq %>% setNames(date_seq),
  .f = get_MRC_by_date,
  data = log_data,
  n_assets = n_assets,
  .options = furrr::furrr_options(seed = TRUE),
  .progress = TRUE
)
toc()

compute_daily_returns = function(df, date_seq){
  ref = data.table("DT" = as.POSIXct(date_seq) - lubridate::hours(1))
  returns = setDT(as.data.table(df))[ref, Price, roll = "nearest", on = "DT"] %>% 
    diff()
}

daily_returns = purrr::map_dfc(
  .x = log_data, 
  .f = compute_daily_returns, 
  date_seq = date_seq
) %>% 
  # mutate(DT = as.POSIXct(date_seq[-1]), .before = 1) %>% 
  as.data.frame()


library(rugarch)
library(rmgarch)

uspec_n = multispec(replicate(n_assets, ugarchspec(
  mean.model = list(armaOrder = c(0,0)),
  variance.model = list(model = "sGARCH", garchOrder = c(1,1))
)))
multf = multifit(uspec_n, daily_returns)

spec1 = dccspec(uspec = uspec_n, dccOrder = c(1, 1), distribution = 'mvnorm')

fit1 = dccfit(
  spec1, 
  data = daily_returns, 
  fit.control = list(eval.se = TRUE), 
  fit = multf
)
fit2 = dccfit(
  spec1, 
  data = daily_returns, 
  fit.control = list(eval.se = TRUE), 
  fit = multf,
  realizedVol = stocks[1:128,]
)

pred1 = dccforecast(fit1, n.ahead = 1)
fitted(pred1)
sigma(pred1)
rcov(pred1)
rcor(pred1)



quantmod::getSymbols("IBM", from = as.Date("2022-01-01"), to = as.Date("2022-08-01"))
rIBM = quantmod::dailyReturn(IBM)
rGOOG = quantmod::dailyReturn(GOOG)

stocks = data.frame(xts(rIBM), xts(rGOOG))
test$daily.returns %>% class()


# get weights

mrc = MRC_series$`2018-06-29`
mrc_inv = solve(mrc)
weigth_matrix = (mrc_inv %*% rep(1, n_assets)) / 
  as.double(t(rep(1, n_assets)) %*% mrc_inv %*% rep(1, n_assets))
weigths = as.vector(weigth_matrix) %>% setNames(rownames(weigth_matrix))

quadprog::solve.QP(
  Dmat = mrc, 
  Amat = matrix(rep(1, n_assets*n_assets), ncol = 10, byrow = T), 
  bvec = rep(1, n_assets), 
  dvec = rep(0, n_assets),
  meq = 0
)

pred_returns = as.vector(fitted(pred1))
target_return = 0.1

sol = quadprog::solve.QP(
  Dmat = mrc, 
  Amat = matrix(c(pred_returns, rep(1, (n_assets-1)*n_assets)), ncol = 10, byrow = T), 
  bvec = c(target_return, rep(1, n_assets-1)), 
  dvec = rep(0, n_assets),
  meq = 0
)$solution

pred_returns %*% sol







