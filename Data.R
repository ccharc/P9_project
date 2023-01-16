library(furrr)
library(data.table)
library(tidyverse)
library(highfrequency)
library(Rsolnp)
library(tictoc)
R.utils::sourceDirectory("functions", modifiedOnly = FALSE)

future::plan(future::multisession(), workers = 35)
data = get_data_df()

log_data = purrr::map(.x = data, .f = function(x) {
  x %>%
    mutate(Price = log(Price))
})

asset_names = names(log_data)
n_assets = length(asset_names)
theta = 0.9

# estimate covariance of error

estimate_error_covariance(log_data)

# MRC of data

date_seq = log_data$EURAUD$DT %>% as.Date(tz = "CET") %>% unique()

# Check number of trades per day

trades_per_day = purrr::map_dfc(.x = log_data, .f = function(x) {
  furrr::future_map_dbl(.x = date_seq, .f = function(date) {
    x %>% 
      filter(as.Date(DT, tz = "CET") == date) %>% 
      nrow()
  },
  .progress = TRUE)
}) %>% 
  mutate(Date = date_seq, .before = 1)

rm_dates = c(256, 259, 309, 315, 321, 327)

tic()
MRC_series = furrr::future_map(
  .x = date_seq[-rm_dates] %>% setNames(date_seq[-rm_dates]),
  .f = get_MRC_by_date,
  data = log_data,
  n_assets = n_assets,
  .options = furrr::furrr_options(seed = TRUE),
  .progress = TRUE
)
toc()

daily_returns = purrr::map_dfc(
  .x = log_data, 
  .f = compute_daily_returns, 
  date_seq = date_seq[-rm_dates]
) %>% 
  mutate(Date = date_seq[-c(1, rm_dates)], .before = 1) %>%
  as.data.frame()

saveRDS(object = MRC_series, file = "MRC_series_euro")
saveRDS(object = daily_returns, file = "daily_returns_euro")

# forecasting 2019 based on 2018 ------------------------------------------

MRC_series = readRDS("MRC_series_euro")
daily_returns = readRDS("daily_returns_euro")

which("2019-01-02" == names(MRC_series))
which("2019-03-29" == names(MRC_series))

tic()
H_predictions = furrr::future_map(
  .x = 259:321 %>% setNames(names(MRC_series)[259:321]), 
  .f = function(x) {
    get_H_t_all(
      MRC_list = MRC_series[1:x], 
      return_matrix = t(daily_returns[1:(x-1), -1])
    )
  },
  .progress = TRUE
)
toc()

weights = find_weights(
  n_assets = 10,
  ccov = H_predictions[[1]],
  cmean = matrix(as.double(daily_returns[257, -1])),
  target_return = 0.05
)

weights_list = furrr::future_map(
  .x = 1:63 %>% setNames(names(H_predictions)), 
  .f = function(x) {
    find_weights(
      n_assets = 10,
      ccov = H_predictions[[x]],
      cmean = matrix(as.double(daily_returns[256 + x, -1])), 
      target_return = 0.05
    )
  }, .progress = TRUE
)

portf_returns = purrr::map_dbl(
  .x = names(H_predictions), 
  .f = compute_portf_return
)

cov_matrix_portf_list = purrr::map(
  .x = names(H_predictions) %>% setNames(names(H_predictions)), 
  .f = function(x) {
    daily_returns %>% 
      filter(Date <= x) %>% 
      select(-Date) %>% 
      cov()
  }
)

portf_var = purrr::map2_dbl(
  .x = weights_list, 
  .y = cov_matrix_portf_list, 
  .f = function(w, C) {
    as.double(t(w[1:10]) %*% C %*% w[1:10])
  }
)

estim_portf_var = purrr::map2_dbl(
  .x = weights_list, 
  .y = H_predictions, 
  .f = function(w, C) {
    as.double(t(w[1:10]) %*% C %*% w[1:10])
  }
)

equal_portf_var = purrr::map_dbl(
  .x = H_predictions, 
  .f = function(C) {
    w = rep(0.1, 10)
    as.double(t(w) %*% C %*% w)
  }
)

gg1 = ggplot(mapping = aes(
  x = as.Date(names(H_predictions), tz = "CET"), 
  y = portf_returns
)) +
  geom_line() +
  labs(x = "Date", y = "Euro", title = "Portfolio PnL") +
  theme_bw()

gg2 = ggplot(mapping = aes(
  x = as.Date(names(H_predictions), tz = "CET"), 
  y = cumsum(portf_returns)
)) +
  geom_line() +
  labs(x = "Date", y = "Euro", title = "Portfolio Accumulated PnL") +
  theme_bw()

gg3 = ggplot() +
  geom_line(mapping = aes(
    x = as.Date(names(H_predictions), tz = "CET"), 
    y = portf_var,
    col = "Actual"
  )) +
  geom_line(mapping = aes(
    x = as.Date(names(H_predictions), tz = "CET"), 
    y = estim_portf_var,
    col = "Prediction"
  )) +
  labs(x = "Date", y = "Variance", title = "Portfolio variance", col = "") +
  theme_bw() +
  theme(legend.position = c(0.1, 0.9), legend.background = element_rect(fill = "transparent"))

gg4 = ggplot() +
  geom_line(mapping = aes(
    x = as.Date(names(H_predictions), tz = "CET"), 
    y = portf_var,
    col = "Actual"
  )) +
  geom_line(mapping = aes(
    x = as.Date(names(H_predictions), tz = "CET"), 
    y = estim_portf_var,
    col = "Prediction"
  )) +
  labs(x = "Date", y = "Variance", title = "Portfolio variance", col = "") +
  coord_cartesian(ylim=c(0, 0.015)) +
  theme_bw() +
  theme(legend.position = c(0.1, 0.9), legend.background = element_rect(fill = "transparent")) 

sharpe_ratio = portf_returns/sqrt(portf_var)

ggsave(
  plot = gg1, 
  filename = "plot_portf_pl.pdf", 
  width = 14, height = 10, units = "cm" 
)
ggsave(
  plot = gg2, 
  filename = paste0("plot_portf_acc_pl.pdf"), 
  width = 14, height = 10, units = "cm" 
)
ggsave(
  plot = gg3, 
  filename = paste0("plot_portf_var.pdf"), 
  width = 14, height = 10, units = "cm" 
)
ggsave(
  plot = gg4, 
  filename = paste0("plot_portf_var_zoom.pdf"), 
  width = 14, height = 10, units = "cm"
)








# library(rugarch)
# library(rmgarch)
# 
# uspec_n = multispec(replicate(n_assets, ugarchspec(
#   mean.model = list(armaOrder = c(0,0)),
#   variance.model = list(model = "sGARCH", garchOrder = c(1,1))
# )))
# multf = multifit(uspec_n, daily_returns)
# 
# spec1 = dccspec(uspec = uspec_n, dccOrder = c(1, 1), distribution = 'mvnorm')
# 
# fit1 = dccfit(
#   spec1, 
#   data = daily_returns, 
#   fit.control = list(eval.se = TRUE), 
#   fit = multf
# )
# fit2 = dccfit(
#   spec1, 
#   data = daily_returns, 
#   fit.control = list(eval.se = TRUE), 
#   fit = multf,
#   realizedVol = stocks[1:128,]
# )
# 
# pred1 = dccforecast(fit1, n.ahead = 1)
# fitted(pred1)
# sigma(pred1)
# rcov(pred1)
# rcor(pred1)
# 
# 
# 
# quantmod::getSymbols("IBM", from = as.Date("2022-01-01"), to = as.Date("2022-08-01"))
# rIBM = quantmod::dailyReturn(IBM)
# rGOOG = quantmod::dailyReturn(GOOG)
# 
# stocks = data.frame(xts(rIBM), xts(rGOOG))
# test$daily.returns %>% class()
# 
# 
# # get weights
# 
# mrc = MRC_series$`2018-06-29`
# mrc_inv = solve(mrc)
# weigth_matrix = (mrc_inv %*% rep(1, n_assets)) / 
#   as.double(t(rep(1, n_assets)) %*% mrc_inv %*% rep(1, n_assets))
# weigths = as.vector(weigth_matrix) %>% setNames(rownames(weigth_matrix))
# 
# quadprog::solve.QP(
#   Dmat = mrc, 
#   Amat = matrix(rep(1, n_assets*n_assets), ncol = 10, byrow = T), 
#   bvec = rep(1, n_assets), 
#   dvec = rep(0, n_assets),
#   meq = 0
# )
# 
# pred_returns = as.vector(fitted(pred1))
# target_return = 0.1
# 
# sol = quadprog::solve.QP(
#   Dmat = mrc, 
#   Amat = matrix(c(pred_returns, rep(1, (n_assets-1)*n_assets)), ncol = 10, byrow = T), 
#   bvec = c(target_return, rep(1, n_assets-1)), 
#   dvec = rep(0, n_assets),
#   meq = 0
# )$solution
# 
# pred_returns %*% sol
# 
# sol = quadprog::solve.QP(
#   Dmat = mrc, 
#   Amat = matrix(c(rep(1, n_assets), rep(pred_returns, (n_assets-1))), ncol = 10, byrow = T), 
#   bvec = c(1, rep(target_return, n_assets-1)), 
#   dvec = rep(0, n_assets),
#   meq = 0
# )$solution
# 
# pred_returns %*% sol
# 
# sol = quadprog::solve.QP(
#   Dmat = mrc, 
#   Amat = matrix(rep(pred_returns, (n_assets)), ncol = 10, byrow = T), 
#   bvec = rep(target_return, n_assets), 
#   dvec = rep(0, n_assets),
#   meq = 0
# )$solution
# 
# pred_returns %*% sol



