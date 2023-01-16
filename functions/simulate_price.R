simulate_price = function(n, lambda, W)
{
  # Brownian motions
  BM_increments = sqrt(t_max/n)*rnorm(n+1,0,1)
  BM = c(0,cumsum(BM_increments))

  # OU specification for varrho
  # varrho = c()
  # varrho0 = rnorm(n = 1, mean = 0, sd = sqrt((-2*alpha)^(-1)))
  # varrho[1] = varrho0
  # for (i in 1:n) {
  #   varrho[i+1] = varrho[i] + alpha * varrho[i] + (BM[i+1] - BM[i])
  # }
  
  # OU specification for varrho solution
  varrho = c()
  varrho0 = rnorm(n = 1, mean = 0, sd = sqrt((-2*alpha)^(-1)))
  varrho[1] = varrho0
  for (i in 1:n) {
    varrho[i+1] = exp(alpha * (i/n)) * (varrho0 + sum(exp(-alpha * ((1:i)/n) ) * (BM[2:(i+1)] - BM[1:i])))
  }
  
  # sigma spot volatility
  sigma = exp(beta0 + beta1 * varrho)

  # Diffusion components
  s1 = rho * sigma
  s2 = sqrt(1 - rho^2) * sigma

  # Simulate efficient log prices X (using Euler scheme)
  X = c()
  X[1] = X0
  delta = t_max / n
  for (i in 1:n) {
    X[i+1] = X[i] + b * delta +
      s1[i] * (BM[i+1] - BM[i]) +
      s2[i] * (W[i+1] - W[i])
  }

  # Noisy log prices Y
  omega = sqrt(gamma^2 * sqrt((1/n) * sum(sigma^4)))
  micro_noise = rnorm(n = length(X), mean = 0, sd = omega^2)
  Y = X + micro_noise

  # Irregular and non-synchronous data using a poisson process
  get_poisson = function(lambda, n)
  {
    s = c()
    s[1] = 0
    while (sum(s) < 1) {
      iid_exp_var = rexp(50, 1/lambda)
      s = c(s, iid_exp_var/n)
    }
    cumsum(s)[cumsum(s) <= 1]
  }

  wait_times = get_poisson(lambda, n)
  Y_step = stepfun(x = (1:n)/n, y = Y, f = 0, right = 0)
  Y_async = Y_step(wait_times)

  # tibble::tibble("Time" = wait_times, "Price" = Y_async)
  datetime = as.POSIXct("2022-10-31 01:00:00", tz = "EST") + lubridate::seconds(wait_times*n)
  price = data.table("DT" = datetime, "Price" = exp(Y_async))
  return(list("price" = price, "sigma" = sigma))
}



# df = tibble::tibble("Time" = wait_times1, "Price" = Y_async)
# 
# # Plot
# gg = ggplot() +
#   geom_line(aes(x = (0:n)/n, y = exp(X), color = "Efficient")) +
#   geom_line(aes(x = (0:n)/n, y = exp(Y), color = "Noisy"), linetype = "dashed") +
#   geom_line(aes(x = wait_times1, y = exp(Y_async), color = "Non-synchronous")) +
#   labs(
#     title = "Simulation of prices", 
#     subtitle = paste0("Number of observations: ", n),
#     x = "Time", y = "Price", color = ""
#   ) +
#   scale_color_manual(values = c(
#     "Efficient" = "tomato", 
#     "Noisy" = "steelblue", 
#     "Non-synchronous" = "black"
#   )) +
#   guides(color = guide_legend(
#     override.aes = list(linetype = c("solid", "dashed", "solid"))
#   ))
# 
# print(gg)

