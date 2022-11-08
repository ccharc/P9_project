simulate_price = function(n, W)
{
  # Brownian motions
  BM_increments = sqrt(t_max/n)*rnorm(n+1,0,1)
  BM = c(0,cumsum(BM_increments))
  
  # OU specification for varrho
  varrho = c()
  varrho0 = rnorm(n = 1, mean = 0, sd = (-2*alpha)^(-1))
  varrho[1] = varrho0
  for (i in 1:n) {
    varrho[i+1] = alpha * varrho[i] + (BM[i+1] - BM[i])
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
  micro_noise = rnorm(n = length(X), mean = 0, sd = omega^2) # perhaps not squared
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
  
  wait_times1 = get_poisson(lambda1, n)
  wait_times2 = get_poisson(lambda2, n)
  Y_step = stepfun(x = (1:n)/n, y = Y, f = 0, right = 0)
  Y_async = Y_step(wait_times1)
  
  # tibble::tibble("Time" = wait_times1, "Price" = Y_async)
  datetime = as.POSIXct("2022-10-31 01:00:00", tz = "EST") + lubridate::seconds(wait_times1*n)
  price = data.table("DT" = datetime, "Price" = exp(Y_async))
  return(list(price, sigma))
}