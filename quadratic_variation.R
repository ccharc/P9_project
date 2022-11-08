# extracting sigma 
rho = -0.3
set.seed(101)

# W needs to be specified outside simulate_price
W_increments = sqrt(t_max/n)*rnorm(n+1,0,1)
W = c(0,cumsum(W_increments))

# creating price series
sigma_list = purrr::map(.x = rep(n, 5), .f = function(x,W) simulate_price(x,W)[2], W = W)

quad_11 = (1-rho^2) * sum(1/n * unlist(sigma_list[1])^2) + rho^2 * sum(1/n * unlist(sigma_list[1])^2)
quad_12 = 0.3^2 * sum(1/3600 * unlist(sigma_list[1]) * unlist(sigma_list[2]))
quad_22 = (1-rho^2) * sum(1/n * unlist(sigma_list[2])^2) + rho^2 * sum(1/n * unlist(sigma_list[2])^2)



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
X_step = stepfun(x = (1:n)/n, y = X, f = 0, right = 0)
X_async = X_step(wait_times1)

plot.ts(X_async)







Y = c()
Y[1] = X0
delta = t_max / n
for (i in 1:n) {
  Y[i+1] = Y[i] + b * delta + 
    s1[i] * (BM[i+1] - BM[i]) +
    s2[i] * (W[i+1] - W[i])
}

plot(X, type = "l")

sum(diff(X)^2)

