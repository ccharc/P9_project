library(dplyr)
library(Rsolnp)

results = readRDS("results/MRC_theta_02_gamma_0_lambda_246810")

v_vec = purrr::map_dfc(.x = results$MRC[1:3], .f = diag) %>% as.matrix()

# Simulated returns
ret = rnorm(15, mean = 0, sd = 0.1)
ret = matrix(ret, ncol = 3)




h_t = function(theta1, v_vec){
  omega = theta1[1:5]
  A =  theta1[6:10]
  B = theta1[11:15]
  h = matrix(nrow = 5, ncol = ncol(v_vec))
  h0 = v_vec[,1]
  h[,1] = h0
  for (i in 1:(ncol(v_vec)-1)) {
   h[,i+1] =  omega + A * v_vec[,i] + B * h[,i]
  }
  return(h)
}

h = h_t(v_vec, theta1 = c(omega, A, B))


# Defining the log-likelihood
llik_H = function(theta1, v_vec, ret){
  omega = theta1[1:5]
  A =  theta1[6:10]
  B = theta1[11:15]
  l = c()
  h = h_t(theta1, v_vec)
  for (i in 1:(ncol(v_vec)-1)) {
    l[i] =  2 * log( prod( h[, i+1]^(1/2)) ) + t(((h[,i+1]^(-1/2)) * ret[,i+1])) %*% ((h[,i+1]^(-1/2)) * ret[,i+1])
  }
  return(0.5 * sum(l))
}

llik_H(theta1 = theta, v_vec = v_vec, ret = ret)

# Maximizing the log-likelihood with initial value theta0
theta0 = rep(34, 15)
pars = solnp(pars = theta0, fun = llik_H, v_vec = v_vec, ret = ret)$pars


# Modelling P_bar and R_bar by their empirical counterparts P_bar = E[RL_t] and R_bar = E[R_t]
u_t = (h_t(theta1 = pars, v_vec = v_vec))^(-0.5) * ret

# R_bar
R_bar  = 0
for (i in 1:ncol(u_t)) {
  R_bar = R_bar + u_t[,i] %*% t(u_t[,i])
}
R_bar = R_bar/ncol(u_t)

# P_bar
RL_t = 0
for (i in 1:ncol(u_t)) {
  RL_t = RL_t + diag(v_vec[,i]^(-0.5)) %*% results1[[i]] %*% diag(v_vec[,i]^(-0.5))
}
P_bar = RL_t / ncol(u_t)









