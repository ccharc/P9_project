library(dplyr)
library(Rsolnp)
library(purrr)
library(tictoc)
source("functions/dcc_heavy_functions.R")


# test data ---------------------------------------------------------------

set.seed(123)

# MRC matrices (the realized measure used)
results = readRDS("results/MRC_theta_02_gamma_0_lambda_246810")
results1 = results$MRC[1:1000]

# Simulated returns
returns = rnorm(5*length(results1), mean = 0, sd = 0.1)
returns = matrix(returns, ncol = length(results1))

# test run
get_theta_H(MRC_list = results1, return_matrix = returns)


# -------------------------------------------------------------------------



# compute the conditional covariance matrix H_t ---------------------------

get_theta_H(MRC_list = MRC_series, return_matrix = t(daily_returns)) # estimated coefficients

H_t = get_H_t_all(MRC_list = MRC_series, return_matrix = t(daily_returns)) # conditional covariance matrices


# -------------------------------------------------------------------------








# define returns and realized measure -------------------------------------

# MRC matrices (the realized measure used)
results = readRDS("results/MRC_theta_02_gamma_0_lambda_246810")
results1 = results$MRC[1:10]

v_vec = purrr::map_dfc(.x = results$MRC[1:10], .f = diag) %>% as.matrix() # each column is diagonal elements of an MRC

# Simulated returns
ret = rnorm(5*ncol(v_vec), mean = 0, sd = 0.1)
ret = matrix(ret, ncol = ncol(v_vec))

# realized correlation matrix
get_real_corr = function(real_cov_list, t){
  diag(v_vec[,t]^(-0.5)) %*% results1[[t]] %*% diag(v_vec[,t]^(-0.5))
}

RL_t = map(
  .x = 1:length(results1),
  .f = get_real_corr,
  real_cov_list = results1)

# h_t (conditional variance) ----------------------------------------------

h_t = function(theta, v_vec){ # computing the conditional variance vector and collect into a matrix
  omega = theta[1:5]
  A =  theta[6:10]
  B = theta[11:15]
  h = matrix(nrow = 5, ncol = ncol(v_vec))
  h0 = v_vec[,1] # initial value MRC from day 1
  h[,1] = h0
  for (i in 1:(ncol(v_vec)-1)) {
   h[,i+1] =  omega + A * v_vec[,i] + B * h[,i]
  }
  return(h)
}

h = h_t(v_vec, theta = c(omega, A, B))


# log-likelihood for theta_H1 ---------------------------------------------

# likelihood
llik_H1 = function(theta, v_vec, ret){
  omega = theta[1:5]
  A =  theta[6:10]
  B = theta[11:15]
  l = c()
  h = h_t(theta, v_vec)
  for (i in 1:(ncol(v_vec)-1)) {
    l[i] =  2 * log( prod( h[, i+1]^(1/2)) ) + t(((h[,i+1]^(-1/2)) * ret[,i+1])) %*% ((h[,i+1]^(-1/2)) * ret[,i+1])
  }
  return(0.5 * sum(l))
}

llik_H1(theta = theta, v_vec = v_vec, ret = ret)

# Maximizing the log-likelihood with initial value theta0
theta0_H1 = rep(0.7, 15)

theta_H1 = solnp(
  pars = theta0_H1, 
  fun = llik_H1, 
  v_vec = v_vec, 
  ret = ret,
  LB = c(rep(0,length(theta0_H1))), # all unknowns are restricted to be positive
  UB = c(rep(Inf, 2*length(theta0_H1)/3), rep(1, length(theta0_H1)/3)))$pars # diagonal elements of B are smaller than the unity


# P bar and R bar ---------------------------------------------------------

# Modelling P_bar and R_bar by their empirical counterparts P_bar = E[RL_t] and R_bar = E[R_t]
# u_t = (h_t(theta1 = pars, v_vec = v_vec))^(-0.5) * ret
u_t = v_vec^(-0.5) * ret

# R_bar
R_bar  = 0
for (i in 1:ncol(u_t)) {
  R_bar = R_bar + results1[[i]] * v_vec[,i] %*% t(v_vec[,i])
}
# R_bar = R_bar/ncol(u_t)
R_bar = P_bar

# P_bar (see eq. (2) in Bauwens and Xu, 2022)
P_bar = 0
for (i in 1:ncol(u_t)) {
  P_bar =P_bar + diag(v_vec[,i]^(-0.5)) %*% results1[[i]] %*% diag(v_vec[,i]^(-0.5))
}
P_bar = P_bar / ncol(u_t)


# R_t (conditional correlation matrix) ------------------------------------

R_t = function(theta, real_corr_mat_list, R_bar, P_bar){ # computing the conditional variance vector and collect into a matrix
  alpha = theta[1]
  beta = theta[2]
  
  R = vector(mode = "list", length = length(real_corr_mat_list))
  R0 = RL_t[[1]] # initial value RL from day 1
  R[[1]] = R0
  
  R_tilde = (1 - beta) * R_bar - alpha * P_bar
  
  for (i in 1:(ncol(v_vec)-1)) {
    R[[i+1]] =  R_tilde + alpha * real_corr_mat_list[[i]] + beta * R[[i]]
  }
  return(R)
}

R_t(theta = c(alpha,beta), real_corr_mat_list = RL_t, R_bar = R_bar,P_bar = P_bar)


# log-likelihood for theta_H2 ---------------------------------------------

llik_H2 = function(theta, real_corr_mat_list, R_bar, P_bar, theta_H1, v_vec, ret){ 
  alpha = theta[1]
  beta = theta[2]
  
  l = c()
  R = R_t(c(alpha,beta), real_corr_mat_list, R_bar, P_bar)
  R_inv = map(.x = R, solve) 
  h = h_t(theta = theta_H1, v_vec = v_vec)
  
  for (i in 1:(length(real_corr_mat_list)-1)) {
    l[i] = log( det(R[[i+1]]) ) + 
      t(((h[,i+1]^(-1/2)) * ret[,i+1])) %*% R_inv[[i+1]] %*% ((h[,i+1]^(-1/2)) * ret[,i+1])
  }
  return(0.5 * sum(l))
}

llik_H2(theta = c(alpha,beta), real_corr_mat_list = RL_t, R_bar = R_bar, 
        P_bar = P_bar, theta_H1 = theta_H1, v_vec = v_vec, ret = ret)

# Maximizing the log-likelihood with initial value theta0_H2
theta0_H2 = rep(0.4, 2)

theta_H2 = solnp(
  pars = theta0_H2, 
  fun = llik_H2,
  real_corr_mat_list = RL_t,
  R_bar = R_bar,
  P_bar = P_bar, 
  theta_H1 = theta_H1,
  v_vec = v_vec, 
  ret = ret,
  LB = c(rep(0,length(theta0_H2))), # all unknowns are restricted to be positive
  UB = c(Inf, 1))$pars


# define theta_H ----------------------------------------------------------

theta_H = c(theta_H1, theta_H2)

