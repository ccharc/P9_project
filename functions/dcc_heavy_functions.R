
# compute H_t -------------------------------------------------------------

get_H_t_all = function(MRC_list, return_matrix){
  
  tic()
  
  theta_H = get_theta_H(MRC_list = MRC_list, return_matrix = return_matrix)
  
  v_vec = purrr::map_dfc(.x = MRC_list, .f = diag) %>% 
    as.matrix() # each column is diagonal elements of an MRC
  
  RL_t = map(
    .x = 1:length(MRC_list),
    .f = get_real_corr,
    real_cov_list = MRC_list,
    v_vec = v_vec)
  
  P_bar = get_P_bar(real_cov_list = MRC_list, v_vec = v_vec)
  
  u_t = t(return_matrix) * diag(cov(t(return_matrix)))
  R_bar = cor(u_t)
  
  h = h_t(theta = theta_H[1:(3*nrow(return_matrix))], v_vec = v_vec, return_matrix = return_matrix)
  R = R_t(theta = theta_H[(3*nrow(return_matrix)+1):(3*nrow(return_matrix)+2)], real_corr_mat_list = RL_t, R_bar, P_bar)
  
  # H = map(
  #   .x = 1:length(MRC_list),
  #   .f = get_H_t, 
  #   cond_var_vec = h, 
  #   cond_corr_mat = R)
  
  H_end = get_H_t(t = length(MRC_list), cond_var_vec = h, cond_corr_mat = R)
  
  toc()
  
  return(H_end)
  
}


# compute estimate of theta_H ---------------------------------------------

get_theta_H = function(MRC_list, return_matrix){ 
  
  tic()
  
  v_vec = purrr::map_dfc(.x = MRC_list, .f = diag) %>% 
    as.matrix() # each column is diagonal elements of an MRC
  
  RL_t = map(
    .x = 1:length(MRC_list),
    .f = get_real_corr,
    real_cov_list = MRC_list,
    v_vec = v_vec)
  
  # Maximizing the log-likelihood with initial value theta0
  theta0_H1 = rep(0.5, 3*nrow(return_matrix))
  
  theta_H1 = solnp(
    pars = theta0_H1, 
    fun = llik_H1, 
    v_vec = v_vec, 
    return_matrix = return_matrix,
    LB = c(rep(0,length(theta0_H1))), # all unknowns are restricted to be positive
    UB = c(rep(Inf, 2*length(theta0_H1)/3), rep(1, length(theta0_H1)/3)))$pars # diagonal elements of B are smaller than the unity
  
  P_bar = get_P_bar(real_cov_list = MRC_list, v_vec = v_vec)
  R_bar = P_bar
  
  # Maximizing the log-likelihood with initial value theta0_H2
  theta0_H2 = rep(0.5, 2)
  
  theta_H2 = solnp(
    pars = theta0_H2, 
    fun = llik_H2,
    real_corr_mat_list = RL_t,
    R_bar = R_bar,
    P_bar = P_bar, 
    theta_H1 = theta_H1,
    v_vec = v_vec, 
    return_matrix = return_matrix,
    LB = c(rep(0,length(theta0_H2))), # all unknowns are restricted to be positive
    UB = c(Inf, 1)
  )$pars
  
  toc()
  
  return("theta_H" = c(theta_H1, theta_H2))
  
}


# -------------------------------------------------------------------------


# realized correlation matrix
get_real_corr = function(real_cov_list, v_vec, t){ 
  diag(v_vec[,t]^(-0.5)) %*% real_cov_list[[t]] %*% diag(v_vec[,t]^(-0.5))
}

# h_t (conditional variance) 
h_t = function(theta, v_vec, return_matrix){ # computing the conditional variance vector and collect into a matrix
  
  omega = theta[1:nrow(return_matrix)]
  A =  theta[(nrow(return_matrix)+1):(2*nrow(return_matrix))]
  B = theta[(2*nrow(return_matrix) + 1):(3*nrow(return_matrix))]
  
  h = matrix(nrow = nrow(return_matrix), ncol = ncol(v_vec))
  h0 = v_vec[,1] # initial value MRC from day 1
  h[,1] = h0
  
  for (i in 1:(ncol(v_vec)-1)) {
    h[,i+1] =  omega + A * v_vec[,i] + B * h[,i]
  }
  return(h)
}

# log-likelihood for theta_H1 
llik_H1 = function(theta, v_vec, return_matrix){ 
  
  omega = theta[1:nrow(return_matrix)]
  A =  theta[(nrow(return_matrix)+1):(2*nrow(return_matrix))]
  B = theta[(2*nrow(return_matrix) + 1):(3*nrow(return_matrix))]
  
  l = c()
  h = h_t(theta = theta, v_vec = v_vec, return_matrix = return_matrix)
  
  for (i in 1:(ncol(v_vec)-1)) {
    l[i] =  2 * log( prod( h[, i+1]^(1/2)) ) + t(((h[,i+1]^(-1/2)) * return_matrix[,i])) %*% ((h[,i+1]^(-1/2)) * return_matrix[,i])
  }
  return(0.5 * sum(l))
}

# Modelling P_bar and R_bar by their empirical counterparts P_bar = E[RL_t] and R_bar = E[R_t]
get_P_bar = function(real_cov_list, v_vec){ 
  p = 0
  for (i in 1:length(real_cov_list)) {
    p = p + diag(v_vec[,i]^(-0.5)) %*% real_cov_list[[i]] %*% diag(v_vec[,i]^(-0.5))
  }
  return(p / length(real_cov_list))
}

# R_t (conditional correlation matrix) 
R_t = function(theta, real_corr_mat_list, R_bar, P_bar){ # computing the conditional variance vector and collect into a matrix
  
  alpha = theta[1]
  beta = theta[2]
  
  R = vector(mode = "list", length = length(real_corr_mat_list))
  R0 = real_corr_mat_list[[1]] # initial value RL from day 1
  R[[1]] = R0
  
  R_tilde = (1 - beta) * R_bar - alpha * P_bar
  
  for (i in 1:(length(real_corr_mat_list)-1)) {
    R[[i+1]] =  R_tilde + alpha * real_corr_mat_list[[i]] + beta * R[[i]]
  }
  return(R)
}

# log-likelihood for theta_H2
llik_H2 = function(theta, real_corr_mat_list, R_bar, P_bar, theta_H1, v_vec, return_matrix){ 
  alpha = theta[1]
  beta = theta[2]
  
  l = c()
  R = R_t(c(alpha,beta), real_corr_mat_list, R_bar, P_bar)
  R_inv = map(.x = R, solve) 
  h = h_t(theta = theta_H1, v_vec = v_vec, return_matrix = return_matrix)
  
  for (i in 1:(length(real_corr_mat_list)-1)) {
    l[i] = log( det(R[[i+1]]) ) + 
      t(((h[,i+1]^(-1/2)) * return_matrix[,i])) %*% R_inv[[i+1]] %*% ((h[,i+1]^(-1/2)) * return_matrix[,i])
  }
  
  
  
  return(0.5 * sum(l))
}

# conditional covariance matrix at one day
get_H_t = function(cond_var_vec, cond_corr_mat, t){ 
  diag(cond_var_vec[,t]^(1/2)) %*% cond_corr_mat[[t]] %*% diag(cond_var_vec[,t]^(1/2))
}











# # compute estimate of theta_H ---------------------------------------------
# 
# get_theta_H = function(MRC_list, return_matrix){ 
#   
#   tic()
#   
#   v_vec = purrr::map_dfc(.x = MRC_list, .f = diag) %>% as.matrix() # each column is diagonal elements of an MRC
#   
#   RL_t = map(
#     .x = 1:length(MRC_list),
#     .f = get_real_corr,
#     real_cov_list = MRC_list,
#     v_vec = v_vec)
#   
#   # Maximizing the log-likelihood with initial value theta0
#   theta0_H1 = rep(0.5, ncol(return_matrix)*3)
#   
#   theta_H1 = solnp(
#     pars = theta0_H1, 
#     fun = llik_H1, 
#     v_vec = v_vec, 
#     return_matrix = return_matrix,
#     LB = c(rep(0,length(theta0_H1))), # all unknowns are restricted to be positive
#     UB = c(rep(Inf, 2*length(theta0_H1)/3), rep(1, length(theta0_H1)/3)))$pars # diagonal elements of B are smaller than the unity
#   
#   P_bar = get_P_bar(real_cov_list = MRC_list, v_vec = v_vec)
#   # R_bar = get_R_bar(real_cov_list = MRC_list, v_vec = v_vec)
#   R_bar = P_bar
#   
#   # Maximizing the log-likelihood with initial value theta0_H2
#   theta0_H2 = rep(0.5, 2)
#   
#   theta_H2 = solnp(
#     pars = theta0_H2, 
#     fun = llik_H2,
#     real_corr_mat_list = RL_t,
#     R_bar = R_bar,
#     P_bar = P_bar, 
#     theta_H1 = theta_H1,
#     v_vec = v_vec, 
#     return_matrix = return_matrix,
#     LB = c(rep(0,length(theta0_H2))), # all unknowns are restricted to be positive
#     UB = c(Inf, 1))$pars
#   
#   toc()
#   
#   return("theta_H" = c(theta_H1, theta_H2))
#   
# }
# 
# 
# # -------------------------------------------------------------------------
# 
# 
# # realized correlation matrix
# get_real_corr = function(real_cov_list, v_vec, t){
#   diag(v_vec[,t]^(-0.5)) %*% real_cov_list[[t]] %*% diag(v_vec[,t]^(-0.5))
# }
# 
# # h_t (conditional variance) 
# h_t = function(theta, v_vec){ # computing the conditional variance vector and collect into a matrix
#   
#   omega = theta[1:ncol(return_matrix)]
#   A =  theta[(ncol(return_matrix)+1):(2*ncol(return_matrix))]
#   B = theta[(2*ncol(return_matrix) + 1):(3*ncol(return_matrix))]
#   
#   h = matrix(nrow = ncol(return_matrix), ncol = ncol(v_vec))
#   h0 = v_vec[,1] # initial value MRC from day 1
#   h[,1] = h0
#   
#   for (i in 1:(ncol(v_vec)-1)) {
#     h[,i+1] =  omega + A * v_vec[,i] + B * h[,i]
#   }
#   return(h)
# }
# 
# # log-likelihood for theta_H1 
# llik_H1 = function(theta, v_vec, return_matrix){
#   
#   omega = theta[1:ncol(return_matrix)]
#   A =  theta[(ncol(return_matrix)+1):(2*ncol(return_matrix))]
#   B = theta[(2*ncol(return_matrix) + 1):(3*ncol(return_matrix))]
#   
#   l = c()
#   h = h_t(theta, v_vec)
#   
#   for (i in 1:(ncol(v_vec)-1)) {
#     l[i] =  2 * log( prod( h[, i+1]^(1/2)) ) + t(((h[,i+1]^(-1/2)) * return_matrix[,i+1])) %*% ((h[,i+1]^(-1/2)) * return_matrix[,i+1])
#   }
#   return(0.5 * sum(l))
# }
# 
# # Modelling P_bar and R_bar by their empirical counterparts P_bar = E[RL_t] and R_bar = E[R_t]
# get_P_bar = function(real_cov_list, v_vec){
#   p = 0
#   for (i in 1:length(real_cov_list)) {
#     p = p + diag(v_vec[,i]^(-0.5)) %*% real_cov_list[[i]] %*% diag(v_vec[,i]^(-0.5))
#   }
#   return(p / length(real_cov_list))
# }
# 
# get_R_bar = function(real_cov_list, v_vec){ # THIS IS COMPLETELY WRONG!!! 
#   r  = 0
#   for (i in 1:length(real_cov_list)) {
#     r = r + real_cov_list[[i]] * v_vec[,i] %*% t(v_vec[,i])
#   }
#   # R_bar = R_bar/ncol(u_t)
#   return(get_P_bar(v_vec, real_cov_list))
# }
# 
# # R_t (conditional correlation matrix) 
# R_t = function(theta, real_corr_mat_list, R_bar, P_bar){ # computing the conditional variance vector and collect into a matrix
#   alpha = theta[1]
#   beta = theta[2]
#   
#   R = vector(mode = "list", length = length(real_corr_mat_list))
#   R0 = real_corr_mat_list[[1]] # initial value RL from day 1
#   R[[1]] = R0
#   
#   R_tilde = (1 - beta) * R_bar - alpha * P_bar
#   
#   for (i in 1:(length(real_corr_mat_list)-1)) {
#     R[[i+1]] =  R_tilde + alpha * real_corr_mat_list[[i]] + beta * R[[i]]
#   }
#   return(R)
# }
# 
# # log-likelihood for theta_H2 ---------------------------------------------
# llik_H2 = function(theta, real_corr_mat_list, R_bar, P_bar, theta_H1, v_vec, return_matrix){ 
#   alpha = theta[1]
#   beta = theta[2]
#   
#   l = c()
#   R = R_t(c(alpha,beta), real_corr_mat_list, R_bar, P_bar)
#   R_inv = map(.x = R, solve) 
#   h = h_t(theta = theta_H1, v_vec = v_vec)
#   
#   for (i in 1:(length(real_corr_mat_list)-1)) {
#     l[i] = log( det(R[[i+1]]) ) + 
#       t(((h[,i+1]^(-1/2)) * return_matrix[,i+1])) %*% R_inv[[i+1]] %*% ((h[,i+1]^(-1/2)) * return_matrix[,i+1])
#   }
#   return(0.5 * sum(l))
# }