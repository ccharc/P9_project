# configs 
X0 = 1
b = 0.03
beta0 = -5/16
beta1 = 1/8
alpha = -1/40
rho = -0.3
theta = 0.8
t_max = 1
n = 10001                       # 8 hours in seconds
gamma = sqrt(0.01)              # levels of volatility: 0, 0.001, 0.01
#lambda1 = c(3, 5, 10, 30, 60)   # avg. waiting times until new price
lambda1 = c(1, 1, 1, 1, 1)
lambda2 = lambda1*2
n_assets = 5