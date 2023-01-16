
find_weights = function(n_assets, ccov, cmean, target_return){ 
  # ccov and cmean are of class "matrix"
  
  zeros = matrix(rep(0,n_assets))
  ones = matrix(rep(1,n_assets))
  
  r1 = rbind(ccov, t(cmean), t(ones))
  r2 = rbind(-cmean,matrix(rep(0,2)))
  r3 = rbind(-ones,matrix(rep(0,2)))
  
  A = cbind(r1,r2,r3)
  b = c(zeros, target_return, 1)

  solve(A,b)
  
}
  
w = find_weights(
  n_assets = 5,
  ccov = results$MRC[[1]],
  cmean = matrix(c(0.004,0.001,-0.005,0,-0.04)), 
  target_return = 0.05
)

