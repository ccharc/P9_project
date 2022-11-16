

make_IV = function(x, n_assets){
  IV = matrix(nrow = n_assets, ncol = n_assets)
  for (i in 1:n_assets) {
   for (j in 1:n_assets) {
     if (i == j){
       IV[i,j] = (1-rho^2) * sum(1/n * unlist(x[i])^2) + rho^2 * sum(1/n * unlist(x[i])^2)
     } 
     else {
       IV[i,j] = (1-rho^2) * sum(1/n * unlist(x[i]) * unlist(x[j]))
     }
    }
  }
  return(IV)
}
#quad_11 = (1-rho^2) * sum(1/n * unlist(sigma_list[1])^2) + rho^2 * sum(1/n * unlist(sigma_list[1])^2)
#quad_33 = (1-rho^2) * sum(1/n * unlist(sigma_list[3])^2) + rho^2 * sum(1/n * unlist(sigma_list[3])^2)
#quad_12 = (1-0.3^2) * sum(1/n * unlist(sigma_list[1]) * unlist(sigma_list[2]))
#quad_22 = (1-rho^2) * sum(1/n * unlist(sigma_list[2])^2) + rho^2 * sum(1/n * unlist(sigma_list[2])^2)