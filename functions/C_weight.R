# psi1_g1 = 1/3
# psi2_g1 = 1/30
# psi1_g2 = 2/15
# psi2_g2 = 1/105
# psi1_g3 = 2/105
# psi2_g3 = 1/630

psi1_g1 = (pi^2)/2
psi2_g1 = 1/2
psi1_g2 = 2*pi^2
psi2_g2 = 1/2
psi1_g3 = (9*pi^2)/2
psi2_g3 = 1/2


A = matrix(
  c(
    theta^2*psi2_g1^2, psi1_g1*psi2_g1, (psi1_g1^2)/(theta^2),
    theta^2*psi2_g2^2, psi1_g2*psi2_g2, (psi1_g2^2)/(theta^2),
    theta^2*psi2_g3^2, psi1_g3*psi2_g3, (psi1_g3^2)/(theta^2)
  ), 
  nrow = 3, 
  ncol = 3, 
  byrow = TRUE
)

A_inv = solve(A)

C_vec = c(
  (2*theta*(151/80640))/((1/12)^2), 
  (2*(1/96))/(theta*(1/12)^2), 
  (2*(1/6))/((1/12)^2*theta^3)
) # Skal disse størrelser mon regnes ud fra g eller g1, g2 & g3?

C_weight = C_vec %*% A_inv





#psi1_g1 = 2/15
#psi2_g1 = 1/105
#psi1_g2 = 2/15
#psi2_g2 = 1/105
#psi1_g3 = 2/105
#psi2_g3 = 1/630