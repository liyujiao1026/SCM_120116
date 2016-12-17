setwd("//dustaff/home/clg/Windesk/yli1216")
source("https://raw.githubusercontent.com/liyujiao1026/SCM_120116/master/global_update.R", echo = F)

tau = 0 ;omega = 0;
rho_x = 1;rho_y = 1;rho_xy = 1;
set <- c(0.01, seq(0.1,0.9,0.1),0.99) # 


# rho_x
Rho_x_sd <- sapply(set, Rep_SCM.Cmp, 
                   rep_times = 200,alpha = 2,
                   controlNumber = 30,timeLength = 30, invTime = 20,beta1 = 0.5, beta2 = 0.5,
                   
                   #omega = omega, rho_x = rho_x, rho_y = rho_y, rho_xy = rho_xy, tau = tau,
                   omega = omega,  rho_y = rho_y, rho_xy = rho_xy, tau = tau,
                   
                   control_rate = 1, pre_inv_rate = 1,
                   weight_equal = TRUE,weight_constant = TRUE 
)

Rho_x_sd_a1 <- unlist(lapply( Rho_x_sd[1,], sd))
Rho_x_sd_an <- unlist(lapply( Rho_x_sd[2,], sd))


prob.W_unbias_rhox <- sapply( 1:length(set),  function(x){
  equalW.hat <- apply(round(Rho_x_sd[3,][[x]][,-1],3), 1, function(i){sum(i == 0.033) == 30 })
  w.probability <- sum(equalW.hat)/length(equalW.hat)
  return(w.probability)
})

parData_rhox <- data.frame(set, "a1.sd" = Rho_x_sd_a1, "an.sd" = Rho_x_sd_an, factorName = "rho_x", "prob.W_unbias" = prob.W_unbias_rhox )
write.csv(parData_rhox, "rhox.csv")


#------------------------------------------------------------------#

# rho_xy
Rho_xy_sd <- sapply(set, Rep_SCM.Cmp, 
                   rep_times = 200,alpha = 2,
                   controlNumber = 30,timeLength = 30, invTime = 20,beta1 = 0.5, beta2 = 0.5,
                   
                   #omega = omega, rho_x = rho_x, rho_y = rho_y, rho_xy = rho_xy, tau = tau,
                   omega = omega, rho_x = rho_x, rho_y = rho_y,  tau = tau,
                   
                   control_rate = 1, pre_inv_rate = 1,
                   weight_equal = TRUE,weight_constant = TRUE 
)

Rho_xy_sd_a1 <- unlist(lapply( Rho_xy_sd[1,], sd))
Rho_xy_sd_an <- unlist(lapply( Rho_xy_sd[2,], sd))


prob.W_unbias_rhoxy <- sapply( 1:length(set),  function(x){
  equalW.hat <- apply(round(Rho_xy_sd[3,][[x]][,-1],3), 1, function(i){sum(i == 0.033) == 30 })
  w.probability <- sum(equalW.hat)/length(equalW.hat)
  return(w.probability)
})

parData_rhoxy <- data.frame(set, "a1.sd" = Rho_xy_sd_a1, "an.sd" = Rho_xy_sd_an, factorName = "rho_xy", "prob.W_unbias" = prob.W_unbias_rhoxy )
write.csv(parData_rhoxy, "rhoxy.csv")



#------------------------------------------------------------------#

# omega

omega_sd <- sapply(set, Rep_SCM.Cmp, 
                    rep_times = 200,alpha = 2,
                    controlNumber = 30,timeLength = 30, invTime = 20,beta1 = 0.5, beta2 = 0.5,
                    
                    #omega = omega, rho_x = rho_x, rho_y = rho_y, rho_xy = rho_xy, tau = tau,
                    rho_x = rho_x, rho_y = rho_y, rho_xy = rho_xy, tau = tau,
                    
                    control_rate = 1, pre_inv_rate = 1,
                    weight_equal = TRUE,weight_constant = TRUE 
)

omega_sd_a1 <- unlist(lapply( omega_sd[1,], sd))
omega_sd_an <- unlist(lapply( omega_sd[2,], sd))


prob.W_unbias_omega <- sapply( 1:length(set),  function(x){
  equalW.hat <- apply(round(omega_sd[3,][[x]][,-1],3), 1, function(i){sum(i == 0.033) == 30 })
  w.probability <- sum(equalW.hat)/length(equalW.hat)
  return(w.probability)
})

parData_omega <- data.frame(set, "a1.sd" = omega_sd_a1, "an.sd" = omega_sd_an, factorName = "omega", "prob.W_unbias" = prob.W_unbias_omega)
write.csv(parData_omega, "omega.csv")



#------------------------------------------------------------------#

#tau

tau_sd <- sapply(set, Rep_SCM.Cmp, 
                   rep_times = 200,alpha = 2,
                   controlNumber = 30,timeLength = 30, invTime = 20,beta1 = 0.5, beta2 = 0.5,
                   
                   #omega = omega, rho_x = rho_x, rho_y = rho_y, rho_xy = rho_xy, tau = tau,
                 omega = omega, rho_x = rho_x, rho_y = rho_y, rho_xy = rho_xy, 
                   
                   control_rate = 1, pre_inv_rate = 1,
                   weight_equal = TRUE,weight_constant = TRUE 
)

tau_sd_a1 <- unlist(lapply( tau_sd[1,], sd))
tau_sd_an <- unlist(lapply( tau_sd[2,], sd))


prob.W_unbias_tau <- sapply( 1:length(set),  function(x){
  equalW.hat <- apply(round(tau_sd[3,][[x]][,-1],3), 1, function(i){sum(i == 0.033) == 30 })
  w.probability <- sum(equalW.hat)/length(equalW.hat)
  return(w.probability)
})

parData_tau <- data.frame(set, "a1.sd" = tau_sd_a1, "an.sd" = tau_sd_an, factorName = "tau", "prob.W_unbias" = prob.W_unbias_tau)
write.csv(parData_tau, "tau.csv")






