
source("https://raw.githubusercontent.com/liyujiao1026/SCM_120116/master/global_update.R", echo = F)

tau = 0 ;omega = 0;
rho_x = 1;rho_y = 1;rho_xy = 1;
set <- c(0.01, seq(0.1,0.9,0.1),0.99) # 

Rho_y_sd <- sapply(set, Rep_SCM.Cmp, 
                   rep_times = 200,alpha = 2,
                   controlNumber = 30,timeLength = 30, invTime = 20,beta1 = 0.5, beta2 = 0.5,
                   
                   #omega = omega, rho_x = rho_x, rho_y = rho_y, rho_xy = rho_xy, tau = tau,
                   omega = omega, rho_x = rho_x, rho_xy = rho_xy, tau = tau,
                   
                   control_rate = 1, pre_inv_rate = 1,
                   weight_equal = TRUE,weight_constant = TRUE 
)

Rho_y_sd_a1 <- unlist(lapply( Rho_y_sd[1,], sd))
Rho_y_sd_an <- unlist(lapply( Rho_y_sd[2,], sd))


prob.W_unbias <- sapply( 1:length(set),  function(x){
            equalW.hat <- apply(round(Rho_y_sd[3,][[x]][,-1],3), 1, function(i){sum(i == 0.033) == 30 })
            w.probability <- sum(equalW.hat)/length(equalW.hat)
            return(w.probability)
})

parData_rhoy <- data.frame(set, "a1.sd" = Rho_y_sd_a1, "an.sd" = Rho_y_sd_an, factorName = "rho_y", "prob.W_unbias" = prob.W_unbias)
write.csv(parData_rhoy, "rhoy.csv")











# 
# ggplot(data=parData, aes(x = parVale, y = an.var, col = factor)) +
#             geom_line(aes(linetype = factor), size=1.5) +
#             geom_point(aes(shape = factor, size=1))+ 
#             
#             scale_linetype_manual(values = c(1,1,1,1,1)) +
#             scale_shape_manual(values=c(0,1,2,3,4))+
#             labs(x = "feature value", y = "variance of alphan")+ ylim(c(0,0.3)) + ggtitle("a1.var")+
#             
#             theme(panel.background = element_rect(fill = 'white', colour = 'black'))+ 
#             theme(axis.text=element_text(size=20),
#                   axis.title=element_text(size=18),
#                   
#                   title =element_text(size=18))+
#             
#             theme(legend.text=element_text(size=15))+
#             theme(legend.key.size  = unit(2, "line"))
# grid.ls(grid.force())    # To get the names of all the grobs in the ggplot
# 
# # The edit - to set the size of the point in the legend to 4 mm
# grid.gedit("key-3-1-2.4-2-4-2", size = unit(4, "mm"))    
# grid.gedit("key-4-1-2.5-2-5-2", size = unit(4, "mm"))    
# grid.gedit("key-5-1-2.6-2-6-2", size = unit(4, "mm"))    
# grid.gedit("key-6-1-2.7-2-7-2", size = unit(4, "mm"))    
# grid.gedit("key-7-1-2.8-2-8-2", size = unit(4, "mm"))    
