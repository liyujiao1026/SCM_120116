source('./global_update.R', echo = F)
# 1. Factorial Design of parameter settings
controlNumber.set <- c(10, 30);
invTime.set = c(10, 40);
rho_x.set <- c(0.1, 0.5)
rho_y.set <- c(0.1, 0.5)
rho_xy.set <- c(0.1, 0.5)
omega.set <- c(0, 0.5)
tau.set <- c(0, 0.5)
post_Inv_Length.set <- c(1, 5, 20)
# control_rate.set <- c(0.5, 1)
# pre_inv_rate.set <- c(0.5, 1)

settings <- expand.grid(controlNumber.set,invTime.set,
                        rho_x.set,rho_y.set,rho_xy.set,omega.set,tau.set,post_Inv_Length.set)

colnames(settings) <- c("controlNumber","invTime",
                        "rho_x","rho_y","rho_xy","omega","tau","post_Inv_Length")
settings$timeLength <- settings$invTime + settings$post_Inv_Length
#write.csv(settings, file = "./setting_matrix.csv", row.names = F)

#=====================================================================#
install.packages("shiny")
install.packages("shinydashboard")
install.packages("tidyr")
install.packages("Synth")
install.packages("compiler")



# 2. start simulation
setwd("/Users/Yujiao/Desktop/SCM_120116/simulation_data_1216/")
source("https://raw.githubusercontent.com/liyujiao1026/SCM_120116/master/global_update.R", echo = F)
range1 <- list( c(1,10), c(11,20), c(21,40), c(41,50))
range2 <- list( c(51,70), c(71,90),c(91,100)) 
range3 <- list( c(101,120), c(121,140), c(141,150)) 
range4 <- list( c(151,170), c(171,190), c(191,200))
range4 <- list( c(201,230), c(231,250), c(251,270), c(271,300))
range5 <- list( c(301,320), c(321,340), c(341,360), c(361,384))  

sapply( range1, Write_SD_Data)
