
source('./global.R', echo = F)
#source("https://raw.githubusercontent.com/liyujiao1026/SCM_120116/master/global.R", echo = F)

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
write.csv(settings, file = "./setting_matrix.csv", row.names = F)

#======================================================================#
# 2. loop parameter setting matrix to run SCM and gain variance in different situations
source("https://raw.githubusercontent.com/liyujiao1026/SCM_120116/master/global.R", echo = F)
settingMatrix <- as.matrix(read.csv("https://raw.githubusercontent.com/liyujiao1026/SCM_120116/master/setting_matrix.csv"))
Write_SD_Data <- function(range){
            start <- range[1]
            end <- range[2]
            sd_file <- data.frame( round( t(apply(settingMatrix[start:end,], 1, Rep_SCM_SD.Cmp)), 4))
            colnames(sd_file) <- c("Sd_gap_1", "Sd_gap_n")
            write.csv(sd_file, file = paste0("sd.",start,"_",end,".csv"),row.names = start:end)
}


range <- list( c(1,384))  
sapply( range, Write_SD_Data)


#======================================================================#

# 3. Analyze 


SD_data <- read.csv("./Simulation data/SD_data_simulation.csv")

#------gap_n--------#
model_interact_all <- lm(Sd_gap_1~controlNumber*invTime*rho_x*rho_y*rho_xy*omega*tau*post_Inv_Length, data = SD_data)
anova(model_interact_all)

model_gap_1.a <- lm(Sd_gap_1 ~ controlNumber+invTime+rho_x+rho_y+rho_xy+omega+tau+post_Inv_Length, data = SD_data)
summary(model_gap_1.a)

model_gap_1.b <- lm(Sd_gap_1 ~ controlNumber*invTime*rho_x*rho_y*rho_xy*omega, data = SD_data)
summary(model_gap_1.b)
anova(model_gap_1.b)



#------gap_n--------#
model_gap_n.a <- lm(Sd_gap_n ~ controlNumber+invTime+rho_x+rho_y+rho_xy+omega+tau+post_Inv_Length, data = SD_data)
summary(model_gap_n.a)
anova(model_gap_n.a)


model_gap_n.b <- lm(Sd_gap_n ~ controlNumber*rho_y*post_Inv_Length, data = SD_data)
summary(model_gap_n.b)
anova(model_gap_n.b)



