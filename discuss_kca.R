
data.path <- "https://raw.githubusercontent.com/liyujiao1026/SCM_120116/master/sd_data_simulation.csv"

SD_data <- read.csv(data.path)
#------gap_n--------#
model_interact_all <- lm(Sd_gap_1~controlNumber*invTime*rho_x*rho_y*rho_xy*omega*tau*post_Inv_Length, data = SD_data)
anova(model_interact_all)

model_gap_1.a <- lm(Sd_gap_1 ~ controlNumber+invTime+rho_x+rho_y+rho_xy+omega+tau+post_Inv_Length, data = SD_data)
summary(model_gap_1.a)

model_gap_1.b <- lm(Sd_gap_1 ~ controlNumber*invTime*rho_x*rho_y*rho_xy*omega, data = SD_data)
summary(model_gap_1.b)
anova(model_gap_1.b)