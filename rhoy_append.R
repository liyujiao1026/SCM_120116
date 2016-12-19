setwd("/Users/Yujiao/Desktop/SCM_120116/simulation_data_1216/sd.data_append_rhoy/")

source("https://raw.githubusercontent.com/liyujiao1026/SCM_120116/master/global_update.R", echo = F)
setting_rhoyMatrix <- as.matrix(read.csv("https://raw.githubusercontent.com/liyujiao1026/SCM_120116/master/settings_rhoy_matrix.csv"))



Write_SD_Data_rhoy <- function(range){
            start <- range[1]
            end <- range[2]
            sd_file <- data.frame( round( t(apply(setting_rhoyMatrix[start:end,], 1, Rep_SCM_SD.Cmp)), 4))
            colnames(sd_file) <- c("Sd_gap_1", "Sd_gap_n")
            write.csv(sd_file, file = paste0("sd.",start,"_",end,".csv"),row.names = start:end)
}


#====#
range <- list( c(1,3), c(4,10), c(11,40),c(41,60)) 
sapply( range, Write_SD_Data_rhoy)
