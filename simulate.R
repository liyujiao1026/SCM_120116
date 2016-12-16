source("https://raw.githubusercontent.com/liyujiao1026/SCM_120116/master/global_update.R", echo = F)
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
