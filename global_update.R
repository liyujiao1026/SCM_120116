# script SCM_Function.R
#
# the purpose of this script is to simulate the Synthetic Control Method

# this version:  161201
# last change by: Yujiao Li


#library(shiny)
#library(shinydashboard)
library(tidyr)
library(Synth)
library(compiler)

#--- Funcion: X_control.sim() ------------------------------------------------------#




DataGeneration <- function(
            alpha, controlNumber, timeLength, invTime,
            tau, omega, rho_x, rho_y, rho_xy,
            beta1, beta2,
            weight.sim = NULL
            
){ 
            Generate_AR1 <- function(timeLength, tau) {
                        n <- timeLength
                        y <- c()
                        y[1] <- rnorm(n = 1, mean = 0, sd = sqrt(1 - tau ^ 2))
                        for (i in 2:n) {
                                    y[i] <- tau * y[i - 1] + rnorm(n = 1, mean = 0, sd = sqrt(1 - tau ^ 2))
                        }
                        return(y)
                        
            }
            
            
            # 0. Function of control units' covariates ---------------------------------------------#
            X_control.Func <- function(controlNumber, timeLength, tau, omega) {
                        
                        X.base <- matrix(Generate_AR1(timeLength = timeLength, tau = tau),
                                         ncol = 1)
                        X.individual <- replicate(controlNumber,  
                                                  Generate_AR1(timeLength = timeLength, tau = tau),
                        )
                        
                        X.control <- apply(X.individual, 2, function(x){ sqrt(omega) * X.base + sqrt(1 - omega) * x })
                        
                        data.X <- data.frame(X.control)
                        
                        rownames(data.X) <- paste("Time", 1:timeLength , sep = "_")
                        colnames(data.X) <- paste("ControlUnit", 1:controlNumber ,sep = "_")
                        
                        return(data.X)
            }
            
            
            
            e_x1 <- rnorm(timeLength)
            e_x2 <- rnorm(timeLength)
            e_xy <- matrix( rnorm(controlNumber * timeLength) , nrow = timeLength)
            e_y <- rnorm(timeLength)
            
            
            #-------------------------------------------------------------#
            # 1. Covariates of control units
            
            X_control1 <- X_control.Func(controlNumber, timeLength, tau, omega)
            X_control2 <- X_control.Func(controlNumber, timeLength, tau, omega)
            
            
            ## 2. Covariates of treated unit
            if (missing(weight.sim)) {
                        weight.sim <- rep( (1/controlNumber), controlNumber)
            }
            
            X_treat1 <- matrix( 
                        sqrt(rho_x) * apply( X_control1, 1, function(x){ sum(x*weight.sim)} ) + sqrt(1 - rho_x) * e_x1,
                        ncol = 1)
            
            X_treat2 <- matrix( 
                        sqrt(rho_x) * apply( X_control2, 1, function(x){ sum(x*weight.sim)} ) + sqrt(1 - rho_x) * e_x2,
                        ncol = 1)
            
            #  3. Outcome of control units
            Y_control <- sqrt(rho_xy) * (beta1 * X_control1 + beta2 * X_control2)  + sqrt(1 - rho_xy) * e_xy
            
            #  4. Outcome of treated unit (CounterFactual and Factual one )
            Y_treat.CountFact <- matrix( 
                        sqrt(rho_y) * apply(Y_control, 1, function(x){ sum(x*weight.sim)} ) +  sqrt(1 - rho_y) * e_y,
                        ncol = 1)
            
            Y_treat_Fact <- Y_treat.CountFact + c(rep(0, invTime), rep(alpha, (timeLength - invTime )))
            
            
            data.SCM_treat <- data.frame("unit.num" = rep(0, timeLength),
                                         "time" = 1:timeLength,
                                         "name" = "treat_0",
                                         "Y" = Y_treat_Fact, 
                                         "X1" = X_treat1, "X2" = X_treat2)
            
            data.SCM_control <- data.frame("unit.num" = rep(1:controlNumber, each = timeLength),
                                           "time" = rep(1:timeLength, controlNumber),
                                           "name" = rep(colnames(Y_control), each = timeLength),
                                           "Y" = matrix(as.matrix(Y_control), ncol = 1, byrow = F), 
                                           "X1" = matrix(as.matrix(X_control1), ncol = 1, byrow = F),
                                           "X2" = matrix(as.matrix(X_control1), ncol = 1, byrow = F))
            
            data.SCM <- rbind(data.SCM_treat, data.SCM_control)
            #-------------------------------------------------------------#
            
            return(data.SCM)
            
            
}


DataGeneration.Cmp <- cmpfun(DataGeneration)

#------Test------------------------------------------------#
# alpha = 2
# controlNumber = 10
# timeLength = 20
# invTime = 10
# tau = 0.9
# omega = 0.5
# rho_x = 0.2
# rho_y = 0.2
# rho_xy = 0.2
# beta1 = 0.5
# beta2 = 0.5
# set.seed(100)
# u.sim <- runif(controlNumber)
# weight.sim <- u.sim / sum(u.sim)      


# data.SCM <- DataGeneration.Cmp(
#             alpha = alpha, controlNumber = controlNumber ,
#             timeLength = timeLength, 
#             invTime = invTime,
#             tau = tau, omega = omega , 
#             beta1 = beta1, beta2 = beta2,
#             rho_x = rho_x, rho_y = rho_y, rho_xy = rho_xy )

#=====================================================================================#




Estimate_SCM <- function(data.SCM, control_rate, pre_inv_rate, 
                         invTime, timeLength, weight.sim){
            
            if (missing(control_rate)) {control_rate <- 1}
            if (missing(pre_inv_rate)) {pre_inv_rate <- 1}
            
            # Sampling control units (J)
            control_ID_all <- setdiff( unique(data.SCM[,1]), 0)
            control_num <- round( length(control_ID_all) * control_rate, digits = 0)
            if (control_num == 0){
                        control_num  <- 1
                        message("Warnings: the number of control units is set as 1")
            }
            
            
            sample.control_ID <- sort(sample(control_ID_all, control_num ))
            
            
            
            # Sampling preintervention time (T0)
            
            pre_intervention_all <- 1:invTime
            inv_num <- round( length(pre_intervention_all) * pre_inv_rate, digits = 0)
            
            if (inv_num == 0) {
                        inv_num  <- 1
                        message("Warnings: the length of pre-intervention time is set as 1")
            }
            
            sample.inv_time <- sort(sample(pre_intervention_all, inv_num ))
            
            
            data.SCM$name <- as.character( data.SCM$name )
            dataprep.out <-
                        dataprep(
                                    foo = data.SCM,
                                    predictors = c("X1", "X2"),
                                    predictors.op = "mean",
                                    dependent = "Y",
                                    unit.variable = "unit.num",
                                    time.variable = "time",
                                    treatment.identifier = 0,
                                    controls.identifier = sample.control_ID  ,
                                    time.predictors.prior = 1:invTime,
                                    time.optimize.ssr = sample.inv_time,
                                    unit.names.variable = "name",
                                    time.plot = 1:max(data.SCM[,2])
                        )
            
            synth.out <- synth(dataprep.out)
            gaps <- round( dataprep.out$Y1plot - (dataprep.out$Y0plot %*% synth.out$solution.w), 
                           digits = 4)
            
            gap_1 <- gaps[invTime + 1]
            gap_n <- mean(gaps[(invTime + 1):timeLength])
            
            colnames(gaps) <- "gaps"
            
            w.hat0 <- synth.out$solution.w
            ID <- as.numeric(row.names(w.hat0))
            w_value <- as.numeric(w.hat0)
            w.hat1 <- cbind(ID, w_value)
            
            if (control_rate == 1) {
                        w.hat <- w.hat1
            }else{
                        w0_ID <- setdiff(control_ID_all, sample.control_ID)
                        w.hat2 <- data.frame(rbind(w.hat1, cbind(w0_ID, 0)))
                        w.hat <- w.hat2[order(w.hat2[,1]),]
                        
            }
            
            
            w.error <- data.frame("ID" = w.hat[,1], "w.error" =  weight.sim - as.numeric(w.hat[,2]))
            row.names(w.error) <- NULL            
            
            
            return(list("w.error" =  w.error, "gap_1" = gap_1, "gap_n" = gap_n ))
            
            
}

Estimate_SCM.Cmp <- cmpfun(Estimate_SCM)

#------Test------------------------------------------------#
# control_rate <- 0.5
# pre_inv_rate <- 0.5
# result <- Estimate_SCM.Cmp(data.SCM = data.SCM, control_rate, pre_inv_rate, 
#                            invTime, timeLength, weight.sim)

#==================================================================================================#




Sim_SCM <- function(alpha, controlNumber, timeLength, invTime,beta1, beta2,
                    tau, omega, rho_x, rho_y, rho_xy,
                    weight.sim = NULL, control_rate = NULL, pre_inv_rate = NULL
){
            
            if (missing(weight.sim)) {
                        weight.sim <- rep( (1/controlNumber), controlNumber)
            }
            
            if ( missing(control_rate) ) { control_rate <- 1}
            if ( missing(pre_inv_rate) ) { pre_inv_rate <- 1}
            
            
            data_SCM <- DataGeneration.Cmp(
                        alpha = alpha, controlNumber = controlNumber, 
                        timeLength = timeLength, invTime = invTime,
                        tau = tau, omega = omega , rho_x = rho_x, rho_y = rho_y, rho_xy = rho_xy,
                        beta1 = beta1, beta2 = beta2,
                        weight.sim = weight.sim
            )
            
            result <- Estimate_SCM.Cmp(data.SCM = data_SCM, 
                                       control_rate = control_rate, 
                                       pre_inv_rate = pre_inv_rate, 
                                       weight.sim = weight.sim,
                                       invTime = invTime, timeLength =  timeLength)
            
            return(result)
}


Sim_SCM.Cmp <- cmpfun(Sim_SCM)
#========================================================================#


# ## Test
# set.seed(100); alpha <-  2;controlNumber <- 10;
# u.sim <- runif(controlNumber);
# weight.sim <- u.sim / sum(u.sim); timeLength = 20;invTime = 10;
# tau = 0.5;omega = 0.5;rho_x = 0.2;rho_y = 0.2;rho_xy = 0.2;
# beta1 = 0.5;beta2 = 0.5;
# control_rate <- 1; pre_inv_rate <- 1


# result2 <- Sim_SCM.Cmp(
#             alpha = alpha,
#             controlNumber = controlNumber,
#             timeLength = timeLength, 
#             invTime = invTime,
#             rho_x = rho_x, rho_y = rho_y, rho_xy = rho_xy,
#             tau = tau, omega = omega , 
#             beta1 = beta1, beta2 = beta2,
#             weight.sim = weight.sim,
#             control_rate = control_rate, pre_inv_rate =  pre_inv_rate
# )
# # 
# 
# final <- replicate(2, 
#                    Sim_SCM.Cmp(
#                                alpha = alpha,
#                                weight.sim = weight.sim,controlNumber = controlNumber,
#                                timeLength = timeLength, invTime = invTime,
#                                tau = tau, omega = omega , beta1 = beta1, beta2 = beta2,
#                                rho_x = rho_x, rho_y = rho_y, rho_xy = rho_xy,
#                                control_rate = control_rate, pre_inv_rate =  pre_inv_rate)
# )

##====================================================================================#


Rep_SCM <- function(
            rep_times,
            
            alpha, controlNumber, timeLength, invTime, beta1, beta2,
            tau, omega, rho_x, rho_y, rho_xy,
            control_rate, pre_inv_rate,
            
            weight_equal= NULL, weight_constant=NULL
){
            
            
            if(missing(weight_equal)) {weight_equal <- TRUE}
            if(missing(weight_constant)) {weight_constant <- TRUE}
            
            weightGenerate <- function(n){
                        u.sim <- runif(n)
                        w <- u.sim / sum(u.sim) 
                        return(w)
            }
            
            
            
            
            
            
            if (weight_constant == TRUE) { 
                        
                        if (weight_equal == TRUE) {
                                    weight.sim <- rep( 1/controlNumber, controlNumber)
                        } else {
                                    weight.sim <- weightGenerate(controlNumber)
                        }
                        
                        result <- replicate(
                                    rep_times, 
                                    Sim_SCM.Cmp(
                                                alpha = alpha,
                                                weight.sim = weight.sim,
                                                controlNumber = controlNumber,
                                                timeLength = timeLength, invTime = invTime,
                                                beta1 = beta1, beta2 = beta2,
                                                tau = tau, omega = omega, 
                                                rho_x = rho_x, rho_y = rho_y, rho_xy = rho_xy,
                                                control_rate = control_rate, 
                                                pre_inv_rate = pre_inv_rate))
                        gap_1 <- unlist(result[2,])
                        gap_n <- unlist(result[3,])
                        Fun.weight <- function(x){result[1,][[x]][,2]}
                        w.e <- cbind(1:rep_times,round( t(sapply(1:rep_times, Fun.weight)), 4))
                        weight.error <- data.frame(w.e)
                        
                        w.true <- matrix(round(weight.sim,4), nrow = 1)
                        weight.true <- data.frame(w.true)
                        colnames(weight.true) <- paste0("unit",1:controlNumber)
                        
            } else {
                        
                        weight.test <- replicate(rep_times, weightGenerate(controlNumber))
                        result <- apply( weight.test, 2, FUN = Sim_SCM.Cmp, 
                                         alpha = alpha,
                                         controlNumber = controlNumber,
                                         timeLength = timeLength, invTime = invTime,
                                         beta1 = beta1, beta2 = beta2,
                                         tau = tau, omega = omega, 
                                         rho_x = rho_x, rho_y = rho_y, rho_xy = rho_xy,
                                         control_rate = control_rate, 
                                         pre_inv_rate = pre_inv_rate)  
                        
                        Fun.gap_1 <- function(x){result[[x]]$gap_1}
                        Fun.gap_n <- function(x){result[[x]]$gap_n}
                        Fun.weight <- function(x){result[[x]]$w.error[,2]}
                        
                        gap_1 <- sapply(1:rep_times, Fun.gap_1)
                        gap_n <- sapply(1:rep_times, Fun.gap_n)
                        w.e <- cbind(1:rep_times,round( t(sapply(1:rep_times, Fun.weight)), 4))
                        weight.error <- data.frame(w.e)
                        weight.true <- NULL
            }
            
            colnames(weight.error) <- c("Rep_Times", paste0("unit",1:controlNumber))
            
            
            return( list("gap_1" = gap_1, "gap_n" = gap_n, "weight.error" = weight.error,
                         "weight.true" = weight.true))
}


Rep_SCM.Cmp <- cmpfun(Rep_SCM)

#========================================================================#
## TEST
# 
# set.seed(100); alpha <-  2;controlNumber <- 10;timeLength = 20;invTime = 10;
# tau = 0.5;omega = 0.5;rho_x = 0.2;rho_y = 0.2;rho_xy = 0.2;beta1 = 0.5;beta2 = 0.5;
# control_rate <- 1;pre_inv_rate <- 1;
# rep_times <- 3;
# weight_constant = TRUE; weight_equal = FALSE; 
# 
# Result4 <- Rep_SCM.Cmp(
#             rep_times = rep_times,
#             
#             alpha = alpha,
#             controlNumber = controlNumber,
#             timeLength = timeLength, invTime = invTime,
#             beta1 = beta1, beta2 = beta2,
#             tau = tau, omega = omega, 
#             rho_x = rho_x, rho_y = rho_y, rho_xy = rho_xy,
#             control_rate = control_rate, 
#             pre_inv_rate = pre_inv_rate,
#             
#             weight_equal = weight_equal ,weight_constant = weight_constant 
# )






##==================================================================================================#
plotFun <- function(gap_1.v, gap_n.v){
            
            x_lim <- c( min(c(gap_1.v,gap_n.v)) - 0.5 * sd(gap_n.v), max(c(gap_1.v,gap_n.v)) + 0.5 * sd(gap_n.v))
            y.max <-  max(c(density(gap_1.v)$y, density(gap_n.v)$y))
            y_lim <- c( 0 , 1.3 * y.max )
            
            gap_1.Q <- round(quantile(gap_1.v, c(0.025, 0.975)),3)
            gap_n.Q <- round(quantile(gap_n.v, c(0.025, 0.975)),3)
            
            
            par(mfrow = c(1,2))
            
            # alpha 1
            hist(gap_1.v, xlab = "alpha_1", ylab = "", freq = F, 
                 xlim = x_lim, ylim = y_lim, col = "lightyellow",
                 main = paste0("hist of alpha_1"," (sd: ",round(sd(gap_1.v),3) ,")" )
            )
            axis(labels = NA, side = 1, tck = -0.015, 
                 at = c(seq(from = gap_1.Q[1], to = gap_1.Q[2], by = 0.001)), col = "lightskyblue")
            text(x = gap_1.Q[1], y = 0.1 * y.max,  gap_1.Q[1], cex = 0.85, col = "blue")
            text(x = gap_1.Q[2], y = 0.1 * y.max,  gap_1.Q[2], cex = 0.85, col = "blue")
            text(x = mean(gap_1.v), y = 1.1 * y.max,  round(mean(gap_1.v),3), cex = 0.9, col = "blue")
            abline(v = round(mean(gap_1.v),3), lty = 2, col = "black")
            lines(density(gap_1.v), col = "black", lwd = 2) 
            
            
            # alpha n
            
            hist(gap_n.v, xlab = "alpha_n", ylab = "", freq = F, 
                 xlim = x_lim, ylim = y_lim, col = "paleturquoise",
                 main = paste0("hist of alpha_n"," (sd: ",round(sd(gap_n.v),3), ")")
            )
            axis(labels = NA, side = 1, tck = -0.015, 
                 at = c(seq(from = gap_n.Q[1], to = gap_n.Q[2], by = 0.001)), col = "lightskyblue")
            text(x = gap_n.Q[1], y = 0.1 * y.max,  gap_n.Q[1], cex = 0.85, col = "blue")
            text(x = gap_n.Q[2], y = 0.1 * y.max,  gap_n.Q[2], cex = 0.85, col = "blue")
            text(x = mean(gap_n.v), y = 1.1 * y.max,  round(mean(gap_n.v),3), cex = 0.9, col = "blue")
            abline(v = round(mean(gap_n.v),3), lty = 2, col = "black")
            lines(density(gap_n.v), col = "black", lwd = 2) 
}



##==================================================================================================#
## Return standard deviation with applying function 
Rep_SCM_SD <- function(x){
            Result <- Rep_SCM.Cmp(
                        controlNumber = x[1], invTime = x[2], timeLength = x[9],
                        rho_x = x[3], rho_y = x[4], rho_xy = x[5],omega = x[6], tau = x[7], 
                        
                        rep_times = 200, alpha = 2, beta1 = 0.5, beta2 = 0.5,
                        control_rate = 1, pre_inv_rate = 1, weight_equal = FALSE ,weight_constant = FALSE 
            )
            
            Sd_gap_1 <- sd(Result$gap_1)
            Sd_gap_n <- sd(Result$gap_n)
            
            return(c(Sd_gap_1, Sd_gap_n))
            
}
Rep_SCM_SD.Cmp <- cmpfun(Rep_SCM_SD) 

# test
# settingMatrix <- as.matrix(read.csv("./setting_matrix.csv"))
# res <- t(apply(settingMatrix[1:100,], 1, Rep_SCM_SD.Cmp))



##==================================================================================================#
settingMatrix <- as.matrix(read.csv("https://raw.githubusercontent.com/liyujiao1026/SCM_120116/master/setting_matrix.csv"))
Write_SD_Data <- function(range){
            start <- range[1]
            end <- range[2]
            sd_file <- data.frame( round( t(apply(settingMatrix[start:end,], 1, Rep_SCM_SD.Cmp)), 4))
            colnames(sd_file) <- c("Sd_gap_1", "Sd_gap_n")
            write.csv(sd_file, file = paste0("sd.",start,"_",end,".csv"),row.names = start:end)
}
