source('./global_update.R', echo = F)
shinyServer(function(input, output) {
            
            synthResult <- eventReactive(input$Submit, {
                        
                        withProgress(message = 'Simulating ... ', value = 0, {
                                    
                                    alpha <- as.numeric(input$alpha)
                                    controlNumber <- as.numeric(input$controlNumber)
                                    timeLength = as.numeric(input$timeLength)
                                    invTime = as.numeric(input$invTime)
                                    beta1 = as.numeric(input$beta1 )
                                    beta2 = as.numeric(input$beta2)
                                    tau = as.numeric(input$tau)
                                    omega = as.numeric(input$omega) 
                                    rho_x = as.numeric(input$rho_x)
                                    rho_y = as.numeric(input$rho_y)
                                    rho_xy = as.numeric(input$rho_xy)
                                    control_rate = as.numeric(input$control_rate)
                                    pre_inv_rate =  as.numeric(input$pre_inv_rate)
                                    rep_times <- as.numeric(input$rep_times)

                                    
                                    Result <- Rep_SCM.Cmp(
                                                rep_times = rep_times,
                                                
                                                alpha = alpha,
                                                controlNumber = controlNumber,
                                                timeLength = timeLength, invTime = invTime,
                                                beta1 = beta1, beta2 = beta2,
                                                tau = tau, omega = omega, 
                                                rho_x = rho_x, rho_y = rho_y, rho_xy = rho_xy,
                                                control_rate = control_rate, 
                                                pre_inv_rate = pre_inv_rate,
                                                
                                                weight_equal = input$weight_equal ,
                                                weight_constant = input$weight_constant 
                                    )
                                    
                                    
                                    
                                    })
            })
            
            

            
            output$weight_error.result <- renderDataTable({
                        data.frame(synthResult()$weight.error)
            })
            
            
            output$weight.true <- renderDataTable({
                        data.frame(synthResult()$weight.true)
            })
            
            
            output$plot <- renderPlot({
                        plotFun(synthResult()$gap_1, synthResult()$gap_n)
           
            })
            
            
            
            
})


