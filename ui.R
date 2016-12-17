#ui.R

source('./global_update.R', echo = F)
library(shiny)
library(shinydashboard)


## 1. header --------------------------------------------------------------------------##
header <- dashboardHeader(title = "Power of Synthetic Control Method ", titleWidth = 300)


## 2. siderbar --------------------------------------------------------------------------##
siderbar <- dashboardSidebar(
            width = 250,
            sidebarMenu(
                        menuItem("SCM", tabName = "SCM", icon = icon("play-circle")) ,
                        menuItem("Weight Estimation", tabName = "data", icon = icon("table")),
                        menuItem("Source code", icon = icon("file-code-o"),
                                 href = "https://github.com/liyujiao1026/SCM_120116/tree/master")
            )
)


## 3. body --------------------------------------------------------------------------##
body <- dashboardBody(
            
            tabItems(  
                        
                        #first tab
                        tabItem(
                                    tabName = "SCM",    
                                    fluidRow(  
                                                
                                                # First 
                                                box(width  = 12, solidHeader = TRUE, status = "warning",title = "Input",
                                                    
                                                    # basic info
                                                    column(width = 3, offset = 1,
                                                           div(head("1. Basic settings"), 
                                                               style = "color:darkorange;font-size:16px;font-weight: 600;"),
                                                           br(),
                                                           textInput(inputId = "alpha", label = "alpha", value = 2),
                                                           textInput(inputId = "controlNumber", label = "control number", value = 9),
                                                           textInput(inputId = "timeLength", label = "total period", value = 20),
                                                           textInput(inputId = "invTime", label = "intervention time", value = 10),
                                                           textInput(inputId = "beta1", label = "beta1", value = 0.5),
                                                           textInput(inputId = "beta2", label = "beta2", value = 0.5)
                                                           
                                                           
                                                    ),
                                                    
                                                    
                                                    column(width = 3, offset = 1,
                                                           div(head("2. Five features"), 
                                                               style = "color:darkorange;font-size:16px;font-weight: 600;"),
                                                           br(),
                                                           
                                                           textInput(inputId = "tau", label = "tau", value = 0.25),
                                                           textInput(inputId = "omega", label = "omega", value = 0.35),
                                                           textInput(inputId = "rho_x", label = "rho_x", value = 0.35),
                                                           textInput(inputId = "rho_y", label = "rho_y", value = 0.15),
                                                           textInput(inputId = "rho_xy", label = "rho_xy", value = 0.25)
                                                    ),
                                                    
                                                    
                                                    column(width = 3, offset = 1,
                                                           div(head("3. Weight setting"), 
                                                               style = "color:darkorange;font-size:16px;font-weight: 600;"),
                                                           br(),
                                                           
                                                           checkboxInput("weight_constant", "Constant weight in simulation", value = TRUE),
                                                           
                                                           conditionalPanel("input.weight_constant == 1",
                                                                            checkboxInput("weight_equal", "Equally weighted", value = 1)               
                                                           ),
                                                           textInput(inputId = "rep_times", label = "replication times", value = 15),
                                                           
                                                           br(),
                                                           div(head("4. Sampling percentage"), 
                                                               style = "color:darkorange;font-size:16px;font-weight: 600;"),
                                                           
                                                           textInput(inputId = "control_rate", label = "control_units_rate", value = 1),
                                                           textInput(inputId = "pre_inv_rate", label = "pre_intervention_rate", value = 1),
                                                           
                                                           
                                                           actionButton("Submit", label = "Submit",icon("paper-plane"), 
                                                                        style = "color: #fff; background-color:#FFA500;border-color:#2e6da4; width:150px")
                                                           
                                                    )
                                                ),
                                                
                                                box(width  = 12, solidHeader = TRUE, status = "warning",title = "Output",
                                                    plotOutput("plot")
                                                )
                                    )
                        ),
                        
                        
                        
                        # 2nd tab
                        tabItem(tabName = "data",
                                div(head("1. Weight.true"), 
                                    style = "color:darkorange;font-size:16px;font-weight: 600;"),
                                
                                dataTableOutput("weight.true"),
                                br(),
                                
                                div(head("2. Weight.Estimation"), 
                                    style = "color:darkorange;font-size:16px;font-weight: 600;"),
                                
                                dataTableOutput("weight_hat.result")
                        )
                        
            )
)



dashboardPage(header,siderbar,body)          

