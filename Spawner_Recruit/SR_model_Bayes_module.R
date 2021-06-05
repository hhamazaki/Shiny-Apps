#===============================================================================    
#  SR_model_Bayes.R  Shiny app for Bayesian Spawner Recruit Analyses 
#  This code read data, create brood table, conduct SR model and 
#  escapement goal analyses 
#  Author:  Toshihide "Hamachan" Hamazaki 
#      
# Naming conventions 
# Data  : Dat_xxxx 
# Plot  : Plt_xxxx
# Table : Tbl_xxxx
# Text  : Txt_xxxx 
#===============================================================================
#initialize
library(shiny)        # used for creating Shiny 
library(shinythemes)  # used to specify themes
library(bslib)
library(markdown)     # used to read markdown file
library(knitr)
library(datasets)
library(lmtest)       # used for dwtest 
library(mgcv)         # used for spline 
library(maptools)     # used for unoverlapping labels 
library(coda)         # used to read MCMC data 
library(R2jags)       # used to run JAGS
library(openxlsx)     # used for creating EXCEL output table  

options(scipen=999)   # Do not show Scientific notation
source("Shiny_modules.R")
source("Shiny_SR_functions.R")
source("Shiny_Bayes_modules.R")
#===============================================================================    
#  UI:  
#===============================================================================
ui<-fluidPage(
 navbarPage(
    theme =  bs_theme(version = 3, bootswatch = 'cerulean'), id = "tabs",
    title = div(
        img(src="Picture2.png",height=40, width=40)
        , "Pacific Salmon Escapement Goal Analyses"),
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 1:  Data Input and Submit 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#navbarMenu("Data Input & Bayes model",
  tabPanel("Data Input",
   sidebarPanel(width = 3,
#-------------------------------------------------------------------------------    
#  File Input
#-------------------------------------------------------------------------------
# Create brood and SR module
selectInput(inputId="dataType","Data Type", choices = c('Run','S-R','Escapement Only')),
# If data is "Run" select first age 
conditionalPanel(condition="input.dataType== 'Run'",
  # Input: Select what to display
  numericInput("fage", "First Run Age", value=4,min=1,max=20,step=1)
  ),
#-------------------------------------------------------------------------------    
# Module:  File Inuput  
#-------------------------------------------------------------------------------
# File Input module 
  dataInputUI("datain", "User data (.csv format)"),
#-------------------------------------------------------------------------------    
# UI:  Limit data 
#-------------------------------------------------------------------------------
  uiOutput('yrange'),
#-------------------------------------------------------------------------------
  hr(),
   ), # End SidePanel

#=========================MainPanel=============================================
  mainPanel(
    tabsetPanel(id="subTab",
#------------------ Show Input data --------------------------------------------      
      tabPanel("Table",  
               strong(uiOutput('note')),
               dataTableOutput('Tbl_data')),
#------------------ Brood Table ------------------------------------------------      
      tabPanel("Brood Table",
                dataTableOutput("Tbl_data.brood")
                  ),
#------------------ Time Series ------------------------------------------------
      tabPanel("Time Series",
              plotOutput("Plt_srt"),
              plotOutput('Plt_runesc')  
              # Activates only when run is available
                ),#End tabPanel
#------------------ Data summary  ----------------------------------------------
      tabPanel("Summary",
               verbatimTextOutput('Txt_sum.data'),
               plotOutput('Plt_hist.sry')
             ),
      tabPanel("Help",
               (               
               includeMarkdown("documents/Input_help.md")
               )
          )  # End tabPanel
        )  # End tabsetPanel
      )  # End mainPanel
     ), # End tabpanel
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 2 Escapement Only Analyses 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++    
tabPanel("Escapement Only Analyses",
    sidebarPanel(width = 3,
      conditionalPanel(condition="input.ePanel == 'Percentile Analyses'",  
        p(strong("Percentile Analyses")),
        PercentileUI("prcnt"),   # Percentile Analyses Module
        (htmlOutput("Txt_Tier")),
              hr(),
        (htmlOutput("Txt_Note"))
                ), # End conditionalPanel
      conditionalPanel(condition="input.ePanel == 'Risk Analyses'",                        
        p(strong("Risk Analyses")), 
        strong(textOutput("Txt_Risk_Model")),
              hr(),
        RiskUI("risk"),   # Risk Analyses Module
#        strong(htmlOutput('Txt_Risk'))
                 ) # End conditionalPanel
          ), #End SidbarPanel
#============= Main Panel ======================================================    
  mainPanel(
    tabsetPanel(
#------------------ Percentile Analyses ----------------------------------------   
      tabPanel("Percentile Analyses",
        plotOutput("Plt_prcnt")   
            ), #End tabPanel:Percentile
#------------------ Risk Analyses  ---------------------------------------------   
      tabPanel("Risk Analyses",
        plotOutput(height = '400px', "Plt_risk"),
        plotOutput(height = '300px', "Plt_risk2"),
          p(strong("Durbin-Watson Serial Correlation Analyses")),
          verbatimTextOutput('Txt_dwtest')
            ), #End tabPanel: Risk
      tabPanel("Help",
          withMathJax(               
            includeMarkdown("documents/ESC_Analyses_help.md")
              )
            ), #End tabPanel: Help
          id = "ePanel"
           )#End tabsetPanel
         )#End main Panel 
  ),#End tabPanel Escapement only Analysis 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 3  Data Analyses 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#navbarMenu("SR Model Analyses",
  tabPanel("SR Model",
    sidebarPanel(width = 3,
#---------  Bayes Model Control Sidebar ----------------------------------------                 
      conditionalPanel(condition="input.Panel == 'Bayes Model'",
        p("Bayesian Model Setting"),
           selectInput('Model',"Select SR Model",choice=list('Ricker','Beverton-Holt'),
                    selected ='Ricker'),
        radioButtons(inputId="add","Model Addition",
            choices=c("None"="","AR(1) Error"="ar1","Time varying alpha"="kf"),
            selected = NULL),
#-------------------------------------------------------------------------------    
# UI:  Bayes Model Control UI
#-------------------------------------------------------------------------------
    BayesInputUI('Bayes')
          ),  # End conditional Panel
#---------  SR Modle Sidebar ----------------------------------------
    conditionalPanel(condition="input.Panel == 'SR Plot'|| input.Panel == 'Yield Plot'", 
        uiOutput('astar'),  
        p(strong("Plot options")),               
        checkboxInput(inputId="show.points", "show Years", FALSE), 
        checkboxInput(inputId="show.smsy", "show Smsy", FALSE),
        checkboxInput(inputId="show.smax", "show Smax", FALSE),
        checkboxInput(inputId="show.int", "show Interval", TRUE),
        sliderInput("CI", "% Interval", value=90,min=0,max=100,step=5),
        selectInput(inputId="Li","Interval Type", choices = c('confidence','prediction')),
        selectInput(inputId="ui","Figure Axis Dislpay Unit", choices = c(1,1000,1000000))
                  ),
       conditionalPanel(condition="input.Panel == 'MCMC'", 
       sliderInput(inputId="CIB", "% Interval", value=90,min=0,max=100,step=5)
       )   
# End Conditional panel  
       ), # End sidebarPanel
       
#=========================MainPanel=============================================       
  mainPanel(tabsetPanel(
#------------------ Bayes Model ------------------------------------------------        
    tabPanel("Bayes Model",
        p(strong("Trace Plots")),
        plotOutput("Plt_trace"),
        p(strong("Model summary")), 
        verbatimTextOutput('BayesSum')  
          ),#End tabPanel:Bayes
#------------------ SR Plot-----------------------------------------------------        
    tabPanel("SR Plot",
        plotOutput(height='500px','Plt_SR'),
#        downloadButton("down", label = 'Download the plot')
            ),#End tabPanel: SR 
#------------------ Yield Plot--------------------------------------------------        
    tabPanel("Yield Plot",
        plotOutput(height='500px','Plt_yield')
            ),#End tabPanel: Yield
#------------------ Residuals  ------------------------------------------------- 
    tabPanel("Diagnoses", 
        plotOutput(height='300px',"Plt_predict"),     
        plotOutput(height='300px',"Plt_residual"),
        plotOutput(height='300px',"Plt_lnalphai")
            ),#End tabPanel: Diagnoses
#------------------ MCMC ------------------------------------------------------- 
    tabPanel("MCMC",
        p(strong("Outliers Removed")),     
        verbatimTextOutput('sumpost'),
        plotOutput("Plt_hist.mc")
            ),#End tabPanel: MCMC
#------------------ Bayes Model Code ------------------------------------------- 
    tabPanel("Model Codes",
        verbatimTextOutput('modelcode')
            ),#End tabPanel: Model Code
tabPanel("Help",
         withMathJax(               
           includeMarkdown("documents/JAGS_help.md")
         )
#         downloadButton("downloadData", label = 'Download'),
#         dataTableOutput("temp")
            ), # End tabPanel: Help
    id = "Panel"
           )#End tabsetPanel
        )#End mainPanel
      ),#End tabPanel

#   ),#End nabVarMenu

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 3: Escapement Goal Analyses 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
navbarMenu("Escapement Goal Analyses",
#-------------------------------------------------------------------------------    
#  tabPanel: Smsy-Smax Goal Analyses 
#-------------------------------------------------------------------------------    
  tabPanel("Smsy & Smax Goal Analyses",      
    sidebarPanel(width = 3,
       ProfileUI('smsy','MSY'),
       hr(),
       ProfileUI('smax','Rmax'),
          ),#End sidebarPanel
#=========================MainPanel=============================================       
    mainPanel(
      tabsetPanel(
#------------------ Smsy Profile ----------------------------------------------- 
        tabPanel("Profile",
           ProfPlotUI('smsy.p','MSY'),  
           ProfPlotUI('smax.p','Rmax'),       
            ), #End tabPanel: Profile
#------------------ Smsy Yield Profile -----------------------------------------
        tabPanel("Yield & Recruit",
          plotOutput(height='300px','Plt_yield.pg'),
          plotOutput(height='300px','Plt_rec.pg')
              ), #End tabPanel: YieldRec
        tabPanel("Goal Ranges",
                p(strong("Smsy Goal Range")),        
          htmlOutput("Txt_Srange.smsy"),
                p(strong("Smax Goal Range")), 
          htmlOutput("Txt_Srange.smax")
              ), #End tabPanel: Goal 
        tabPanel("Help",
                 withMathJax(               
                   includeMarkdown("documents/Profile_help.md")
                 )
                ) #End tabPanel: Help  
            )#End tabsetPanel
        )#End mainPanel
      ),#End tabPanel: Smsy Goal Analyses 
 
#-------------------------------------------------------------------------------    
#  tabPanel Recruit & Yield Goal Analyses 
#-------------------------------------------------------------------------------    
  tabPanel("Yield & Recruit Goal Analyses",
    sidebarPanel(width = 3,
      conditionalPanel(condition="input.cPanel == 'Recruit Goal Analyses'",  
        p(strong("Recruit Goal Analyses")), 
        uiOutput('minRec'),
        sliderInput("r1p", "Min % Achieve", value=90,min=0, max=100,step=5)
        ), # End conditionalPanel
      conditionalPanel(condition="input.cPanel == 'Yield Goal Analyses'",                        
        p(strong("Yield GoalAnalyses")),
        uiOutput('minYield'),
        sliderInput("y1p", "Min % Achieve", value=90,min=0, max=100,step=5)
        ) # End conditionalPanel
      ),  # End sidebarPanel

#=========================MainPanel=============================================       
    mainPanel(
      tabsetPanel(
#------------------ Yield Goal  Profile ----------------------------------------                
        tabPanel("Yield Goal Analyses",
            plotOutput(height='300px','Plt_yield.gl'),
            plotOutput(height='300px','Plt_yield.prof'),
            verbatimTextOutput("Txt_Yield_gl")
        ),
#------------------ Recruit Goal Profile ---------------------------------------   
        tabPanel("Recruit Goal Analyses",
          plotOutput(height='300px','Plt_rec.gl'),
          plotOutput(height='300px','Plt_rec.prof'),
          verbatimTextOutput("Txt_Rec_gl")
          ),# End tabPanel
        tabPanel("Help",
          withMathJax(               
            includeMarkdown("documents/Yield_Recruit_help.md")
            )        
        ),# End tabPanel
        id = "cPanel"
          )#End tabsetPanel
        )#End maiPanel
      ),#End tabPanel Recruit & Yield Goal Analyses 
              
#-------------------------------------------------------------------------------    
# tabPanel Custom Escapement Goal Evaluation 
#-------------------------------------------------------------------------------    
  tabPanel("Custom Escapment Goal Range",
    sidebarPanel(width = 3,
      p(strong("Select Lower and Upper Escapement Goal")),
      fluidRow(
      column(6,uiOutput('minEG')),  
      column(6,uiOutput('maxEG'))
       ),
      p("Submit Goal Range for  Analyses"),
      actionButton("Run","Run"),
# Horizontal line ----
      tags$hr(),
      uiOutput('cyg'),
      uiOutput('crg')

                       ), #End Sidepar Panel
#============= Main Panel ======================================================    
    mainPanel(
      tabsetPanel(
#------------------ Profile ----------------------------------------------------   
        tabPanel("Profile Analyses",
          plotOutput('plt_msyprof_c'),
          plotOutput('plt_maxprof_c')
#                 splitLayout(cellWidths = c("50%", "50%"),
#                             p(strong("Mean and Annual Yields Summary")),
#                             p(strong("Probability of Meeting Target"))
#                        ),
#                 splitLayout(cellWidths = c("50%", "50%"),
#                             verbatimTextOutput("Txt_Yield_cg"),
#                             verbatimTextOutput("Txt_Yield_pb_cg")
#                    )
        ), #End tab Panel
                 
#------------------ Expected Mean Recruit and Yields ---------------------------   
        tabPanel("Expected Mean & Annual Yield",
          plotOutput(height = '300px', 'Plt_yield.cg'),      
          plotOutput(height='300px',"EGR.Yield"),
            splitLayout(cellWidths = c("50%", "50%"),
              p(strong("Mean and Annual Yields Summary")),
              p(strong("Probability of Meeting Target"))),
            splitLayout(cellWidths = c("50%", "50%"),
              verbatimTextOutput("Txt_Yield_cg"),
              verbatimTextOutput("Txt_Yield_pb_cg"))
          ), #End tab Panel
#------------------ Recruit Annual Recruit and Yields --------------------------   
        tabPanel("Expected Mean & Annual Recruit",
          plotOutput(height = '300px', 'Plt_rec.cg'),         
          plotOutput(height='300px',"EGR.Rec"),
            splitLayout(cellWidths = c("50%", "50%"),
              p(strong("Mean and Annual Recruit Summary")),
              p(strong("Probability of Meeting Target"))),
            splitLayout(cellWidths = c("50%", "50%"),
              verbatimTextOutput("Txt_Rec_cg"),
              verbatimTextOutput("Txt_Rec_pb_cg"))
          ), #End tab Panel
    tabPanel("Help",
          )# End tabPanel
        )#End tabsetPanel
      )#End main Panel 
    )
#    ,#End tabPanel Custom Escapement Goal Evaluation
    ),#End navbarMenu
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 4  Management Strategy Evaluation 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
navbarMenu("MSE Analyses",
#-------------------------------------------------------------------------------    
#  Simulation model UI
#-------------------------------------------------------------------------------  
 tabPanel("Simulation Model",
  sidebarPanel(width = 3,        
   conditionalPanel(condition="input.MSEPanel == 'Simulation Run'",           
    p(strong("Escapement Goal")), 
    fluidRow(
      column(6,uiOutput('LEG')),  
      column(6,uiOutput('UEG'))
    ),
    p(strong("Fishery Harvest")),  
    fluidRow(
      column(6,uiOutput('minH')),  
      column(6,uiOutput('maxH')),
    ),
    p(strong("Management Option")),  
    fluidRow(
      column(6,
             selectInput(inputId="strType","Priority", 
                         choices = c('Escapement','Harvest','Hybrid'),selected = 'Escapement')
             ),
        column(6,
        selectInput(inputId="cmode","Target ESC", 
          choices = c('Lower','Middle','Upper'),selected = 'Middle')
             ),  

          ),
    uiOutput('nsim'),   
#  selectInput(inputId="cmode","Target Escapement", 
#               choices = c('Lower','Middle','Upper'),
#               selected = 'Middle'),              
#  selectInput(inputId="strType","Management Priority", 
#                choices = c('Escapement','Harvest','Hybrid'),
#                selected = 'Escapement'),
    actionButton("SimRun","Simulate"),
    actionButton("show", "Show modal dialog"),   
#    sliderInput(inputId="maxHr", "Maximum Surplus Harvest Rate", value=0.9,min=0,max=1,step=0.1),
#    ),
#    conditionalPanel(condition="input.strType=='Harvest",
                     
#                     ),
    p(strong("Management Error")),       
    sliderInput(inputId="spred", "Run Assessment Error", value=15,min=0,max=100,step=5),
    sliderInput(inputId="simpH", "Management Error", value=10,min=0,max=100,step=5),
    #    sliderInput(inputId="sobsH", "Harvest Observation %E", value=10,min=0,max=100,step=5),
    #    sliderInput(inputId="sobsE", "Escapement Observation %E", value=30,min=0,max=100,step=5),
    sliderInput(inputId="simy", "Management", value=50,min=0,max=100,step=5,round=0)
      ),
   conditionalPanel(condition="input.MSEPanel == 'Sim Summary'",
      )              
      ), #End sidebarPanel
                    
#---------------- mainPanel ----------------------------------------------------
  mainPanel(
    tabsetPanel(
#-------------------------------------------------------------------------------    
#  Simulation Run 
#-------------------------------------------------------------------------------        
      tabPanel("Simulation Run",
        fluidRow(  
        plotOutput(height='500px',"simplot")  
           ),
        fluidRow( 
          #          column(3, actionButton("InitRun","Initialize")),
#          column(3, actionButton("SimRun","Simulate")),
#          column(3,actionButton("SimClear","Clear Results"))
                  ),
        fluidRow(  
#          plotOutput(height='500px',"runsim"),
          verbatimTextOutput("Txt_sum.mse"),
          plotOutput("Plt_sum.mse")
                  )
                  ), #End tabPanel
        tabPanel("Sim Summary",
          plotOutput('Plt_freq_mse'),
          verbatimTextOutput("Txt_HE_mse")
                  ), #End tabPanel
                        
        tabPanel("Sim time series",
#          plotOutput(height='600px',"altsim.N"), 
#          downloadButton("simdownload", "Download")
                  ),  #End tabPanel
                        
#------------------ Model Parameters ---------------------------------   
        tabPanel("Model Parameters",
          fluidPage(
            title = 'Set MSE Simulaiton Initization Parameters',
            hr(),
            fluidRow( 
              column(4,
                p(strong("Simulation Years")),           
#                sliderInput(inputId="burnin", "Burnin", value=25,min=0,max=100,step=5,round=0),
#                sliderInput(inputId="train", "Training", value=25,min=0,max=100,step=5,round=0),
#                sliderInput(inputId="simy", "Management", value=50,min=0,max=100,step=5,round=0)
                  ),
              column(4,
                p(strong("Errors")),       
#                sliderInput(inputId="spred", "Preseason Run prediction %E", value=20,min=0,max=100,step=5),
#                sliderInput(inputId="simpH", "Management Implementation %E", value=10,min=0,max=100,step=5),
#                sliderInput(inputId="sobsH", "Harvest Observation %E", value=10,min=0,max=100,step=5),
#                sliderInput(inputId="sobsE", "Escapement Observation %E", value=30,min=0,max=100,step=5),
#                sliderInput(inputId="Nobage", "Age Comp Sample size", value=100,min=10,max=500,step=10)
                   ),
              column(4,
                p(strong("Population Errors")), 
#              sliderInput(inputId="phi", "AR1 correlation", value=0.6,min=0,max=1,step=.1),
#              sliderInput(inputId="D", "Drishelet D", value=50,min=0,max=200,step=10)
                  )
              ) # End fluidRow
            )# End fluidOPage
          ), # End tabPanel
         id = "MSEPanel"
        )#End tabsetPanel
      )#End mainPanel
    ),#End tabPanel Simulation Model 
           
#------------------ Model Descriptions -----------------------------------------   
      tabPanel("Help",
        tabsetPanel(
#------------------ Model Structure  -------------------------------------------             
              tabPanel("Model description",
                       includeMarkdown("documents/MSE_help.md")
                      ),  #End tabPanel
#------------------ Parameters Description ---------------------------------             
              tabPanel("Base model Parameters",
                    h3("Simulation length"),
                    p("- Burnin: Years to make modle stabilize "),
                    p("- Training: Years SR data are collected befre setting active management"),
                    p("- Managmenet: Years active mangement is conducted"),
                    h3("Management Errors"),
                    p("Fishery management takes following steps: 1) predict preseason run size, 
              2) determin harves target, 3) execute harvests, and 
              4) observe harvest and escapetnt to set and escapement goal. The model incorporates errors
                  associated with each step.  Errors were modeled as independent log-normal"),
                    p("- Preseaon Run prediction: Accuracy +/-  x%"),
                    p("- Management Imprementation: Accuracy +/-  x%"),
                    p("- Harvest Observation: Accuracy +/-  x%"),
                    p("- Escapement Observation: Accuracy +/-  x%"),
                    p("Observed postseason run size is Observed harvest + escapement"),
                    h3("Poplation Dynamic"),
                    p("- AR1 corelation: Recuruitment error was modeled as AR1 with sigma and phi (correlation),
              sigma is derived from SR model fit."),
                    p("- Drishelet D. Brood age proportion is modeled as Drishelet distrribution
              D determines level of variation. Lower D indicate higher variation")
                      ) # End tabPanel
                    )# End tabsetPanel
           )#End tabPanel Model Description
      ),#End navMenue Panel

tabPanel("Report Output",   
  sidebarPanel(
#    helpText(),
    radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                 inline = TRUE),
    downloadButton('downloadReport')
  ),
  mainPanel(
#    plotOutput('regPlot')
  )
)  
  
 ),#End nabVarPage
#-------------------------------------------------------------------------------
# Citation Disclaimer  
#-------------------------------------------------------------------------------
hr(),
h5("Disclaimer"),
print(strong("This App is developed by Toshihide Hamachan Hamazaki, Alaska Department of Fish and Game Division of Commercial Fisheries")),
h5("Contact about this applicaiton"), 
print(strong("Questions and improvement suggestions? Please contact",
a(href="mailto:toshihide.hamazaki@alaska.gov", "Hamachan"))),
h5("Suggested Citation"),
print(strong(paste("Hamazaki, T.",format(Sys.Date(), "%Y"),". Pacific salmon escapement goal analyses (source: https://hamachan.shinyapps.io/Spawner_Recruit_Bayes/)"))),
h5("Other Models"),
p(strong("Missed escapement passage estimation:",
         a(href="https://hamachan.shinyapps.io/Missed_Run/", "Missed Passage")))
) #End fluidPage

#===============================================================================    
#  Server:  
#===============================================================================
server<-shinyServer(function(input, output, session){
# dataType reading mould 
#  dataType <- callModule(dataType,"datatype") 
#  bs_themer()
output$note <- renderUI({
    if(input$dataType== "S-R"){
      paste("S-R Data file column orders: Year, Spawner (Escapement), Recruit")
    } else if(input$dataType== "Run")  {
      paste("Run Data file column orders: Year, Escapement, Run,
                   Run by age (or proportion) from youngest to oldest")
    } else {
      paste("Escapement only Data file column orders: Year, Escapement")
    } 
  })

#-------------------------------------------------------------------------------
#  Tab Control  
#-------------------------------------------------------------------------------
observe({
  if(input$dataType=="Run") {
    showTab(inputId = "tabs", target = "Escapement Only Analyses")
    showTab(inputId = "tabs", target = "Escapement Goal Analyses")
    showTab(inputId = "tabs", target = "MSE Analyses")
    showTab(inputId = "tabs", target = "SR Model")
    showTab(inputId = "subTab", target = "Brood Table")
  } else if (input$dataType== "S-R") {
    hideTab(inputId = "tabs", target = "Escapement Only Analyses")
    showTab(inputId = "tabs", target = "Escapement Goal Analyses")
    hideTab(inputId = "tabs", target = "MSE Analyses")
    showTab(inputId = "tabs", target = "SR Model")
    hideTab(inputId = "subTab", target = "Brood Table")
  } else {
    showTab(inputId = "tabs", target = "Escapement Only Analyses")
    hideTab(inputId = "tabs", target = "Escapement Goal Analyses")
    hideTab(inputId = "tabs", target = "MSE Analyses")
    hideTab(inputId = "tabs", target = "SR Model")
    hideTab(inputId = "subTab", target = "Brood Table")
  }

})  

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Data upload and output 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data file reading module  
   data <- dataInputServer("datain")
# table ---- Uploaded data table output ----------------------------------------
  output$Tbl_data <- renderDataTable({data()})  

# brood.table--- Construct brood table (when dataType is "Run") ----------------   
  brood.out <-  reactive({
    if(input$dataType== "Run"){
      brood <- make.brood(data(),input$fage)
      return(brood)
    } else{NA}
  })
  
# Tbl_data.brood ----- show brood table ----------------------------------------
output$Tbl_data.brood <- renderDataTable({
    if(input$dataType== "Run"){
    round(brood.out()$brood,0)
    }  
  }) 
  
# brood age comp----------------------------------------------------------------
  brood.p <- reactive({
     brood <- brood.out()$brood
     p.brood <- brood[complete.cases(brood),]
     ncol <- dim(p.brood)[2]
     pbrood <- p.brood[,-c(1,2,ncol)]/p.brood$Recruit
     return(pbrood)
    })
   
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Create SR data 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Original SR data -------------------------------------------------------------  
sr.data.0 <- reactive({
  req(input$dataType)
    if(input$dataType== "Run"){
      x <- brood.out()$SR
    } else if (input$dataType== "S-R"){
      x <- data()
      x <- x[complete.cases(x),]
    }
    names(x) <- c('Yr','S','R')
    return(x)   
  })

   
# Original Escapement data -----------------------------------------------------  
e.data.0 <- reactive({
      x <- data()[,c(1:2)]
    names(x) <- c('Yr','S')
    return(x)     
  })  

# yrange --- UI output to determine data year range -----------------------------
output$yrange <- renderUI({
  if(input$dataType== "Escapement Only"){
    name <- 'Run'
    year <- e.data.0()$Yr    # Extract brood year data range 
  }else{
    name <- 'Brood'
    year <- sr.data.0()$Yr   # Extract brood year data range     
  }
    fyear <- min(year)       # First brood year 
    lyear <- max(year)       # Last brood year
    #  Slider input UI 
    sliderInput("sryears", label = paste("Select",name,"Year Range"), min = fyear, max = lyear, value = c(fyear, lyear),step=1,sep = "")
  })
  
# sr.data --- final dataset used for SR analyses -------------------------------
  sr.data <- reactive({ cut.data(sr.data.0(),input$sryears) })

# e.data --- final dataset used for percentile risk -----------------------------
  e.data <- reactive({ cut.data(e.data.0(),input$sryears) })

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Plot SR data 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
plot_runesc <- function(dat,u){
     par(yaxs='i',bty='l')
     plot(R/u~Yr,data=dat,type='l',ylim=c(0,with(dat,max(R,S)/u)),xlab='',ylab='')
     lines(S/u~Yr,data=dat,lty=2)
   }
# Plt_runesc --- Plot Run-Escapement Time series (when data is "Run") --------------
output$Plt_runesc <- renderPlot({
  if(input$dataType== "Run"){
  x <- data()[,c(1:3)]
  names(x) <-c('Yr','S','R')
  u <- as.numeric(input$ui)
  multi <- mult(u)  
  plot_runesc(x,u)
# Calculate Harvest rate 
  x$hrate <- with(x,(R-S)/R)
  par(new = TRUE)
  plot(hrate~Yr, data=x,type = "l", xaxt = "n",yaxt = "n",xlab='',ylab='',ylim=c(0,1),col=2)
  axis(side = 4)
  mtext("Harvest rate",side=4,line=2)
  legend('topright',c('Run','Escapement','Harvest rate'),lty=c(1,2,1),
         col=c(1,1,2), box.lty=0)  
  title("Run and Escapement", xlab="Year",
        ylab=paste('Run / Escapement',multi))  
  }
 })

# Plt_agecompr --- Plot Rum Age comp Time series (when data is "Run") ----------
output$Plt_agecompr <- renderPlot({
  if(input$dataType== "Run"){
    x <- data()
    names(x) <-c('Yr','S','R')
    u <- as.numeric(input$ui)
    multi <- mult(u)  
    plot_runesc(x,u)
    legend('topright',c('Run','Escapement'),lty=c(1,2),box.lty=0)  
    title("Run and Escapement", xlab="Year",
          ylab=paste('Run / Escapement',multi))  
  }
})

# Plt_agecompb --- Plot Brood Age comp Time series (when data is "Run") --------
output$Plt_agecompr <- renderPlot({
  if(input$dataType== "Run"){
    x <- data()
    names(x) <-c('Yr','S','R')
    u <- as.numeric(input$ui)
    multi <- mult(u)  
    plot_runesc(x,u)
    legend('topright',c('Run','Escapement'),lty=c(1,2),box.lty=0)  
    title("Run and Escapement", xlab="Year",
          ylab=paste('Run / Escapement',multi))  
  }
})

# Plt_srt ---------- Plot SR time series ---------------------------------------
output$Plt_srt <- renderPlot({
  if(input$dataType != "Escapement Only"){
  x <- sr.data.0()
  u <- as.numeric(input$ui)
  multi <- mult(u)
  par(yaxs='i',bty='u',mar=c(4,4,3,3))
  plot(R/u~Yr,data=x,type='l',ylim=c(0,with(x,max(R,S)/u)),xlab='',ylab='')
  lines(S/u~Yr,data=x,lty=2)
  par(new = TRUE)
  plot(log(R/S)~Yr, data=x,type = "l", xaxt = "n",yaxt = "n",xlab='',ylab='',col=4)
  axis(side = 4)
  mtext("ln(R/S)",side=4,line=2)
  legend('topright',c('Spawner','Recruit','ln(R/S)'),lty=c(2,1,1),col=c(1,1,4),box.lty=0)  
  title("Spawner and Recruit", xlab="Brood Year",
        ylab=paste('Spawner / Recruit',multi)) 
  } else {
    x <- e.data.0()
    u <- as.numeric(input$ui)
    multi <- mult(u)  
    par(yaxs='i',bty='l')
    plot(S/u~Yr,data=x,type='l',ylim=c(0,with(x,max(S)/u)),xlab='',ylab='')
    title("Escapement", xlab="Year",
          ylab=paste('Escapement',multi)) 
   }
  # Add Cutting data 
  if(max(input$sryears)<max(x$Yr)|min(input$sryears)>min(x$Yr)){
  abline(v=input$sryears,col=2)
  }
})

# Plt_lnsrt ---------- Plot SR time series -------------------------------------
output$Plt_lnsrt <- renderPlot({
  x <- sr.data.0()
  u <- as.numeric(input$ui)
  multi <- mult(u)
  plot_runesc(x,u)
  legend('topright',c('Spawner','Recruit'),lty=c(2,1),box.lty=0)  
  title("Spawner and Recruit", xlab="Brood Year",
        ylab=paste('Spawner / Recruit',multi))  
  # Add Escapement Goal range  
  if(max(input$sryears)<max(x$Yr)|min(input$sryears)>min(x$Yr)){
    abline(v=input$sryears,col=2)
  }
})


# Plt_srtime ---------- Plot SR time series ------------------------------------
output$Plt_srtime <- renderPlot({
  x <- sr.data.0()
  RS <- with(x,log(R/S))
  names(RS) <- x$Yr
  stars <- stars(RS, L=10, p=0.05,h=2, AR1red="est", prewhitening = F)  
  test <- as.data.frame(stars$starsResult)
  par(yaxs='i',bty='l')
  plot(log(R/S)~Yr,data=x,type='l',xlab='',ylab='')
  lines(x$Yr,test$mean,col=4,lwd=2)
  legend('topright',c('R/S'),lty=c(2,1),box.lty=0)  
  title("ln(Recruit/Spawner)", xlab="Brood Year",
        ylab=paste('ln(Spawner/Recruit)'))  
  # Add Escapement Goal range  
  if(max(input$sryears)<max(x$Yr)|min(input$sryears)>min(x$Yr)){
    abline(v=input$sryears,col=2)
  }
})


# summary -------- sr data summary Output --------------------------------------
output$Txt_sum.data <- renderPrint({
  if(input$dataType != "Escapement Only"){
  dat <- sr.data()
  dat$Y <- dat$R-dat$S
  }else{
  dat <- e.data()    
  }
  summary(dat[,-1])  # Remove year 
  })

# Plt_hist.sry ---------- sr data histgranm ------------------------------------------- 
output$Plt_hist.sry <- renderPlot({
  if(input$dataType != "Escapement Only"){
  par(mfrow=c(1,3))
  x <- sr.data()
  x$Y <- x$R-x$S
  hist(x$S,main='',xlab='Spawnter')
  hist(x$R,,main='',xlab='Recruit')
  hist(x$Y,,main='',xlab='Yield') 
  } else {
    x <- e.data()  
    hist(x$S,main='',xlab='Escapement')    
  }
  })

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 2: Bayesian Model:  Create JAG data and model   
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Bayesedata --- Create dataset for Bayesian modeling --------------------------
Bayesdata <- reactive({
  #  Import SR data 
  x <- sr.data()
  # nyrs is the number of brood years (i.e. number of rows) 
  nyrs <- dim(x)[1]
  R <- x$R
  S <- x$S
  # d is S multiplier
  d <- floor(log10(mean(S)))
  #  ar1: 1 if ar1 is included, 0 if not 
  if(input$add =='ar1'){ar1 <- 1} else {ar1 <- 0}
  if(input$add =='kf'){kf <- 1} else {kf <- 0}
  out <-list(nyrs=nyrs, S=S, R=R,d=d,ar1=ar1,kf=kf)
  return(out)
  })

# Select Bayes model 
Bayesmodel <- reactive({model_select(input$Model,input$add)})
# Show model code
output$modelcode <- renderPrint({ Bayesmodel() })

#===============================================================================
#  Run JAG Model Module
#===============================================================================
#  Bayesmodel  Model section for JAG Models 
#  The function outputs: 
#  jagmodel: Selected JAG model
#  parameters: Model output parameters
#  model:  SR model used for simulation 
#===============================================================================
# Run Bayesian Model 
sim <- BayesInputServer('Bayes', Bayesdata, Bayesmodel)

# Model output 
run.JAGS <- reactive({sim()})

#-------------------------------------------------------------------------------
#  Extract JAG results 
#-------------------------------------------------------------------------------
# BayesSum ------------ Output MCMC sumaary ------------------------------------
output$BayesSum <- renderPrint({
  sim_sum <- print(run.JAGS())
  print(sim_sum$sum)
  })

# Plt_trace --------- Trace and density plot -----------------------------------
output$Plt_trace <- renderPlot({
  mcmc <- as.mcmc(run.JAGS())
  pars <- c('lnalpha','beta')
  if(input$add=='ar1') {pars <- c(pars,'phi')}
  par(mfrow=c(2,4))
  plot(mcmc[,pars],auto.layout=FALSE)
  })
#===============================================================================
#===============================================================================

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 3: SR Model Analyses   
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# SR.post --- Remove outliers
# Create SR parameters: alpha, beta, Seq, Smsy, Umsy, Smax ---------------------

SR.post <- reactive({
  D <- as.numeric(Bayesdata()$d)
# Read mcmc data
  mcmc <- as.matrix(as.mcmc(run.JAGS()))
  post <- sim.out(mcmc,d=D,add=input$add,model=input$Model,model.br=Bayesmodel()$model.br)
  return(post)
  })

sum.fun <- function(x,ci){
    p <- (100-ci)/200
    min <- sapply(x,min, na.rm=TRUE)
    max <- sapply(x,max, na.rm=TRUE)
    lci <- sapply(x,quantile, prob=p, na.rm=TRUE)
    uci <- sapply(x,quantile, prob=1-p, na.rm=TRUE)
    mean <- sapply(x,mean, na.rm=TRUE)
    median <- sapply(x,median, na.rm=TRUE)
    out <- rbind(min,lci,mean,median,uci,max)
    rownames(out) <- c('Min',paste0(100*p,'%'),'Mean',
                       'Median',paste0(100*(1-p),'%'),'Max')
    return(out)
 }

# sumpost --- SR Parameter Summaries output------------------------------------- 
output$sumpost <- renderPrint({
  ci <- input$CIB
  parname <-c('alpha','lnalpha','beta','Seq','Smsy','Umsy','Smax')
  t(round(t(sum.fun(SR.post()[,parname],ci)),c(3,3,3,0,0,3,0)))
  })

# Plt_hist.mc --- SR Parameters Density plots ----------------------------------
output$Plt_hist.mc <- renderPlot({
  D <- Bayesdata()$d
  if(input$add=='ar1'){ar1<- TRUE} else {ar1<- FALSE}
  plot_density(SR.post(),D,ar1,model=input$Model)
  })

#===============================================================================
#  Time Variant allpha Analyses  input$add == 'kf'
#===============================================================================
# lnalphais  Extract Time_variant alpha-----------------------------------------
lnalphais <-reactive({
 if(input$add=='kf'){
  nyrs <- Bayesdata()$nyrs
  year <- sr.data()$Yr
# Bayesian simulation out parameters -------------------------------------------    
  parname <- paste0('lnalphai[',1:nyrs,']')
# Extract lnalphai from posterior   
  lnalphai <- SR.post()[,parname]
# Mean lnalphai  
  lnalphai.m <- apply(lnalphai,2,mean)
# Get 95% CI  
  cil <- apply(lnalphai,2,function(x) quantile(x, 0.025))
  ciu <- apply(lnalphai,2,function(x) quantile(x, 0.975))
# Calculate STARS uisng STARS function 
   names(lnalphai.m) <- year
   stars <- stars(lnalphai.m, L=10, p=0.05,  h=2, AR1red="est", prewhitening = F)  
   test <- as.data.frame(stars$starsResult)
# Combine data 
   lnalphai.m <- data.frame(cbind(year,lnalphai.m,cil,ciu,test$mean))
   names(lnalphai.m) <- c('year','lnalphai.m','cil','ciu','star')
# Get unique year
    miny <- aggregate(year~star,data=lnalphai.m,min)
# order by small to large  
    miny <- miny[order(miny$year),]
# replace first one to first year
    miny$year[1] <- min(year)
    maxy <- aggregate(year~star,data=lnalphai.m,max)
# merge miny and maxy   
    cuty <- merge(miny,maxy,by='star')
    names(cuty) <- c('star','miny','maxy')
# Order from first year to last year    
    cuty <- cuty[order(cuty$miny),] 
# Add text range 
    cuty$txt <- paste0(cuty$miny,'-',cuty$maxy)
# Add number of years for each period
    cuty$ny <- cuty$maxy-cuty$miny+1
# Add cum  years for each period
    cuty$cy <- cumsum(cuty$ny)
# Add cum  years begin 
    cuty$cby <- cuty$cy-cuty$ny+1
    out <- list(lnalphai=lnalphai.m, stars=stars,cuty=cuty)
      } 
    return(out)
  })

output$Plt_lnalphai <- renderPlot({
  if(input$add=='kf'){
#  nyrs <- Bayesdata()$nyrs
#  year <- sr.data()$Yr
# Select parameter name: time variant alpha name is lnalphai[1],lnalphai[2],
# ... ,lnalpha[ny]  
#  parname <- paste0('lnalphai[',1:nyrs,']')
# Extract lnalphai  
#  lnalphai <- SR.post()[,parname]
# Extract mean lnalpha for each year 
  xa <- lnalphais()$lnalphai
#  lnalphai.m <- xa$lnalphai.m
# Mean overall lnalpha  
  lnalpha <- mean(xa$lnalphai.m)
# Calculate STARS
# Plot figures   
  par(bty='l')
  with(xa,plot(lnalphai.m~year,type='l',lwd=2,main='time-varying lnalpha',
       xlab='Year',ylab='lnalpha',ylim=c(min(cil),max(ciu))))
  with(xa,polygon(c(year,rev(year)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA)) 
  abline(h=lnalpha,lwd=2,col=2)
  lines(star~year,data=xa,col=4,lwd=2)
  } 
 })

# yrange --- UI output to determine data year range -----------------------------
output$astar <- renderUI({
 if(input$add=='kf'){
# Extract lnalhpha Star 
 cuty <- lnalphais()$cuty
 radioButtons(inputId="alphai","Time Variant alpha Select Periods",
               choices=c("None",cuty$txt),
               selected = NULL)
  }
 })

# SR.pred.i ---- Time variant Bayesian Model Prediction ------------------------
SR.post.i <- reactive({
# Read mcmc data
  if(input$add=='kf' & input$alphai != 'None'){
    post <- SR.post()
    cuty <- lnalphais()$cuty
    period <- cuty[cuty$txt==input$alphai,c('cby','cy')] 
    parname <- paste0('lnalphai[',period$cby:period$cy,']')
    # Extract lnalphai  
    lnalphai <- as.matrix(SR.post()[,parname])
    post$lnalpha <- apply(lnalphai,1, FUN=mean, na.rm=TRUE)
    return(post)
    }
})

#===============================================================================
#  Create SR Model Predictions    
#===============================================================================
# SR.resid ----- Model Residuals -----------------------------------------------
SR.resid <-reactive({
#---------- Select SR Model ----------------------------------------------------  
  srmodel <- Bayesmodel()$model
#---------- Extract MCMC SR Model Parameters -----------------------------------
  lnalpha <-SR.post()$lnalpha
  beta <- SR.post()$beta
  phi <- SR.post()$phi
  S <- Bayesdata()$S
  D <- Bayesdata()$d
  R <- Bayesdata()$R
  nyrs <- Bayesdata()$nyrs
#---------  Set up empty matrix ------------------------------------------------  
  ncol <- length(S) 
  nrow <- length(lnalpha)  #  Extract number of MCMC sample 
# Create Residuals  MCMC matrix    
  RD <- matrix(NA,nrow=nrow,ncol=ncol) 
  RD2 <- matrix(NA,nrow=nrow,ncol=ncol) 

#-------- Time variant alpha residual ------------------------------------------
if(input$add=='kf'){
 parname <- paste0('lnalphai[',1:nyrs,']')
    # Extract lnalphai  
  lnalphai <- as.matrix(SR.post()[,parname])
  for(i in 1:nrow){
      # Calculated expected Returns form each MCMC SR model parameters   
      Ey <- srmodel(lnalphai[i,],beta[i],S,D)
      RD[i,] <- log(R)-log(Ey) 
     }  
    } # End if 
#------ Others -----------------------------------------------------------------
       else {
  for(i in 1:nrow){
    # Calculated expected Returns form each MCMC SR model parameters   
    Ey <- srmodel(lnalpha[i],beta[i],S,D)
    RD[i,] <- log(R)-log(Ey)
         }  
       }
#------ Residuals for AR1 Remove AR1 correlation -------------------------------
  if(input$add=='ar1'){
      for(i in 1:nrow){
        RD2[i,2:ncol] <- RD[i,2:ncol] - phi[i]*RD[i,1:(ncol-1)]
        RD2[i,1] <- RD[i,1]-phi[i]*SR.post()$e0[i]
         }  
      } # End else
# Create residuals -----------------------------------------------------------  
  out <- list(RD=RD,RD2=RD2)
  return(out)
  })  

# SR.pred ---- Bayesian Model Prediction ---------------------------------------
SR.pred <-reactive({
  srmodel <- Bayesmodel()$model
#---------- Extract MCMC SR Model Parameters -----------------------------------
  D <- Bayesdata()$d
#---------- Determine model S length -------------------------------------------
  Seq <- quantile(SR.post()$Seq,0.9)   # Extract 90 percentile Seq
  max.s <- max(Seq,max(sr.data.0()$S)) # Extract max spawner 
  out <- SR.pred.sim(SR.post(),D,max.s,srmodel)    
  if(input$add=='kf')
  {
  if(input$alphai == 'None') {
    out <- SR.pred.sim(SR.post(),D,max.s,srmodel) 
  }
   else{
  out <- SR.pred.sim(SR.post.i(),D,max.s,srmodel)  
   } 
  }
  return(out)
 })  
output$temp <- renderDataTable({head(as.data.frame(SR.pred()$R))})

# SRp ------ Model predicted mean, CI, PI  SR range ----------------------------
SRp <- reactive({
       pred_CI(SR.pred(),input$CI)
  })  


# downloadData ---- Results download -------------------------------------------
output$downloadData <- downloadHandler(
  filename = function() {
    "downloaddata.xlsx"
  },
  content = function(file) {
    S <-as.vector(SR.pred()$S)
    R <- as.matrix(SR.pred()$R)
    write.xlsx(rbind(S,R), file)
  }
)

#===============================================================================
#  Standard SR and Yield Plots and Tables Outputs  
#=============================================================================== 
base.pl <- reactive({
  u <- as.numeric(input$ui)
  x <- sr.data.0()
  xp <- x/u
  SRp <- SRp()/u
#---  Basic SR plot ------------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l')
  plot(R~S,data=xp,pch=19,col='gray',cex=1.5, 
       xlab=paste("Escapement",mult(u)),ylab=paste('Recruit',mult(u)),
       xlim=c(0,max(SRp$S)),ylim=c(0,1.1*max(xp$R)))
  x2 <- sr.data()
  xp2 <- x2/u
  points(R~S,data=xp2,pch=19,col=1,cex=1.5)
  # Plot 1:1 line 
  abline(0,1)
  # Add Predicted   
  lines(RS.md~S,data=SRp,col=1,lw=2,lty=2)
  lines(RS.me~S,data=SRp,col=1,lw=2)
  out1 <-recordPlot()
#-------------------------------------------------------------------------------
#---  Basic Yield plot ---------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l')
  plot((R-S)~S,data=xp,pch=19,col='gray', cex=1.5,
       xlab=paste("Escapement",mult(u)),ylab=paste('Yield',mult(u)),
       xlim=c(0,max(SRp$S)),ylim=c(min(SRp$Rl-SRp$S),1.1*max(xp$R-xp$S)))
  points((R-S)~S,data=xp2,pch=19,col=1,cex=1.5)
  lines((RS.md-S)~S,data=SRp,col=1,lw=2,lty=2)
  lines((RS.me-S)~S,data=SRp,col=1,lw=2)
  abline(h=0)
#-------------------------------------------------------------------------------
#----  Plots Output ------------------------------------------------------------  
  out2 <-recordPlot()  
  return(list(base.p=out1,base.py=out2))
})


base.p <- reactive({base.pl()$base.p})

base.py <- reactive({base.pl()$base.py})

#===============================================================================
#  Standard SR and Yield Plots with CI-PI
#=============================================================================== 
# srplot.g : plot goal range in SR plot-----------------------------------------
base.sr <- reactive({
  u <- as.numeric(input$ui)
  SRp <- SRp()/u
# Plot base recruit Plot  
  replayPlot(base.p())
  # SR CI range 
  with(SRp,polygon(c(S,rev(S)),c(Ru,rev(Rl)),col=tcol('grey',50),border=NA))
  # SR PI range 
  with(SRp,lines(S,Ru.p,lty=2,col='grey'))
  with(SRp,lines(S,Rl.p,lty=2,col='grey'))
  out1 <-recordPlot()
# Plot base Yield Plot
  replayPlot(base.py())
  with(SRp,polygon(c(S,rev(S)),c(Ru-S,rev(Rl-S)),col=tcol('grey',50),border=NA))
  with(SRp,lines(S,Ru.p-S,lty=2,col='grey'))
  with(SRp,lines(S,Rl.p-S,lty=2,col='grey'))  
  out2 <-recordPlot()
  return(list(base.r=out1,base.y=out2))  
})

base.r <- reactive({base.sr()$base.r})

base.y <- reactive({base.sr()$base.y})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 4: SR Model Analyses plots   
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# srplot ------- SR plot function ----------------------------------------------
srplot <- reactive({
  u <- as.numeric(input$ui)
  SRp <- SRp()/u
  xp <- sr.data.0()
  dyear <- floor(xp$Yr/10)*10
  colp <- (dyear-min(dyear))/10+1
    # Draw Base SR Plot
  replayPlot(base.p())
  add <- ifelse(input$add=="ar1","AR(1) Error",
                ifelse(input$add=="kf","Time varying alpha",""))
  texmodel <- paste0('SR Model: ',input$Model,' ',add)    
  legend('top',legend=texmodel,box.lty=0)
  
#-------- Time Varying Alpha ---------------------------------------------------  
  if(input$add=='kf'){
    srmodel <- Bayesmodel()$model
    model.br <- Bayesmodel()$model.br
#---------- Extract MCMC SR Model Parameters -----------------------------------
    star <- lnalphais()$cuty
#    star <- data.frame(yr = sr.data()$Yr,star=xa$star)
    beta <- mean(SR.post()$beta)
    S <- SRp()$S
    D <- Bayesdata()$d
# Get unique alpha 
    alpha.star <- star$star
    br <- model.br(alpha.star,beta,D)
    nstar <- length(alpha.star)
    for (i in 1:nstar){
     R <- srmodel(alpha.star[i],beta,S,D)
     lines(S/u,R/u,lty=2,lwd=2,col=1+i)
     }
    tex <- star$txt 
    if(input$show.smsy==FALSE){
    legend('right',col=c(1:nstar+1),lwd=3,lty=2,legend=tex,box.lty=0)
    }
    }
  
  
  # Confidence Interval 
  if (input$Li =='confidence') {
    lwr <- SRp$Rl
    upr <- SRp$Ru
    }
  else {
    # Prediction Interval
    lwr <- SRp$Rl.p
    upr <- SRp$Ru.p
   }
  
  # Add CI    
  if(input$show.int==TRUE){
      polygon(c(SRp$S,rev(SRp$S)),c(upr,rev(lwr)),col=tcol('grey',50),border=NA)
    }
  # Add Years
  if(input$show.points==TRUE) {
    points(xp$S/u,xp$R/u,pch=19,cex=1.5,col=colp)
    pointLabel(xp$S/u,xp$R/u, labels=as.character(xp$Yr), cex= 1)
    legend('topleft',col=unique(colp),legend=unique(dyear), pt.cex = 1.2,
           pch=19,box.lty=0)
    }
  # Add Smsy
  t1 <- ''
  l1 <- 0
  if(input$show.smsy==TRUE) {
    if(input$add=='kf'){
      abline(v=c(median(SR.post()$Smsy),br$Smsy)/u,col=c(1:(nstar+1)),lty=2) 
      t1 <- paste('Smsy:',c('Mean',star$txt),paste(round(c(median(SR.post()$Smsy),br$Smsy)/u,0)))
      } else {
    abline(v=median(SR.post()$Smsy)/u,col=1,lty=2)
    t1 <- paste('Smsy:',round(median(SR.post()$Smsy)/u,0))
      }
    l1 <- 2
    }
  # Add Smax       
  t2 <- ''
  l2 <- 0
  if(input$show.smax==TRUE & input$Model == 'Ricker') {
    abline(v=median(SR.post()$Smax)/u,col=1,lty=3)
    t2 <- paste('Smax:',round(median(SR.post()$Smax/u),0))
    l2 <- 3
    }
  legend('topright',c(t1,t2),lty=c(rep(l1,length(t1)),l2),
         lwd=2, col=c(1:(length(t1)),1),box.lty=0)  
  out <-recordPlot()
  return(out)  
  })


# Plt_SR ------ SR plot -------------------------------------------------------
output$Plt_SR <- renderPlot({srplot()})

# down --------- SR plot download ----------------------------------------------
#output$down <- downloadHandler(
#  filename = function() {
#    paste("myreport","png", sep = ".")
    #    paste("myreport", input$report, sep = ".")
#    },
#  content = function(file){
        #    if(input$report == "png")
#    png(file,width = 900, height = 900,antialias = "cleartype" )
    #    else
    #      pdf(file)
##    (srplot())
#    dev.off()
#    }
#  )

# Plt_yield -------- Yield plot ----------------------------------------------- 
output$Plt_yield <- renderPlot({
  u <- as.numeric(input$ui)
  SRp <- SRp()/u
  xp <- sr.data.0()
  dyear <- floor(xp$Yr/10)*10
  colp <- (dyear-min(dyear))/10+1
  # Plot base Yield plot
  replayPlot(base.py())
  add <- ifelse(input$add=="ar1","AR(1) Error",
                ifelse(input$add=="kf","Time varying alpha",""))
  texmodel <- paste0('SR Model: ',input$Model,' ',add)    
  legend('top',legend=texmodel,box.lty=0)
#-------- Time Varying Alpha ---------------------------------------------------  
  if(input$add=='kf'){
    srmodel <- Bayesmodel()$model
    model.br <- Bayesmodel()$model.br
#---------- Extract MCMC SR Model Parameters -----------------------------------
    star <- lnalphais()$cuty
    beta <- mean(SR.post()$beta)
    S <- SRp()$S
    D <- Bayesdata()$d   
    # Get unique alpha 
    alpha.star <- star$star
    nstar <- length(alpha.star)
    br <- model.br(alpha.star,beta,D)    
    for (i in 1:nstar){
      R <- srmodel(alpha.star[i],beta,S,D)
      lines(S/u,(R-S)/u,lty=2,lwd=2,col=i+1)
    }
    tex <- star$txt 
    legend('right',col=c(1:nstar+1),lwd=3,lty=2,legend=tex,box.lty=0)
  }
    # Confidence Interval 
  if (input$Li =='confidence') {
    lwr <- SRp$Rl-SRp$S
    upr <- SRp$Ru-SRp$S
  }
  else {
    # Prediction Interval
    lwr <- SRp$Rl.p-SRp$S
    upr <- SRp$Ru.p-SRp$S
  }
  # Add CI    
  if(input$show.int==TRUE){
    polygon(c(SRp$S,rev(SRp$S)),c(upr,rev(lwr)),col=tcol('grey',50),border=NA)
  }
  # Add Years
  if(input$show.points==TRUE) {
    points(xp$S/u,(xp$R-xp$S)/u,pch=19,cex=1.5,col=colp)
    legend('topleft',col=unique(colp),legend=unique(dyear), pt.cex = 1.5,
           pch=19,box.lty=0)
    pointLabel(xp$S/u,(xp$R-xp$S)/u, labels=as.character(xp$Yr), cex= 1,col=1)
  }
  
  # Add Smsy
  t1 <- ''
  l1 <- 0
  if(input$show.smsy==TRUE) {
    if(input$add=='kf'){
      abline(v=c(median(SR.post()$Smsy),br$Smsy)/u,col=c(1:(nstar+1)),lty=2) 
      t1 <- paste('Smsy:',c('Mean',star$txt),paste(round(c(median(SR.post()$Smsy),br$Smsy)/u,0)))
    } else {
      abline(v=median(SR.post()$Smsy)/u,col=1,lty=2)
      t1 <- paste('Smsy:',round(median(SR.post()$Smsy)/u,0))
    }
    l1 <- 2
  }
  # Add Smax       
  t2 <- ''
  l2 <- 0
  if(input$show.smax==TRUE & input$Model == 'Ricker') {
    abline(v=median(SR.post()$Smax)/u,col=1,lty=3)
    t2 <- paste('Smax:',round(median(SR.post()$Smax/u),0))
    l2 <- 3
  }
  legend('topright',c(t1,t2),lty=c(rep(l1,length(t1)),l2),
         lwd=2, col=c(1:length(t1),1),box.lty=0)    
  })

# Plt_residual --- Plot Residual Plot ------------------------------------------
output$Plt_residual <- renderPlot({
  year <- sr.data()$Yr
  if(input$add=='ar1'){
    resid <-SR.resid()$RD2  
   } else {
    resid <- SR.resid()$RD
   }
  resid.m <- apply(resid,2,mean)
  cil <- apply(resid,2,function(x) quantile(x, 0.025))
  ciu <- apply(resid,2,function(x) quantile(x, 0.975))
  par(bty='l')
  plot(resid.m~year,xlab='Year',type='l',ylab='Residuals',lwd=2, main='Residual',
       ylim =c(min(cil),max(ciu)))
  abline(h=0)
#  model <- gam(resid.m~s(year),family=gaussian, fit =TRUE)
#  pred.year <- data.frame(year, predict.gam(model,se = TRUE))    
#  lines(pred.year$year, pred.year$fit,lwd=2,col=4)
  polygon(c(year,rev(year)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA)
  })


# Plt_predict --- Plot Predicted Plot ------------------------------------------
output$Plt_predict <- renderPlot({
  year <- sr.data()$Yr
  resid <-SR.resid()$RD
  R <- log(Bayesdata()$R)
  R.m <- R-apply(resid,2,mean)
  cil <- R - apply(resid,2,function(x) quantile(x, 0.025))
  ciu <- R - apply(resid,2,function(x) quantile(x, 0.975))
  par(bty='l')
  plot(R.m~year,xlab='Year',type='l',lwd=2,ylab='ln(Recruit)',main='Recruit',
       ylim =c(min(cil,R),max(ciu,R)))
  points(R~year,pch=19,col=2)
  polygon(c(year,rev(year)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA)
  legend('topright',legend=c('Observed','Predicted '),pch=c(19,NA),lty=c(NA,1), col=c(2,1),box.lty=0)
 })

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 5: Escapement Goal Analyses 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#===============================================================================
#  Smsy Goal Analyses: This produces 
#  EG.Smsy, EG.Smsy.st, SA.BEG, p.msy, p.msy.t
#===============================================================================
  smsyprof <- ProfileServer("smsy",SR.pred,'MSY',as.numeric(input$ui))
  EG.Smsy <- reactive({smsyprof$EG()})
  EG.Smsy.st <- reactive({smsyprof$EG.st()})
  SA.BEG  <- reactive({smsyprof$BEG()})
  p.msy <- reactive({smsyprof$p.min()})
  p.msy.t <- reactive({smsyprof$p.t()})
  plt.msy.prof <- reactive({smsyprof$plt.profile()})
  
  ProfPlotServer("smsy.p",smsyprof,'MSY',as.numeric(input$ui))
  
#===============================================================================
#  Smax Goal Analyses 
#===============================================================================
smaxprof <- ProfileServer("smax",SR.pred,'Rmax',as.numeric(input$ui))  
  EG.Smax <- reactive({smaxprof$EG()})
  EG.Smax.st <- reactive({smaxprof$EG.st()})
  SM.BEG  <- reactive({smaxprof$BEG()})   # Smax based goal range
  p.max <- reactive({smaxprof$p.min()})
  p.max.t <- reactive({smaxprof$p.t()})
  plt.max.prof <- reactive({smaxprof$plt.profile()})
  
  ProfPlotServer("smax.p",smaxprof,'Rmax',as.numeric(input$ui))
  
#===============================================================================
#  Smsy-Smax Goal Analyses Output 
#===============================================================================
plot_range <- function(out,baseplot,sr.data,SRp,Srange1,Srange2=c(NA,NA),goal=NA,u)
  {
  x <- sr.data
  xp <- x/u
  SRp <- SRp()/u
  replayPlot(baseplot)
  if(out=='r'){
  ymin <-c(0,0)
  ymax <-c(1.1*max(xp$R),1.1*max(xp$R))
  }
  if(out=='y'){
    ymin <-c(min(SRp$Rl-SRp$S),min(SRp$Rl-SRp$S))
    ymax <-c(1.1*max(xp$R-xp$S),1.1*max(xp$R-xp$S))
  }
  polygon(c(Srange1/u,rev(Srange1/u)),c(ymin,ymax),col=tcol(3,80),border=NA)
  # Plot escapement goal range 
  if(!is.na(sum(Srange2))) {
    polygon(c(Srange2/u,rev(Srange2/u)),c(ymin,ymax),col=tcol(4,80),border=NA)
  }
  if(!is.na(goal)) {
  abline(h=goal/u,lwd=2,col=2)
  }  
  }

# Yield and Recruit Plot -------------------------------------------------------
output$Plt_rec.pg <- renderPlot({
  plot_range('r',base.r(),sr.data(),SRp(),Srange1=EG.Smsy()$S.Range,Srange2=EG.Smax()$S.Range,u=as.numeric(input$ui))
  })
  
output$Plt_yield.pg <- renderPlot({
  plot_range('y',base.y(),sr.data(),SRp(),Srange1=EG.Smsy()$S.Range,Srange2=EG.Smax()$S.Range,u=as.numeric(input$ui))
  })
  
# Txt_Srange.smsy -------- Smsy goal output ------------------------------------
  output$Txt_Srange.smsy <-renderUI({ SA.BEG() })
# Txt_Srange.smax -------- Smax Goal range output table ------------------------
  output$Txt_Srange.smax <-renderUI({ SM.BEG() })
  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  4.0 Target Yield and Recruit based Escapement Goal
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#---- UI Output Initial Value generating function ------------------------------
numinput <- function(dat,p=0.5){
  s <- quantile(dat,p,na.rm=TRUE)
  D <- floor(log10(s))
  # This makes largest numbers into integer (e.g. 100000)
  imr <- round(s,-D)  
  step <- 10^(D-1)
  out <- c(imr,step)
  return(out)
}

#---- UI Output-----------------------------------------------------------------
output$minYield = renderUI({
  v <- numinput(sr.data()$R-sr.data()$S,0.5)
  numericInput("y1", "Min Mean Yield", value=v[1],min=0, step=v[2])
})

#----- Yield Goal Simulation ---------------------------------------------------
Yield_gl_sim <- reactive({
# Import User defined Yield Gaol 
  yg <- input$y1        # Target minimum yields 
# Import MCMC Expected mean Yields 
  mc.Y <- SR.pred()$Y
# Import MCMC Expected annual Yield 
  mc.Y.p <- SR.pred()$Y.p  
# Import S 
  S <- SR.pred()$S
# For each simulation, determine if expected yields exceed desired goal  
# Mean yields    
  mc.Yb <- apply(mc.Y,2,function(x) ifelse(x >yg,1,0))
# Annual yields  
  mc.Ybp <- apply(mc.Y.p,2,function(x) ifelse(x >yg,1,0))   
# calculate mean: This is the same as probability   
  mc.Ypm <- colMeans(mc.Yb)  # mean yield
  mc.Ypa <- colMeans(mc.Ybp) # annual yield
  out <- list(mc.Yp=mc.Ypm, mc.Ypa=mc.Ypa)
  return(out)
  })



# Optimum Mean and annual Yield Proflie Plot 
output$Plt_yield.prof <- renderPlot({
  u <- as.numeric(input$ui)
  mult <- mult(u)
  yg <- input$y1
  ypg <- input$y1p/100
  # Bootstrap probile 
  S <- SR.pred()$S/u
  mc.Yp <- Yield_gl_sim()$mc.Yp
  mc.Ypa <- Yield_gl_sim()$mc.Ypa
  # Create a plot 
  par(xaxs='i',yaxs='i',bty='l')
  plot(S,mc.Yp,type='l',ylim=c(0,1),ylab = 'Probability',xlab=paste("Escapement",mult),
       main=paste('Minimum',yg,'Yield probability plot')) 
  lines(S,mc.Ypa,lty=2)
  abline(h = ypg,lwd=2,col=2)
  BEG.p <- Yield_gl()/u 
  polygon(c(BEG.p[1,],rev(BEG.p[1,])),c(c(0,0),c(1,1)),col=tcol(3,80),border=NA)
  polygon(c(BEG.p[2,],rev(BEG.p[2,])),c(c(0,0),c(1,1)),col=tcol(4,80),border=NA)
  legend('topright',legend=c('Mean','Annual'),lty=c(1,2), box.lty=0)
  }) 

# Find Yield Target Intersection 
Yield_gl <- reactive({
  ypg <- input$y1p/100
  S <- SR.pred()$S 
  mc.Yp <- Yield_gl_sim()$mc.Yp
  mc.Ypa <- Yield_gl_sim()$mc.Ypa
# Find Intersections 
  b.p <- S[mc.Yp > ypg]
  b.pa <- S[mc.Ypa > ypg]
  BEG.p <- c(NA,NA)
  if(sum(b.p) > 0){BEG.p <- c(min(b.p),max(b.p))}
  BEG.pa <- c(NA,NA)
  if(sum(b.pa) > 0){BEG.pa <- c(min(b.pa),max(b.pa))}
  out <- rbind(BEG.p,BEG.pa)
  return(out)
  })

# Print Optimum Yield Proflie Goal Range  
output$Txt_Yield_gl <-renderText({
  BEG.p <- Yield_gl()
  paste(
  paste('Mean target range:',BEG.p[1,1],'-',BEG.p[1,2]),
  paste('Annual target range:',BEG.p[2,1],'-',BEG.p[2,2]),
  sep='\n')
  })

# Yield Plot 
output$Plt_yield.gl <- renderPlot({
  u <- as.numeric(input$ui)
  BEG.p <- Yield_gl()/u 
  plot_range('y',base.y(),sr.data(),SRp(),BEG.p[1,],BEG.p[2,],input$y1,u)
#  yg <- input$y1/u
#  abline(h=yg,lwd=2,col=2)
    }) 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  4.0 Target Recruitment  based Escapement Goal
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#---- UI Output----------------------------------------------------------------------
output$minRec <- renderUI({
  v <- numinput(sr.data()$R,0.5)
  numericInput("r1", "Min Mean Recruit", value=v[1],min=0, step=v[2])
})

Rec_gl_sim <- reactive({
  # Import Targe Recruitment goal   
  rg <- input$r1
  # Import bootstrap S and Recruit 
  S <- SR.pred()$S 
  R <- SR.pred()$R
  R.p <- SR.pred()$R.p
  # For each bootstrap, determin if expected Recruitment exceed desired goal  
  Rp <- apply(R,2,function(x) ifelse(x >rg,1,0))
  # Recruitment target probabilty profil   
  Rp <- colMeans(Rp)
  Rpa <- apply(R.p,2,function(x) ifelse(x >rg,1,0))
  # Recruitment target probabilty profil   
  Rpa <- colMeans(Rpa)   
  out <- list(Rp=Rp, Rpa = Rpa)
  return(out)
})


#------------------------------------------------------------------------------
#  Calculate probability that intercepts profile 
#------------------------------------------------------------------------------
Rec_gl <- reactive({
  rpg <- input$r1p/100
  S <- SR.pred()$S 
  Rp <- Rec_gl_sim()$Rp
  Rpa <- Rec_gl_sim()$Rpa  
  # Find Intersections 
  b.p <- S[Rp > rpg]
  b.pa <- S[Rpa > rpg]
  BEG.p <- c(NA,NA)
  if(sum(b.p) > 0){BEG.p <- c(min(b.p),max(b.p))}
  BEG.pa <- c(NA,NA)
  if(sum(b.pa) > 0){BEG.pa <- c(min(b.pa),max(b.pa))}
  out <- rbind(BEG.p,BEG.pa)
  return(out)
})

# Recruit Plot 
output$Plt_rec.gl <- renderPlot({
  u <- as.numeric(input$ui)
  BEG.p <- Rec_gl()/u
  plot_range('r',base.r(),sr.data(),SRp(),BEG.p[1,],BEG.p[2,],input$r1,u)
#  rg <- input$r1/u
#  abline(h=rg,lwd=2,col=2)
  }) 

#  Minimum Recruit Profile Plot 
output$Plt_rec.prof <- renderPlot({
  rg <- input$r1
  rpg <- input$r1p/100
  u <- as.numeric(input$ui)
  mult <- mult(u)
  S <- SR.pred()$S/u
  Rp <- Rec_gl_sim()$Rp
  Rpa <- Rec_gl_sim()$Rpa  
  par(xaxs='i',yaxs='i',bty='l')
  plot(S,Rp,type='l',ylim=c(0,1),ylab = 'Probability',xlab=paste("Escapement",mult),
       main=paste('Minimum',rg,'Recruit probability Plot')) 
  lines(S,Rpa,lty=2)
  BEG.p <- Rec_gl()/u 
  polygon(c(BEG.p[1,],rev(BEG.p[1,])),c(c(0,0),c(1,1)),col=tcol(3,80),border=NA)
  polygon(c(BEG.p[2,],rev(BEG.p[2,])),c(c(0,0),c(1,1)),col=tcol(4,80),border=NA)
  abline(h = rpg,lwd=2,col=2)
  legend('topright',legend=c('Mean','Annual'),lty=c(1,2), box.lty=0)
  })  


# Optimum Recruit Proflie Escapement Goal 
output$Txt_Rec_gl <-renderText({
  BEG.p <- Rec_gl()
  paste(
    paste('Mean target range:',BEG.p[1,1],'-',BEG.p[1,2]),
    paste('Annual target range:',BEG.p[2,1],'-',BEG.p[2,2]),
    sep='\n')
  })

#==============================================================================
#  Custom escapement goal analyses 
#===============================================================================
#---- UI Output----------------------------------------------------------------------
output$minEG <- renderUI({
  v <- numinput(data()[,2],0.25)
  numericInput("lg", "Lower", value=v[1],min=0, step=v[2])
 })
#---- UI Output-----------------------------------------------------------------
output$maxEG <- renderUI({
  v <- numinput(data()[,2],0.75)
  numericInput("ug", "Upper", value=v[1],min=0, step=v[2])
})
#---- UI Output-----------------------------------------------------------------
output$cyg = renderUI({
  v <- numinput(sr.data()$R-sr.data()$S,0.25)
  numericInput("yg", "Min Target Yield", value=v[1],min=0, step=v[2])
})
#---- UI Output-----------------------------------------------------------------
output$crg = renderUI({
  v <- numinput(sr.data()$R,0.25)
  numericInput("rg", "Min Target Recruit", value=v[1],min=0, step=v[2])
})

#----- Profile output ----------------------------------------------------------
output$plt_msyprof_c <- renderPlot({
  u <- as.numeric(input$ui)
  replayPlot(plt.msy.prof())
  S <- c(input$lg,input$ug)/u
  polygon(c(S,rev(S)),c(c(0,0),c(1,1)),col=tcol(3,80),border=NA)
  #  Add legends 
    percent <- c(90,80,70,as.numeric(p.msy()))
    EG.pf <- (data.frame(S=EG.Smsy()$S,t(EG.Smsy.st()$S.prof.st),EG.Smsy()$S.prof))
    EG.p <- EG.pf[EG.pf$S>=input$lg,]
    pl <- round(EG.p[1,-1]*100,0)
    EG.p <- EG.pf[EG.pf$S>=input$ug,]
    pu <- round(EG.p[1,-1]*100,0)
    txt <- c(paste(percent,'%','MSY','acehiving',pl,' - ',pu,'%'))
  legend("right", legend= txt, lwd=c(1,1,1,2), lty=c(1,2,4,1),
         col=c(1,1,1,4),box.lty=0)
  
  })

#----- Profile output ----------------------------------------------------------
output$plt_maxprof_c <- renderPlot({
  u <- as.numeric(input$ui)
  replayPlot(plt.max.prof())
  S <- c(input$lg,input$ug)/u
  polygon(c(S,rev(S)),c(c(0,0),c(1,1)),col=tcol(4,80),border=NA)
  #  Add legends 
  percent <- c(90,80,70,as.numeric(p.max()))
  EG.pf <- (data.frame(S=EG.Smax()$S,t(EG.Smax.st()$S.prof.st),EG.Smax()$S.prof))
  EG.p <- EG.pf[EG.pf$S>=input$lg,]
  pl <- round(EG.p[1,-1]*100,0)
  EG.p <- EG.pf[EG.pf$S>=input$ug,]
  pu <- round(EG.p[1,-1]*100,0)
  txt <- c(paste(percent,'%','RMAX','acehiving',pl,' - ',pu,'%'))
  legend("right", legend= txt, lwd=c(1,1,1,2), lty=c(1,2,4,1),
         col=c(1,1,1,4),box.lty=0)
})


#-------------------------------------------------------------------------------
CG_sim <- eventReactive(input$Run,{
#-----------------------------------------------------------------------  
  progress <- Progress$new(session, min=1, max=15)
  on.exit(progress$close())
  progress$set(message = 'MCMC calculationin progress',
               detail = 'This may take a while...')
  for (i in 1:15) {progress$set(value = i)}
  #-----------------------------------------------------------------------   
  #  Import user defined lower and upper goal 
  lg <- input$lg
  ug <- input$ug
  # create goal range 
  S <- seq(lg,ug,length.out=201)   
  D <- Bayesdata()$d
  srmodel <- Bayesmodel()$model
  #---------- Extract MCMC SR Model Parameters --------------------------------
  lnalpha <-SR.post()$lnalpha
  beta <- SR.post()$beta
  sigma <- SR.post()$sigma
  nrow <- length(SR.post()$lnalpha)  #  Extract number of MCMC sample 
  # Create Expected mean and observed Recruit MCMC matrix    
  R <- matrix(NA,nrow=nrow,ncol=201) 
  R.p <- matrix(NA,nrow=nrow,ncol=201)
  
  for(i in 1:nrow){
    # Calculated expected Returns form each MCMC SR model paramters   
    Ey <- srmodel(lnalpha[i],beta[i],S,D)
    R[i,] <- Ey
    # mc.R.p adds observation error (sigma)  
    R.p[i,] <- exp(rnorm(201,log(Ey),sigma[i]))
  }  
  # Create expecte mean and observed Yield matric
  Y <-  t(t(R)-S) 
  Y.p <-  t(t(R.p)-S) 
  
#------  Create Output list files ---------------------------------------------  
  # Outputs are mean and annual Yields and recruits within proposed S range.    
  out <- list(S = S, R = R, Y = Y, R.p = R.p, Y.p = Y.p)
  return(out) 
  })


#-----------------------------------------------------------------------
#  Plot distribution of Mean and Annual  Yield at Given Escapement Range
#-----------------------------------------------------------------------
output$EGR.Yield <- renderPlot({
  par(mfrow=c(1,1),xaxs='i',yaxs='i',bty='l')
  u <- as.numeric(input$ui)
  mult <- mult(u)
  yg <- input$yg/u
  Y.p <-CG_sim()$Y.p
  Y.p <- Y.p[Y.p < quantile(Y.p,0.99)]
  # Bootstrap estimate  
  d1 <- density(Y.p/u)
  d2 <- density(CG_sim()$Y/u)
  plot(d2,xlim =c(min(d1$x),max(d1$x)),axes = FALSE,main='',xlab='',ylab='')
  par(new = TRUE)  
  plot(d1, main='Expected Mean and Annual Yields',lty=2,xlab=paste("Yield",mult),ylab='')
  abline(v=yg,col=2)
  legend('topright',legend=c('Mean','Annual'),lty=c(1,2), box.lty=0)
  }) 

#-----------------------------------------------------------------------
#  Plot distribution of Recruit and Yield at Given Escapement Range
#  SR model Parameteric based CI and PI
#-----------------------------------------------------------------------
output$EGR.Rec <- renderPlot({
  par(mfrow=c(1,1),xaxs='i',yaxs='i',bty='l', cex=1.2)
  u <- as.numeric(input$ui)
  mult <- mult(u)
  rg <- input$rg/u
  # Annualestimate   
  R.p <-CG_sim()$R.p
  R.p <- R.p[R.p < quantile(R.p,0.99)]
  # Bootstrap estimate
  d1 <- density(R.p/u)
  d2 <- density(CG_sim()$R/u)
  plot(d2,xlim =c(min(d1$x),max(d1$x)),axes = FALSE,main='',xlab='',ylab='')
  par(new = TRUE)
  # Mean estimate   
  # Bootstrap estimate  
  plot(d1, main='Expected Mean and Annual Recruit',xlab=paste("Recruit",mult),
           lty=2,ylab='')
  abline(v=rg,col=2)
  legend('topright',legend=c('Mean','Annual'),lty=c(1,2), box.lty=0)
  }) 


output$Txt_Rec_cg <- renderPrint({
  R <- as.vector(CG_sim()$R)    
  R.p <- as.vector(CG_sim()$R.p)
  dat <- data.frame(R,R.p)
  names(dat) <- c('Mean Recruit','Annual Recruit')
  print(summary(dat),digits=0)
  })
  

output$Txt_Yield_cg <- renderPrint({
  Y <- as.vector(CG_sim()$Y)
  Y.p <- as.vector(CG_sim()$Y.p)
  dat <- data.frame(Y,Y.p)
  names(dat) <- c('Mean Yields','Annual Yields')
  print(summary(dat),digits=0)
  })

# Calculate Probability meeting target  
output$Txt_Rec_pb_cg <- renderText({
  rg <- input$rg
  prg <- mean(ifelse(CG_sim()$R>rg,1,0))
  pyg <- mean(ifelse(CG_sim()$R.p>rg,1,0))
  t.prg <- paste('Meeting Mean Recruit Target:',round(100*prg,0),'%')
  t.pyg <- paste('Meeting Annual Recruit Target:',round(100*pyg,0),'%')
  paste(t.prg,t.pyg,sep='\n')
  })

# Calculate Probability meeting target  
output$Txt_Yield_pb_cg <- renderText({
  yg <- input$yg
  prg <- sum(ifelse(CG_sim()$Y>yg,1,0))/length(CG_sim()$Y)
  pyg <- sum(ifelse(CG_sim()$Y.p>yg,1,0))/length(CG_sim()$Y.p)
  t.prg <- paste('Meeting Mean Yield Target:',round(100*prg,0),'%')
  t.pyg <- paste('Meeting Annual Yiled Target:',round(100*pyg,0),'%')
  paste(t.prg,t.pyg,sep='\n')
  })

# Recruit Plot 
output$Plt_rec.cg <- renderPlot({
  plot_range('r',base.r(),sr.data(),SRp(),c(input$lg,input$ug),goal=input$rg,u=as.numeric(input$ui))
}) 

# Yield Plot 
output$Plt_yield.cg <- renderPlot({
  plot_range('y',base.y(),sr.data(),SRp(),c(input$lg,input$ug),goal=input$yg,u=as.numeric(input$ui))
  }) 

#===============================================================================
#===============================================================================
#  Percentile Analyses 
#===============================================================================
# Call Percentile Analyses module Server 
prcntout <- PercentileServer("prcnt",e.data,as.numeric(input$ui))

# Txt_Tier : Tier Definition output  
  txt <- reactive({prcntout$Txt_Tier()})
output$Txt_Tier <- renderUI({ txt() })

# Txt_Note:  Tier based goal range  
  txt2 <- reactive({prcntout$Txt_Note()})
output$Txt_Note <- renderUI({ txt2() })

# Plt_prcnt: Plot Run-Escapement Time series 
output$Plt_prcnt <- renderPlot({ prcntout$Plt_prcnt() })

#===============================================================================
#===============================================================================
#===============================================================================
#  Risk Analyses 
#===============================================================================
# Call Riks Analyses module Server 
riskout <- RiskServer("risk",e.data,as.numeric(input$ui))

Risk_sim_base <- reactive({riskout$Risk_sim_base()})
Risk_sim <- reactive({riskout$Risk_sim()})

# Risk output
output$Plt_risk <- renderPlot({ riskout$Plt_risk()}) 

# Txt_dwtest: Durbin-Watson test results ----------------------------------------
output$Txt_dwtest <- renderPrint({ Risk_sim_base()$dw })

# Risk Model  ---------------------------------------------
output$Txt_Risk_Model <-renderText({ Risk_sim_base()$md})

# Risk Target: Print out Target ------------------------------------------------
output$Txt_Risk <-renderUI({ Risk_sim()$txt })

# Plt_risk2 --------------------------------------------------------------------
output$Plt_risk2 <- renderPlot({riskout$Plt_risk2()})


#===============================================================================
#  Panel 4: Management Strategy Evaluation    
#===============================================================================
#-------------------------------------------------------------------------------
# MSE simulation 
# This simulation evaluate effects of an escapement goal and fishery management 
# strategy for achieving management objectives in more realistic situations
# The model creates fish population dynamics based on data.

# Step 1:   
# Step 2: Use the escapement data to generate expected recruit and run 

# Step 1: Obtain escapement data from the last calendar year escapement to the 
# last complete brood year escapement. 
# Step 2: Use the escapement data to generate expected recruit and run 
#---- UI Output-----------------------------------------------------------------
output$LEG <- renderUI({
#  v <- numinput(data()[,2],0.25)
  numericInput("LEG", "Lower", value=input$lg)
})
#---- UI Output-----------------------------------------------------------------
output$UEG <- renderUI({
#  v <- numinput(data()[,2],0.75)
  numericInput("UEG", "Upper", value= input$ug)
})

#---- UI Output-----------------------------------------------------------------
output$maxH <- renderUI({
  v <-max(data()[,3]-data()[,2])
  d <- floor(log10(v))
  numericInput(inputId="maxH", "Maximum", value=round(v,-d))
})

#---- UI Output-----------------------------------------------------------------
output$minH <- renderUI({
    v <-min(data()[,3]-data()[,2])
    d <- floor(log10(v))
  numericInput(inputId="minH", "Minimum", value=round(v,-d))
})
 
#Bayesian figure page
output$nsim = renderUI({
  lnalpha <-SR.post()$lnalpha
  mn <- length(lnalpha)
  sliderInput("nsim", "number of simulation", value=100,min=1,max=mn,step=100)
})

observeEvent(input$show, {
  showModal(modalDialog(
    title = "Management Option",
    HTML("
          <b>Harvest Strategy:</b> Max fishery harvest regardless run size <br/><br/>
          <b>Escapement Strategy</b><br/>
          Predicted run < target escapement -> <b>Fishery is closed</b><br/>
          Predicted run > target escapement -> <b>Up to max fishery harvest</b><br/><br/>
          <b>Hybrid Strategy</b><br/> 
          Predicted run < target escapement -> <b>Minimum fishery harvest.</b><br/>
          Predicted run > target escapement -> <b>Up to max fishery harvest.</b><br/>
         ")
          ,
        easyClose = TRUE
  ))
})

#-----------------------------------------------------------------------  
#  MSE.int: Create initial modeling conditions
#-----------------------------------------------------------------------  
MSE.int <- reactive({
#-----------------------------------------------------------------------  
# Extract S0 from brood data   
  brood <- brood.out()$brood
# Extract S0
  S0 <- brood[is.na(brood$Recruit),2]
# S0 is last lage years of escapement 
  S0 <- S0[!is.na(S0)]
# Extract lage   
  lage <- length(S0)
# Extract number of simulation years
  nyrs <- input$simy
# import brood table
  brood.p <- brood.p()
# Maturity schedule is a random sample of observed brood proportion 
  e.p  <- brood.p[sample(dim(brood.p)[1],nyrs+lage,replace = TRUE),]    
# Prediction and Implementation Error   
  e.pred <- (rnorm(nyrs,1,input$spred/100))
  e.imp <- (rnorm(nyrs,1,input$simpH/100))
# Determine Fishery opening target: FT  
  out <- list(S0=S0,e.pred = e.pred, e.imp = e.imp, e.p = e.p)
  return(out) 
})
#-------------------------------------------------------------------------------
# Txt_Strategy:  Explanation of Strategy 
#-------------------------------------------------------------------------------  
Txt_Strategy <- reactive({
  txt<- HTML(
    if(input$strgy =="Harvest"){
      paste("Constant Harvest Strategy",
            "Target harvest is taken regardless run size",
             sep = '<br/>')
    } else if(input$strgy == "Escapement")  {
      paste("Escapement Goal Strategy",
            "Predicted run is below target escapement",
            "     Fishery is closed.",
            "Predicted run is above target escapement",
            "     Fihsery harvest is up to max harvest rare or max harvest.",
            sep = '<br/>')
    } else {
      paste("Hybrid Strategy",
            "Predicted run is below target escapement",
            "     Fishery harvest is minimum.",
            "Predicted run is above target escapement",
            "     Fihsery harvest is up to max harvest rare or max harvest.",
            sep = '<br/>')
    } 
  )
  return(txt)
})


MSE.sim <- function(srmodel,lnalpha.i,beta,S0,D,e.Rec,e.p,e.pred,e.imp,FT,mH){
#-------------------------------------------------------------------------------
#  Create Brood Ages 
#-------------------------------------------------------------------------------  
# lage: last age, nages: number of adult return age, fage: first adult return age
  lage <- length(S0)
  nages <- dim(e.p)[2]
  fage <- lage - nages + 1
  nyrs <- length(e.pred)
#-------------------------------------------------------------------------------
#  Create Empty vector for simulation and output  
#-------------------------------------------------------------------------------  
# Brood Year Recruit: R0: straight from model, R: R0 with error 
  R0 <- numeric(nyrs+lage)
  R <- numeric(nyrs+lage)
# Annual Run: N: True , N.pred: Assessed run with error 
  N <- numeric(nyrs)
  N.pred <- numeric(nyrs)
# Annual Run by age 
  N.ta <- matrix(0,ncol=nages, nrow=nyrs+lage*2)
# Annual Harvest: H: True , H.target: Target Harvest 
  H <- numeric(nyrs)
  H.target <-numeric(nyrs)
# Annual Escapement: S: True   
  S <- numeric(nyrs)
# EG: Met lower escapement goal?   
  EG <- numeric(nyrs)

#-------------------------------------------------------------------------------
#  Population Dynamics: Initialization (b.y is brood year)
#-------------------------------------------------------------------------------
  for (b.y in 1:lage){
# Calculate expected Returns from SR model parameter   
    R0[b.y] <- srmodel(lnalpha.i[b.y],beta,S0[b.y],D)
# Add Error   
    R[b.y] <- R0[b.y]*exp(e.Rec[b.y])
# Distribute recruitment based on maturity schedule 
#b.y+fage+a-1 is calendar year. 
# Example: fage = 4: fish return as age 4.  
# Fish spawn in b.y (say 2000) will return in 
# 2004 as 4 years old, 2005 as 5yo, 2006 as 6 yo,...
    for (a in 1:nages){N.ta[b.y+fage+a-1,a] <- R[b.y]*e.p[b.y,a]}
  }  # Expected return   
  
#-------------------------------------------------------------------------------
#  Population Dynamics: Management Strategies (y is calendar year) 
#-------------------------------------------------------------------------------
for (y in 1:nyrs){
# Brood year b.y is y + lage
  b.y <-y+lage
# Annual Run is sum of all ages
  N[y] <- sum(N.ta[b.y,])
# Predicted Run with error 
    N.pred[y] <- N[y]*(e.pred[y])
# Management based on Escapement goal 
  H.target[y] <- ifelse(
# Case 1 assessed run is less than FT, Fishery is minimum harvest
# minimum harvest = 0 for Escapement priority or defined for other strategy     
    N.pred[y] < FT, mH[1], 
# Case 2 assessed run is greater than FT, 
# fishery target is surplus or min harvest whichever bigger  
# fishery target is also between surplus and the max harvest (fishing capacity) whichever smaller 
    min(mH[2],max(mH[1],(N.pred[y]-FT)))
    )  # End ifelse 
# Target Harvest rate 
# Target harvest rate is 
  HTR <- H.target[y]*(e.imp[y])/N.pred[y]
#  Actual Harvest: Harvest will not exceed 95% of incoming ru
    H[y] <- N[y]*min(HTR,0.95)
    S[y] <- N[y] - H[y]
#     
    R0[b.y] <- srmodel(lnalpha.i[b.y],beta,S[y],D)
# Expected return        
    R[b.y] <- R0[b.y]*exp(e.Rec[b.y])
    # Fill ages       
    for (a in 1:nages){N.ta[b.y+fage+a-1,a] <- R[b.y]*e.p[b.y,a]}
  }
#  out <- list(R=data.frame(cbind(N,S,H,R[-c(1:lage)],R0[-c(1:lage)],EG)), N.ta=data.frame(N.ta))
  out <- data.frame(cbind(N,S,H,R[-c(1:lage)],R0[-c(1:lage)]))
  names(out) <- c('N','S','H','R','R0')
  return(out)
}

msesim <- eventReactive(input$SimRun,{
#-----------------------------------------------------------------------
#  Import Error Data 
#-------------------------------------------------------------------------------  
# Initial Spawners  
  S0 <- MSE.int()$S0
  lage <- length(S0)
# Prediction and Implementation Error
  e.imp <- as.vector(MSE.int()$e.imp)
  e.pred <- as.vector(MSE.int()$e.pred)
  nyrs <- length(e.pred)
# Brood age comp 
  e.p <- as.matrix(MSE.int()$e.p)
#---------- Extract MCMC SR Model Parameters -----------------------------------
# SR model   
  srmodel <- Bayesmodel()$model
# D  
  D <- Bayesdata()$d
# SR parameters
  lnalpha <-SR.post()$lnalpha
  beta <- SR.post()$beta
  sigma <- SR.post()$sigma
  nsim <- length(lnalpha)
# Error is AR1 process
    if(input$add=='ar1'){
    phi <- SR.post()$phi
# e0 is the residuals of the last year    
    e0 <- SR.resid()$RD[,Bayesdata()$nyrs]
    }
# lnalpha is time variant  
  if (input$add=='kf'){
    sigmaw <- SR.post()$sigmaw
    ny <- Bayesdata()$nyrs
    parname <- paste0('lnalphai[',ny,']')
    # Extract the last year's lnalpha   
    lnalphai <- SR.post()[,parname]
  }
# Fishery   
  FT <- FTA(input$cmode,input$LEG,input$UEG)
  if(input$strType=='Escapement'){
    mH <- c(0,input$maxH)
  } else if(input$strType=='Harvest'){
    mH <- c(input$maxH,input$maxH)
  } else if(input$strType =='Hybrid'){
    mH <- c(input$minH,input$maxH)
  }
  

#-------------------------------------------------------------------------------
# Select Parameters 
#-------------------------------------------------------------------------------  
j <-sample(1:100,1)

#-------------------------------------------------------------------------------
# Set SR simulation parameters
#-------------------------------------------------------------------------------  
# Set lnalpha.i  (random walk when alpha is time varying) ----------------------
# Constant alpha
    lnalpha.i <- rep(lnalpha[j],nyrs+lage)
# Time Varying alpha  
  if (input$add=='kf'){
    w <- rnorm(nyrs+lage,0,sigmaw[j])
    lnalpha.i <- numeric(nyrs+lage)
    lnalpha.i[1] <- lnalphai[j]
    for(i in 2:(nyrs+lage)){
      lnalpha.i[i] <- lnalpha.i[i-1]+w[i]
    }
  }

# Set Error (Random, AR1) ------------------------------------------------------
# Random Error   
  e.Rec <- rnorm(nyrs+lage,0,sigma[j])
# Create AR1 Error 
  if(input$add=='ar1'){
    e.Rec.0 <- e.Rec
    e.Rec[1] <- e.Rec.0[1]+phi[j]*e0[j]
    for(i in 2:(nyrs+lage)){
    e.Rec[i] <- phi[j]*e.Rec[i-1]+e.Rec.0[i]
    }
  }         
# Set SR beta: Constant --------------------------------------------------------
  beta <- beta[j]

# Simulate MSE    
  sim.out <- MSE.sim(srmodel,lnalpha.i,beta,S0,D,e.Rec,e.p,e.pred,e.imp,FT,mH)  
  return(sim.out)   
})


output$simplot <- renderPlot({
  u <- as.numeric(input$ui)
  mult <- mult(u)
  lyear <- max(data()[,1])
  nyrs <- input$simy
  years <- c(1:nyrs)+lyear
  N <- msesim()$N
  H <- msesim()$H
  S <- msesim()$S
  R <- msesim()$R
  R0 <- msesim()$R0
  par(yaxs='i',bty='l')
  plot(years,N/u,type='l',ylim=c(0,max(N/u,na.rm=TRUE)), main='', xlab = '',ylab='')
  lines(years,H/u,col=2)
  lines(years,S/u, col=3,lwd=2)
  polygon(c(lyear-1,max(years)+2,max(years)+2,lyear-1),c(input$LEG/u,input$LEG/u,input$UEG/u,input$UEG/u),col=tcol(5,80),border=NA)
  abline(h=input$minH/u, col=2,lwd=2)
#    polygon(c(-2,nyrs+2,nyrs+2,-2),c(input$minH/u,input$minH/u,input$maxH/u,input$maxH/u),col=tcol(2,60),border=NA)
  title("MSE Simulation", xlab="Year", ylab=paste('Run / Escapement/Harvest',mult))
  legend('topright',legend=c('Run','Escapement','Harvest'),col = c(1,3,2),lwd=c(1,2,1), box.lty=0)
})

  e.freq <- function(data,c){
    temp1 <- rle(data)
    temp2 <- data.frame(length=temp1$length,value=temp1$values)
    temp3 <- temp2[temp2$value==c,1]
    return(temp3)
  }
  
MSE.out <- reactive({
  dat <- msesim()[,c('N','S','H')]
# Frequency of fishery closure  
  dat$H0 <- ifelse(dat$H==0,1,0)
# Frequency of fishery below minimum harvest
  dat$Hmin  <- ifelse(dat$H<input$minH,1,0)
# Frequency of meeting escapement goal  
  dat$EG <- ifelse(dat$S>=input$LEG,1,0)
# Consecutive fishery closure 
#  f.H0 <- e.freq(dat$H0,1)
# Consecutive fishery below minimum
#  f.Hmin <- e.freq(dat$Hmin,1)
# Consecutive escapement failure
#  f.EG <- e.freq(dat$EG,0)
  return(list(dat=dat)) 
})

output$Txt_sum.mse <- renderPrint({
    x <- MSE.out()$dat[,c('N','S','H')]
    min <- sapply(x,min, na.rm=TRUE)
    max <- sapply(x,max, na.rm=TRUE)
    mean <- sapply(x,mean, na.rm=TRUE)
    median <- sapply(x,median, na.rm=TRUE)
    sd <- sapply(x,sd,na.rm=TRUE)
    cv  <- sd/mean
    out <- data.frame(round(min,0),round(mean,0),round(sd,0),round(cv,3),
                      round(median,0),round(max,0))
    names(out) <- c('Min','Mean','SD','CV',
                       'Median','Max')
    row.names(out) <- c('Run','Escapement','Harvest')
    return(out)
  })

output$Plt_sum.mse <- renderPlot({
  boxplot(MSE.out()$dat[,c('H','S','N')],horizontal = TRUE, names = c('Harvest','Escapement','Run'))
})

output$Txt_HE_mse <- renderText({
  dat <- MSE.out()$dat[,c('H0','Hmin','EG')]
  t.H0 <- paste('Complete fishery closure:',round(100*mean(dat$H0),0),'%')
  t.Hmin <- paste('Below minimum harvest target:',round(100*mean(dat$Hmin,0)),'%')
  t.EG <- paste('Meeting escapement goal:',round(100*mean(dat$EG,0)),'%')
  out <- paste(t.H0,t.Hmin, t.EG,sep='\n')
  return(out)
})


output$Plt_freq_mse <- renderPlot({
  par(mfrow=c(1,3))
  dat <- MSE.out()$dat[,c('H0','Hmin','EG')]
  # Consecutive fishery closure 
    f.H0 <- e.freq(dat$H0,1)
  # Consecutive fishery below minimum
    f.Hmin <- e.freq(dat$Hmin,1)
  # Consecutive escapement failure
    f.EG <- e.freq(dat$EG,0)
  if(length(f.H0)>0){
    barplot(table(f.H0),main='Frequency of consecutive fishery closure',xlab='Years')
  }  
  # Consecutive fishery closure 
    if(length(f.Hmin)>0){
      barplot(table(f.Hmin),main='Frequency of consecutive fishery below minimum',xlab='Years')
    }
    if(length(f.EG)>0){
      barplot(table(f.EG),main='Frequency of consecutive below EG',xlab='Years')
    }
})

output$downloadReport <- downloadHandler(
  filename = 
    function() {
    paste('my-report', sep = '.', switch(
      input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    ))
  }
  ,
  
  content = function(file) {

    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
#    owd <- setwd(tempdir())
#    on.exit(setwd(owd))
#    file.copy(src, 'report.Rmd', overwrite = TRUE)
    
    library(rmarkdown)
    out <- render('report.Rmd', switch(
      input$format,
      PDF = pdf_document(), HTML = html_document(), Word = word_document()
    ))
    output_file=file.rename(out, file)
  }
)

})# End of Server 

# Create Shiny app ----
shinyApp(ui, server)
