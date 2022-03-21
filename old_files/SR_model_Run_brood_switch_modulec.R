#===============================================================================    
#  SR_model_Ru_Brood_switch.R  Shiny app for Frequentis Spawner Recruit Analyses 
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
library(shiny)        # used to run Shiny 
library(shinythemes)  # used to set Shiny theme
library(datasets)
library(lmtest)       # used for dwtest 
library(reshape2)
library(mgcv)   # GAM   
library(MCMCpack)
library(maptools)  # Used to make years not to overlap
library(nlme)   # gls method
library(AICcmodavg)
library(bsplus)
options(scipen=999)
source("Shiny_modules.R")
source("Shiny_Boot_modulesC.R")
#=======================================================================    
#  UI:  
#=======================================================================
ui<-fluidPage(
  navbarPage(
    theme = shinytheme("cerulean"),  id = "tabs",
    "Pacific Salmon Escapement Goal Analyses",
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 1:  Data Input and Submit 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabPanel("Data Input",
    sidebarPanel(width = 3,
#------------------------------------------------------------------------    
#  File Inuput
#------------------------------------------------------------------------
# Data Type  
selectInput(inputId="dataType","Data Type", choices = c('S-R','Run')) %>%
  shinyInput_label_embed(
    shiny_iconlink()  %>%
      bs_embed_popover(title="",
        content  = "Choose Summarized SR or Run data", placement = "right"
      )
    ),
conditionalPanel(
  condition = "input.dataType == 'Run'",
  # Input: Checkbox if file has header ----
  p("Select First age of run"),
  # Input: Select what to display
  numericInput("fage", "First Retun Age", value=4,min=1,max=20,step=1) %>%
    shinyInput_label_embed(
      shiny_iconlink() %>%
        bs_embed_popover(
          title = "", content = "Return Age =FW Age+SW Age + 1 ", placement = "right"
        )
    )
  ),# End Conditional Panel

# File Input modulde 
  dataInputUI("datain", "User data (.csv format)"),
# Check Limit datasets 
  p(strong("Choose brood year range")),
  uiOutput('yrange'),
#-------------------------------------------------------------------------------    
#  Conditional File Inuput UI
#-----------------------------------------------------------------------------
  BootrepUI('Boots'),
  selectInput(inputId="ui","Axis Dislpay Unit", choices = c(1,1000,1000000))
  ), # End of Sidebar  Panel

#-------------------------------------------------------------------------------    
# Main Panel: Input Data Summary 
#-------------------------------------------------------------------------------
  mainPanel(
    tabsetPanel(
      tabPanel("Data Table",
               h4(uiOutput('note')),
               dataTableOutput("Tbl_data")
               ),
      tabPanel("Brood Table",
              conditionalPanel(
                  condition = "input.dataType == 'Run'",
                  dataTableOutput("Tbl_data.brood"),
               p("Download Brood table"),
               downloadButton("downloadData", "Download"))
               # Horizontal line ----
               ),
#------------------ Time Series ------------------------------------------- 
      tabPanel("Time Series",
               plotOutput("Plt_srt"),
               plotOutput('Plt_runesc')  
              ),#End tabPanel
            
      tabPanel("Summary",
               verbatimTextOutput('Txt_sum.data'),
               plotOutput('Plt_hist.sry')
               ) #End tabPanel
          )  # End tabsetPanel
        )  # End mainPanel
  ), # End tabanel

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 2  SR Analyses 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  tabPanel("SR Analyses",
    sidebarPanel(width = 3,
      selectInput(inputId="SRM","SR Model", choices = c('Auto','Standard','AR1'))%>%
        shinyInput_label_embed(
          shiny_iconlink() %>%
          bs_embed_popover(
          title = "", content = "Auto: Choose better model based on Likelhood ratio test", placement = "right"
              )
            ),  
          strong(textOutput("modelslct")),
           hr(),
        checkboxInput(inputId="show.points", "show Years", TRUE), 
        checkboxInput(inputId="show.smsy", "show Smsy", TRUE),
        checkboxInput(inputId="show.smax", "show Smax", TRUE),
        checkboxInput(inputId="show.int", "show Interval", TRUE),
        numericInput(inputId="CI", "% Interval", value=90,min=0,max=100,step=5),
        selectInput(inputId="Li","Interval Type", choices = c('confidence','prediction'))
      ),  # End sidebarPanel
#------------------------------------------------------------------------    
#  SR Analyses Output
#--------- mainPanel  ----------------------------------------------------------    
  mainPanel(
    tabsetPanel(
#------------------ SR Plot----------------------------------------------        
      tabPanel("SR Plot",
        plotOutput(height='500px','Plt_SR'),
        downloadButton("down", label = 'Download the plot'),
        p(strong("SR Parameters")), 
        verbatimTextOutput('Txt_sr.pars')                      
            ),#End tabPanel

#------------------ Yield Plot-------------------------------------------        
      tabPanel("Yield Plot",
        plotOutput(height='500px','Plt_yield')
            ),#End tabPanel

#------------------ Residuals  --------------------------------------------- 
      tabPanel("Residuals", 
        plotOutput("Plt_predict"),      
        plotOutput("Plt_residual"),
        p(strong("Durbin-Watson Serial Correlation Analyses")), 
        verbatimTextOutput('Txt_dwtest')
            ),#End tabPanel
#------------------ Residuals  --------------------------------------------- 
      tabPanel("ANOVA", 
          p(strong("Anova Table")), 
          verbatimTextOutput('Txt_ANOA')      
      ),#End tabPanel

#------------------ Bootstrap ----------------------------------------------- 
      tabPanel("Bootstrap",
        verbatimTextOutput('Txt_sum.b'),
        verbatimTextOutput('Txt_quantile.b'),
        plotOutput("Plt_hist.b")
            ) # End tabPanel
        )#End tabsetPanel
      
      )#End mainPanel
    ),#End tabPanel

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 3  Escapement Goal Aanalyses 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  navbarMenu("Escapement Goal Analyses",
#-----------------------------------------------------------------------------    
#  Smsy Goal Analyses 
#-----------------------------------------------------------------------------    
    tabPanel("Smsy-Smax Goal Analyses",      
      sidebarPanel(width = 3,
          SmsyprofUI('smsy'), 
                   hr(),
          SmaxprofUI('smax')              
          ),  #End sidebarPanel
#-------  mainPanel ------------------------------------------------------------      
      mainPanel(
        tabsetPanel(
          
#------------------ Smsy Profile ----------------------------------------------- 
          tabPanel("Profile",
              plotOutput(height='300px','Plt_Smsy.prof'),
              plotOutput(height='300px','Plt_Smax.prof')  
                    ), #End tabPanel

#------------------ Smsy Yield Profile -----------------------------------------
          tabPanel("Yield  & Recruit Profile",
            splitLayout(cellWidths = c("50%", "50%"),         
              plotOutput(height='300px','Plt_yield.pg'),
              plotOutput(height='300px','Plt_rec.pg')
               ),
            splitLayout(cellWidths = c("50%", "50%"),   
                  p(strong("Smsy Goal Range")),  
                  p(strong("Smax Goal Range"))
                ),
            splitLayout(cellWidths = c("50%", "50%"),   
                  verbatimTextOutput("Txt_Srange.smsy"),  
                  verbatimTextOutput("Txt_Srange.smax")
                )
                    ), #End tabPanel
#------------------ Smsy Recruit Yield Profile -----------------------------------------
          tabPanel("Recruit & Yiled distribution",
               splitLayout(cellWidths = c("50%", "50%"),
            plotOutput(height='600px','Plt_Smsy.dist'),
            plotOutput(height='600px','Plt_Smax.dist')
               ),
            verbatimTextOutput('Txt_sum.smsy'),
            verbatimTextOutput('Txt_sum.smax')
                    )#End tabPanel
                  )#End tabsetPanel
                )#End mainPanel
              ),#End tabPanel Smsy Goal Analyses
#------------------------------------------------------------------------    
#  Smax Goal Analyses 
#------------------------------------------------------------------------    
    tabPanel("Smax Goal Analyses",
      sidebarPanel(width = 3,
#        p(strong("Smax Analyses")),            
#        sliderInput("p.rmx", "Min % of Rmax", value=90,min=0,max=100,step=5),
#        sliderInput("p.rmx.t", "% Meeting Rmax Target", value=90,min=0,max=100,step=5)
          ), # End sidebarPanel 

#-------------  mainPanel ------------------------------------------------------
      mainPanel(
        tabsetPanel(
#------------------ Smax Profile -----------------------------------------------                     
          tabPanel("Smax Profile",
#            plotOutput(height='500px','Plt_Smax.prof'),
#            verbatimTextOutput('Txt_Srange.smax')
                      ), # End tabrPanel 
                  
#------------------ Run Profile -----------------------------------------------                  
          tabPanel("Run Profile",
#                    splitLayout(cellWidths = c("50%", "50%"),                 
#            plotOutput(height='300px',"Plt_rec.smax"),
 #           plotOutput(height='300px',"Plt_yield.smax")
                      ),  # End tabPanel

#------------------ Smax Recruit Yield Profile -----------------------------------------
          tabPanel("Recruit & Yiled distribution",
#               splitLayout(cellWidths = c("50%", "50%"),
#            plotOutput(height='600px','Plt_Smax.dist'),
#            verbatimTextOutput("Txt_sum.smax")
#                plotOutput(height='300px','')
#              )
                      )# End tabPanel
                    )#End tabsetPanel
                  )#End mainPanel
                ),#End tabPanel Smax Profile

#------------------------------------------------------------------------    
#   Recruit & Yield Goal Analyses 
#------------------------------------------------------------------------    
    tabPanel("Recruit & Yield Goal Analyses",
      sidebarPanel(width = 3,
        conditionalPanel(condition="input.cPanel == 'Recruit Goal Analyses'",  
          p(strong("Recruit Goal Analyses")), 
          uiOutput('minRec'),
          sliderInput("r1p", "Min % Achieve", value=90,min=0, max=100,step=5)
                      ),  # End conditionalPanel
        conditionalPanel(condition="input.cPanel == 'Yield Goal Analyses'",                        
          p(strong("Yield GoalAnalyses")), 
          uiOutput('minYield'),
          sliderInput("y1p", "Min % Achieve", value=90,min=0, max=100,step=5)
                      )# End conditionalPanel
#          ,
                    ),  # End sidebarPanel 

#------------------- mainPanel -------------------------------------------------
      mainPanel(
        tabsetPanel(
#------------------ Yield Goal  Profile ----------------------------------------                
          tabPanel("Yield Goal Analyses",
#                      splitLayout(cellWidths = c("50%", "50%"),
            plotOutput(height='300px','Plt_yield.gl'),
            plotOutput(height='300px','Plt_yield.prof'),
#                      )
            splitLayout(cellWidths = c("50%", "50%"),
#           verbatimTextOutput("byt"),
            verbatimTextOutput("Txt_Yield_gl"))
                       ), #End tabPanel

#------------------ Recruit Goal Profile ---------------------------------------   
          tabPanel("Recruit Goal Analyses",
#                      splitLayout(cellWidths = c("50%", "50%"),
            plotOutput(height='300px','Plt_rec.gl'),
            plotOutput(height='300px','Plt_rec.prof'),
#                      )
            splitLayout(cellWidths = c("50%", "50%"),
#                      verbatimTextOutput("brt"),
            verbatimTextOutput("Txt_Rec_gl"))
                      ),# End tabPanel
              id = "cPanel"
              )#End tabsetPanel
              )#End maiPanel
          ),#End tabPanel Recruit & Yield Goal Analyses

#-------------------------------------------------------------------------------    
#   Custom Escapement Goal Evaluation 
#-------------------------------------------------------------------------------    
    tabPanel("Custom Goal range Analyses",
      sidebarPanel(width = 3,
        p(strong("Select Lower and Upper Escapement Goal")),  
        numericInput("lg", "Lower Goal", value=50000,min=0, step=1000),  
        numericInput("ug", "Upper Goal", value=100000,min=0, step=1000),
        p("Submit Goal Range for  Analyses"),
        actionButton("Run","Run"),
            # Horizontal line ----
          tags$hr(),
        numericInput("rg", "Target Recruit", value=200000,min=0, step=1000),
        numericInput("yg", "Target Yield", value=100000,min=0, step=1000)
            ), #End Sidepar Panel
      
#----------------  mainPanel ---------------------------------------------------      
      mainPanel(
        tabsetPanel(
#------------------ Expected Mean Recruit and Yields ----------------------------   
          tabPanel("Expected Yields Mean & Annual",
              plotOutput(height = '300px', 'Plt_yield.cg'),      
              plotOutput(height='300px',"EGR.Yield"),
              splitLayout(cellWidths = c("50%", "50%"),
                      p(strong("Mean and Annual Yields Summary")),
                      p(strong("Probability of Meeting Target"))),
               splitLayout(cellWidths = c("50%", "50%"),
                      verbatimTextOutput("Txt_Yield_cg"),
                      verbatimTextOutput("Txt_Yield_pb_cg"))
                  ), #End tab Panel

#------------------ Recruit Annual Recruit and Yields ---------------------------------   
          tabPanel("Expected Recruit Mean & Annual",
              plotOutput(height = '300px', 'Plt_rec.cg'),         
              plotOutput(height='300px',"EGR.Rec"),
              splitLayout(cellWidths = c("50%", "50%"),
                      p(strong("Mean and Annual Recruit Summary")),
                      p(strong("Probability of Meeting Target"))),
              splitLayout(cellWidths = c("50%", "50%"),
                     verbatimTextOutput("Txt_Rec_cg"),
                     verbatimTextOutput("Txt_Rec_pb_cg"))
                  ) #End tab Panel
                )#End tabsetPanel
              )#End main Panel 
            )#End tabPanel Custom Goal range Analyses
          ), #End nabVarMenu

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 4  Management Strategy Evaluation 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  navbarMenu("MSE Analyses",
#------------------------------------------------------------------------    
#  Simulation model UI
#------------------------------------------------------------------------  
    tabPanel("Simulation Model",
      sidebarPanel(width = 3,        
        p(strong("Modeling Parameters")),  
        selectInput(inputId="EGm","Escapement Goal", choices = c('Smsy','Smax')),  
        sliderInput("EGlu", label = "Escament Goal Range", min = 0, max = 3, value = c(0.8, 1.6),step=0.1),
        selectInput(inputId="cmode","Fishery Oepnng above Escapement Goal", choices = c('Lower','Middle','Upper')),              
        numericInput(inputId="maxH", "Maximum Harvest", value=100000,min=0,step=10000),
        sliderInput(inputId="maxHr", "Maximum Surplus Harvest Rate", value=0.5,min=0,max=1,step=0.1),
        numericInput(inputId="EGY", "Update Escapement Goal Years", value=6,min=1,step=1)
              ), #End sidebarPanel
      
#---------------- mainPanel ----------------------------------------------------
      mainPanel(
        tabsetPanel(
#-------------------------------------------------------------------------------    
#  Simulation Run 
#-------------------------------------------------------------------------------        
          tabPanel("Simulation Run",
            fluidRow(  
              p(("To compare outcomes of the same startegy, reapeat Simulation and Simuulate")),
              p(("To compare outcomes of different startegy, change strategis and click Simulate"))
                       ),
            fluidRow( 
              column(3, actionButton("InitRun","Initialize")),
              column(3, actionButton("SimRun","Simulate")),
              column(3,actionButton("SimClear","Clear Results"))
                        ),
            fluidRow(  
              plotOutput(height='500px',"runsim"),
              verbatimTextOutput("simsum"),
              plotOutput("simhist")
                            )
                        ), #End tabPanel
          tabPanel("Sim Summary",
              plotOutput('altsim.H'),
              verbatimTextOutput("altsim.sum")
                    ), #End tabPanel
          
          tabPanel("Sim time series",
              plotOutput(height='600px',"altsim.N"), 
              downloadButton("simdownload", "Download")
                    ),  #End tabPanel

#------------------ Model Parameters ---------------------------------   
          tabPanel("Model Parameters",
            fluidPage(
                title = 'Set MSE Simulaiton Initization Parameters',
                hr(),
            fluidRow( 
              column(4,
              p(strong("Simulation Years")),           
              sliderInput(inputId="burnin", "Burnin", value=25,min=0,max=100,step=5,round=0),
              sliderInput(inputId="train", "Training", value=25,min=0,max=100,step=5,round=0),
              sliderInput(inputId="simy", "Management", value=50,min=0,max=100,step=5,round=0)
                    ),
              column(4,
              p(strong("Errors")),       
              sliderInput(inputId="spred", "Preseason Run prediction %E", value=20,min=0,max=100,step=5),
              sliderInput(inputId="simpH", "Management Imprementation %E", value=10,min=0,max=100,step=5),
              sliderInput(inputId="sobsH", "Harvest Observation %E", value=10,min=0,max=100,step=5),
              sliderInput(inputId="sobsE", "Escapement Observation %E", value=30,min=0,max=100,step=5),
              sliderInput(inputId="Nobage", "Age Comp Sample size", value=100,min=10,max=500,step=10)
                    ),
              column(4,
              p(strong("Population Errors")), 
#              sliderInput(inputId="phi", "AR1 correlation", value=0.6,min=0,max=1,step=.1),
#              sliderInput(inputId="D", "Drishelet D", value=50,min=0,max=200,step=10)
                     )
                    ) # End fluidRow
                 )# End fluidOPage
                ) # End tabPanel
              )#End tabsetPanel
            )#End mainPanel
          ),#End tabPanel Simulation Model 

#------------------ Model Descriptions ---------------------------------   
    tabPanel("Model Description",
      tabsetPanel(
#------------------ Model Structure  -----------------------------------             
        tabPanel("Model Structure",
          h2("Model steps"),
          p("Management Strategy Evaluation (MSE) simulation model take follwoing steps"),
          p("1. Set preseason run forecast: Each year, run size is forecasted"),
          p("2. Set preseason harvest target: Based on preseason run size and harvest strategy, harvest target is determined."),
          p("3. Implenent harvet: Harvest strategy is implemented"),
          p("4. Escapement: Escapement is Actual run minus harvest"),
          p("5. Recruitment: Future recuritment is determined by Ricker SR model"),
          p("6. Run: Annual run consists of Recuritment and maturity schedule (brood return age proporion"),
          p("7. Back to step 1."),
          h2("Management stragey"),
          p("Management strategy is based on setting escapement goal."),
          p("Each year annual harvest and escapement is observed (with error), brood table is built, 
                and Ricker SR parmeters are estimated."),
          p("Management target: the model considered two taregets: Smsy,and Smx. Escapement goal range is set as
                  x% below and y% above target."),
          p("Fishery opening triger: the model has 3 trigers: Lower (open fishey when preseson run exceeds
                 lower escapement goal), Middle (open above mid-escapemnt goal), and Upper (open above upper
                 escapement goal"),
          p("Fishery target: Harvestable surplus is preseason run minus escapement goal. The model considers two criteria: 
                maximum harvest and maximum harvest rate.  Maximum harvest is the maximum number of fish harvested. Maximum harvest rate 
                is the maximum surplus harvest rate. 100% harvest rate means that all fish above escapement target will be harvested."),
          p("Frequency of evaulating escapement goal. Model can change escapement goal every x years. Typical bord cycle is 
                every 6 years")
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
          )#End navMenue Panel
        ), #End nabVarPage
#------------------------------------------------------------------------
# Citation Discraimer  
#------------------------------------------------------------------------
  use_bs_tooltip(),
  use_bs_popover(),
# withMathJax(),
    hr(),
  h5("Disclaimer"),
  print(strong("This App is developed by Toshihide Hamachan Hamazaki, Alaska Department of Fish and Game Division of Commercial Fisheries")),
  h5("Contact about this applicaiton"), 
  print(strong("Questions and improvement suggestions? Please contact",
             a(href="mailto:toshihide.hamazaki@alaska.gov", "Hamachan"))),
  h6("Update"),
  print("01/22/2020: Added Brood Year analyses data range. Now user can chosse analysis based on selected range."),
  h5("Suggested Citation"),
  print(strong(paste("Hamazaki, T.",format(Sys.Date(), "%Y"),". Spawner-Recruit Analyses (source: https://hamachan.shinyapps.io/Spawner_Recruit/"))),
h5("Other Models"),
  p(strong("Bayesian SR model:",
             a(href="https://hamachan.shinyapps.io/Spawner_Recruit_Bayes/", "Bayes"))),
  p(strong("Missed escapement passage estimation:",
             a(href="https://hamachan.shinyapps.io/Missed_Run/", "Missed Passage")))
)#End fluidPage


#===============================================================================    
#  Server:  
#===============================================================================
server<-shinyServer(function(input, output, session){
#-------------------------------------------------------------------------------
#  Control MSE Analysis page.
#  This is page will show up only when Data type is Run
#-------------------------------------------------------------------------------  
  observe({
    if(input$dataType=='S-R') {
      hideTab(inputId = "tabs", target = "MSE Analyses")
      hideTab(inputId = "tabs", target = "Brood Table")
      }
    else {
    showTab(inputId = "tabs", target = "MSE Analyses")
    showTab(inputId = "tabs", target = "Brood Table")
      }
   })

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 1: Data upload and output 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  output$note <- renderUI({
    if(input$dataType== "S-R"){
      paste("S-R Data file column orders: Year, Spawner (Escapement), Recruit")
    } else {
      paste("Run Data file column orders: Year, Escapement, Run,
                   Run by age (or proportion) from youngest to oldest")
    }
  })
  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Data upload and output 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Data file reading module     
  data <- callModule(dataInput,"datain",stringsAsFactors = FALSE)
  
# table ---- Uploaded data table output ----------------------------------------
  output$Tbl_data <- renderDataTable({data()})  
  
# brood.table--- Construct brood table (when dataType is "Run") --------------------   
  brood.out <-  reactive({
    if(input$dataType== "Run"){
      brood <- make.brood(data(),input$fage)
      return(brood)
    } else{NA}
  })
  
# Tbl_data.brood ----- show brood table ------------------------------------------------
  output$Tbl_data.brood <- renderDataTable({
    if(input$dataType== "Run"){
      round(brood.out()$brood,0)
    }  
  }) 
  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Create SR data 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Orginal SR data -------------------------------------------------------------  
  sr.data.0 <- reactive({
    req(input$dataType)
    if(input$dataType== "Run"){
      x <- brood.out()$SR
    } else {
      x <- data()
    }
    names(x) <- c('Yr','S','R')
    return(x)     
  })

#---- UI Output User determined year range -------------------------------------
output$yrange = renderUI({
  year <- sr.data.0()$Yr
  fyear <- min(year)
  lyear <- max(year)
  sliderInput("sryears", label = "year range", min = fyear, 
              max = lyear, value = c(fyear, lyear),step=1,sep = "")
  })

# Sr data: this is used for analyses. ------------------------------------------
sr.data <- reactive({
    x <- sr.data.0()
    fyear <- input$sryears[1]
    lyear <- input$sryears[2]    
    x <- x[x$Yr>=fyear & x$Yr<=lyear,]
  return(x)     
  })

  

# Data summary output disply ---------------------------------------------------
output$Txt_sum.data <- renderPrint({
      dat <- sr.data()
      dat$Y <- dat$R-dat$S
      summary(dat[,-1])
    })

# SR data Histogram------------------------------------------------------------- 
output$Plt_hist.sry <- renderPlot({
  par(mfrow=c(1,3))
  x <- sr.data()
  x$Y <- x$R-x$S
  hist(x$S,main='',xlab='Spawnter')
  hist(x$R,,main='',xlab='Recruit')
  hist(x$Y,,main='',xlab='Yield')  
  })

# Plt_srt ------------- Brood S-R  time serise -------------------------------------  
  output$Plt_srt <- renderPlot({
    x <- sr.data.0()
    u <- as.numeric(input$ui)
    mult <- mult(u)
    par(yaxs='i',bty='l')
    plot(R/u~Yr,data=x,type='l',ylim=c(0,with(x,max(R,S)/u)),
         #       main=input$caption,
         xlab='Brood Year',
         ylab=paste('Spawner / Recruit',mult))
    lines(S/u~Yr,data=x,lty=2)
    legend('topright',c('Spawner','Recruit'),lty=c(2,1),box.lty=0)  
    # Add Escapement Goal range  
    if(input$sryears[1]>min(x$Yr)|input$sryears[2]>max(x$Yr)){
      abline(v=input$sryears[1],col=2) 
      abline(v=input$sryears[2],col=2)  
    }
  })
  
#-------------------------------------------------------------------------------
#  Plot runesc - Run escapement   time serise
#------------------------------------------------------------------------------- 
  output$Plt_runesc <- renderPlot({
    x <- data()[,c(1:3)]
    names(x) <-c('Yr','S','R')
    u <- as.numeric(input$ui)
    mult <- mult(u)
    par(yaxs='i',bty='l')
    plot(R/u~Yr,data=x,type='l',ylim=c(0,with(x,max(R,S)/u)),
         main=input$caption,
         xlab='Year',
         ylab=paste('Run / Escapement',mult))  
    lines(S/u~Yr,data=x,lty=2)
    legend('topright',c('Run','Escapement'),lty=c(1,2),box.lty=0)  
  })
  
  
#------------------ Data Download   ---------------------------------  
output$downloadData <- downloadHandler(
    filename = function() {"broodtable.csv"},
    content = function(file) {
      write.csv(brood.out()$brood, file, row.names = FALSE)
    })
  
plotdownload <- function(plots) {
    plotout <- downloadHandler(filename = function() {
      paste("plot",input$Doption,sep= ".")},
      content = function(file){
        # open the format of file which needs to be downloaded ex: pdf, png etc. 
        if (input$Dooption == "png"){
          png(file)
        } else if (input$Dooption == "pdf"){
          pdf(file)
        } else {
          jpeg(file) 
        }
        plots
        dev.off()
      }
    )
    return(plotout)
  }
  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 2: SR Data Analyses and Output  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------
#  1.0: Create SR Model   
#-------------------------------------------------------------------------------
SRM <- reactive({
  x <- sr.data()
  D <- floor(mean(log10(x$S)))
  x$s <- x$S/(10^D)
# Regression no AR1
  model.s <- gls(log(R/S)~s,data=x,method='ML')
# Regression with AR1
  model.ar1 <- gls(log(R/S)~s,data=x,correlation=corAR1(form=~1),method='ML')
# ANOVA test 
  anova <- anova(model.s,model.ar1)
  out <- list(model.s,model.ar1,anova)
  return(out)
  })   

# SR =============  SR model selected by ANOVA Test ============================  
SR <- reactive({
  model.s <- SRM()[[1]]
  model.ar1 <- SRM()[[2]]
  anova <- SRM()[[3]]
# Choose SR   
    if(input$SRM=='Standard') { model <- model.s }
    else if (input$SRM=='AR1') { model <- model.ar1 }
    else {
# Automatic selection based on ANOVA      
      if(anova[[9]][2] < 0.05) {model <- model.ar1}
     else
      { model <- model.s }
    }
    return(model)
  })  

#==============  Output ANOVA Table  ===========================================
output$Txt_ANOA <- renderPrint({
  list(likelihood_Ratio_Test=SRM()[[3]],
       Model_Summary=summary(SR()),
       Parmeter_Intervals=intervals(SR()))
})

#==============  Output Selected SR Model ======================================
output$modelslct <- renderText({
  anova <- SRM()[[3]]
  if(input$SRM=='Standard') { model <- 'Standard Ricker' }
  else if (input$SRM=='AR1') { model <- 'AR1 Ricker'}
  else {
    if(anova[[9]][2] < 0.05) {model <- 'AR1 Ricker'}
      else
      { model <-'Standard Ricker' }
    }
  paste('Model Selected:',model)
  })


#-----------------------------------------------------------------------
#  2.0: Output SR Parameters
#-------------------------------------------------------------------------------
SR.out <- reactive({
  D <- floor(mean(log10(sr.data()$S)))
  ln.alpha <- coef(SR())[1]
  alpha <- exp(ln.alpha)
  beta <- -coef(SR())[2]/(10^D) 
  sigma <- sigma(SR())
  Seq <- ln.alpha/beta
  Smsy <- Seq*(0.5-0.07*ln.alpha)
  Umsy <- ln.alpha*(0.5-0.07*ln.alpha)
  Rmsy <- Smsy*exp(ln.alpha-beta*Smsy)
  MSY <- Rmsy-Smsy
  Smax <- 1/beta
  Rmax <- exp(ln.alpha-1)/beta
  out <- data.frame(t(c(ln.alpha,alpha, beta, sigma ,Seq,Smsy,Umsy,Rmsy,MSY,Smax,Rmax)))
  names(out) <- c('lnalpha','alpha', 'beta', 'sigma','Seq','Smsy','Umsy','Rmsy','MSY','Smax','Rmax')
  return(out)
  })

#============  Output  Model Parameters ======================================== 
output$Txt_sr.pars <- renderPrint({
  print(SR.out(),digits=c(3,3,10,3,3,0,0,0,0,0,0))
  })  

#-------------------------------------------------------------------------------
#  3.0: SR.pred.m: Parametric Model based predicted mean, CI, PI
#-------------------------------------------------------------------------------
SR.pred.m <- reactive({
# Import model parameters
  par <-SR.out()
# Import CI %
  mp <- input$CI/100
# Prediction model s range  
  D <- floor(mean(log10(sr.data()$S)))
  max.s <- max(par$Seq,max(sr.data.0()$S))
  max.b <- ceiling(max.s/(10^D))
  sd <- seq(0,max.b,length.out=201)
  s <- sd*(10^D)
# Predicttion 
  pred <- predictSE(SR(), newdata=data.frame(s=sd))
# Predicted ln(R/S)
  lRS <- pred$fit
# Model SE
  se <- pred$se.fit
# Model st.Residual
  res <- sd(as.vector(SR()$residuals))
# Model df  
  foo <-as.data.frame(SR()$dims)
  dft <- foo$N-foo$p
# Calculatge SE for Prediction interval
  pse <- sqrt(se^2+res^2)
# Calculate tdist
  tf <- qt((1-mp)/2,dft,lower.tail=FALSE)
# Calculate CI-PI
  if (input$Li =='confidence') {
  lwr <- lRS -tf*se
  upr <- lRS +tf*se
  }
  else {
  lwr <- lRS -tf*pse
  upr <- lRS +tf*pse
  }
# Calculate parametric model expected return 
  ER <- exp(cbind(lRS,lwr,upr))*s
  out <- data.frame(cbind(s,ER))
  names(out) <- c('S','fit','lwr','upr')
  return(out)
  })  

#===============================================================================
#  Standard SR and Yield Plots and Tables Outputs  
#=============================================================================== 
base.pl <- reactive({
  u <- as.numeric(input$ui)
  mult <- mult(u)
  x <- sr.data.0()
  xp <- x/u
  SRp <- SR.pred.m()/u
  #---  Basic SR plot ------------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l')
  plot(R~S,data=xp,pch=1,col=1, 
       main= input$caption,
       xlab=paste("Escapement",mult),ylab=paste('Recruit',mult),
       xlim=c(0,max(SRp$S)),ylim=c(0,1.1*max(xp$R)))
  # Plot 1:1 line 
  abline(0,1)
  x2 <- sr.data()
  xp2 <- x2/u
  points(R~S,data=xp2,pch=19,col=1)    
  # Add Predicted   
  lines(fit~S,data=SRp,col=1,lw=2)  
  out1 <-recordPlot()
#-------------------------------------------------------------------------------
#---  Basic Yield plot ---------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l')
  plot((R-S)~S,data=xp,pch=1,col=1, 
       main= input$caption,
       xlab=paste("Escapement",mult),ylab=paste('Yield',mult),
       xlim=c(0,max(SRp$S)),ylim=c(min(SRp$lwr-SRp$S),1.1*max(xp$R-xp$S)))
  points((R-S)~S,data=xp2,pch=19,col=1)  
  lines((fit-S)~S,data=SRp,col=1,lw=2)
  abline(h=0)
#-------------------------------------------------------------------------------
#----  Plots Output ------------------------------------------------------------  
  out2 <-recordPlot()  
  return(list(base.p=out1,base.py=out2))
})


base.p <- reactive({base.pl()$base.p})

base.py <- reactive({base.pl()$base.py})


#===============================================================================
# Panel 1: SR Analyses SEction 
#===============================================================================
#-------------------------------------------------------------------------------
#  SR plot 
#-------------------------------------------------------------------------------
srplot <- function(){
  u <- as.numeric(input$ui)
  x <- sr.data.0()
  par <-SR.out()
  xp <- x/u
  SRp <- SR.pred.m()[,1:4]/u
  # Draw Base SR Plot
  replayPlot(base.p())
  # Add CI    
  if(input$show.int==TRUE){
    with(SRp,polygon(c(S,rev(S)),c(upr,rev(lwr)),col=tcol('grey',50),border=NA))
  }
  # Add Years
  if(input$show.points==TRUE) {
    pointLabel(xp$S,xp$R, labels=as.character(x$Yr), cex= 1,col=4)}

  # Add Smsy
  t1 <- ''
  l1 <- 0
  if(input$show.smsy==TRUE) {
    abline(v=par$Smsy/u,col=1,lty=2)
    t1 <- 'Smsy'
    l1 <- 2
  }
  # Add Smax       
  t2 <- ''
  l2 <- 0
  if(input$show.smax==TRUE) {
    abline(v=par$Smax/u,col=1,lty=3)
    t2 <- 'Smax'
    l2 <- 3
  }
  legend('topright',c(t1,t2),lty=c(l1,l2),bty='n')    
  }

# Plt_SR ----------- Plot SR --------------------------------------------------
output$Plt_SR <- renderPlot({ srplot()})

#**************** SR Plot Download *********************************************
output$down <- downloadHandler(
  filename = function() {
    paste("myreport","png", sep = ".")
      },
    content = function(file){
    #    if(input$report == "png")
    png(file)
    #    else
    #      pdf(file)
    srplot()
    dev.off()
    }
  )
#*******************************************************************************

#-------------------------------------------------------------------------------
#  SR Yield plot 
#-------------------------------------------------------------------------------
output$Plt_yield <- renderPlot({
  u <- as.numeric(input$ui)
  x <- sr.data()
  par <-SR.out()
  SRp <- SR.pred.m()[,1:4]/u
  xp <- x/u
  # Plot base Yiled plot
  replayPlot(base.py())
  # Add CI    
  if(input$show.int==TRUE){
    with(SRp,polygon(c(S,rev(S)),c(upr-S,rev(lwr-S)),col=tcol('grey',50),border=NA))
  }
  # Add Years
  if(input$show.points==TRUE) {
    pointLabel(xp$S,(xp$R-xp$S), labels=as.character(x$Yr), cex= 1,col=4)
    #    text((R-S)~S,data=xp, labels=x$Yr, cex= 1, pos=3,col=4)
  }

  # Add Smsy 
  t1 <- ''
  l1 <- 0
  if(input$show.smsy==TRUE) {
    abline(v=par$Smsy/u,col=1,lty=2)
    t1 <- 'Smsy'
    l1 <- 2
  }
  # Add Smax       
  t2 <- ''
  l2 <- 0
  if(input$show.smax==TRUE) {
    abline(v=par$Smax/u,col=1,lty=3)
    t2 <- 'Smax'
    l2 <- 3
  }
  # Add legend  
  legend('topright',c(t1,t2),lty=c(l1,l2),box.lty=0)    
  })

#-------------------------------------------------------------------------------
#  Plot Residual Plot
#-------------------------------------------------------------------------------  
output$Plt_residual <- renderPlot({
  year <- sr.data()$Yr
  resid <-residuals(SR())
  # Prediction model s range  
  D <- floor(mean(log10(sr.data()$S)))
  S <- sr.data()$S
  sd <- S/(10^D)
  # Predicttion 
  pred <- predictSE(SR(), newdata=data.frame(s=sd))
  # Model SE
  se <- pred$se.fit
  ciu <- resid + 2*pred$se.fit
  cil <- resid - 2*pred$se.fit
  par(bty='l')
  plot(resid~year,xlab='Year',type='l', main='Residuals',
       ylab='Residuals',ylim=c(min(cil),max(ciu)))
  abline(h=0)
#  model <- gam(resid~s(year),family=gaussian, fit =TRUE)
#  pred.year <- data.frame(year, predict.gam(model,se = TRUE))    
#  lines(pred.year$year, pred.year$fit,lwd=2,col=4)
  polygon(c(year,rev(year)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA)
  })

# Plt_predict --- Plot Preidcted Plot -------------------------------------------
output$Plt_predict <- renderPlot({
  year <- sr.data()$Yr
  # Prediction model s range  
  D <- floor(mean(log10(sr.data()$S)))
  S <- sr.data()$S
  sd <- S/(10^D)
  R <- sr.data()$R
  lRS <- log(R/S)
  # Predicttion 
  pred <- predictSE(SR(), newdata=data.frame(s=sd))
  # Model SE
  ciu <- pred$fit + 2*pred$se.fit
  cil <- pred$fit - 2*pred$se.fit
  par(bty='l')
  plot(pred$fit~year,xlab='Year',type='l',lwd=2,ylab='Recruit',main='Prediction',
       ylim =c(min(cil,lRS),max(ciu,lRS)))
  lines(lRS~year,type='l',lwd=2,col=4)
  polygon(c(year,rev(year)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA)
  legend('topright',legend=c('Observed','Predicted '),lty=c(1,1), col=c(4,1),bty ='n')
})
#===============================================================================
#  Durbin-Watson auto-correlaiton Test 
#===============================================================================
output$Txt_dwtest <- renderPrint({
  year <- sr.data()$Yr
  resid <-residuals(SR())
  dwtest(resid~year)
})

#===============================================================================
#  Bootstrap Analyses 
#===============================================================================
boot.SR <- callModule(Bootrep,'Boots',SR,sr.data)

#-----------------------------------------------------------------------
#  SR Bootstrap Parameters Distribution   
#-----------------------------------------------------------------------
# SR.post --- Create SR parameters: alpha, beta, Seq, Smsy, Umsy, Smax ---------
SR.post <- reactive({
      D <- floor(mean(log10(sr.data()$S))) 
      # Read mcmc data
      boot <- as.matrix(boot.SR())
      post <- sim.out(boot,D)
      return(post)
  })

    
# sumpost --- SR Parameter Summaries output------------------------------------- 
output$Txt_sum.b <- renderPrint({
    parname <-c('alpha','lnalpha','beta','Seq','Smsy','Umsy','Smax')
    summary(SR.post()[,parname],digits=c(3,3,3,0,0,0,0))
  })
    
# Plt_hist.b --- SR Parameters Density plots ------------------------------------- 
output$Plt_hist.b <- renderPlot({
    D <- floor(mean(log10(sr.data()$S))) 
    ar1 <- ifelse(mean(SR.post()$phi)==0,'FALSE','TRUE')
    plot_density(SR.post(),D,ar1)
  })
    

#-----------------------------------------------------------------------
#  2.0 Bootstrap SR prediction, CI and PI
#-----------------------------------------------------------------------
# SR.pred ---- Simulaiton Model Prediction -------------------------------------
# SR.pred outputs  List --------------------------------------------------------
# S: Simulation Spawner range (Vector)
# R: Simulation predicted Mean Recruit (Matrix)
# R.p: Simulation predicted Annual Recruit (Matrix)
# Y: Simulation predicted Mean Yield (Matrix)
# Y.p: Simulation predicted Annual Yield (Matrix)
SR.pred <-reactive({
  srmodel <- SR.CR   # Ricker SR model 
#---------- Extract Simulation SR Model Parameters -----------------------------
  D <- floor(mean(log10(sr.data()$S))) 
#---------- Determine model S length -------------------------------------------
  Seq <- quantile(SR.post()$Seq,0.9)   # Extract 90 percentile Seq
  max.s <- max(Seq,max(sr.data.0()$S)) # Extract max spanwer 
  out <- SR.pred.sim(SR.post(),D,max.s,srmodel)  
  return(out)
})  
#-------------------------------------------------------------------------------
# SRp ------ Model predicted mean, CI, PI  SR range ----------------------------
# SRp outputs Data.frame -------------------------------------------------------
# S: Simulation Spawner range 
# RS.md: predicted Median Recruit
# RS.me: predicted Mean Recruit 
# Rl:  predicted Lower CI 
# Ru:  predicted Upper CI 
# Rl.p:  predicted Lower PI 
# Ru.p:  predicted Upper PI 
SRp <- reactive({
  out <- pred_CI(SR.pred(),input$CI)
  return(out)
})  

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

#---------------------------------------------------------------------------
#  Plot Base Predicted Yield 
#---------------------------------------------------------------------------
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
#  Panel 3: Smsy-Smax based Escapement Goal  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#===============================================================================
#  Smsy Goal Analyses 
#===============================================================================
smsyprof <- callModule(Smsyprof,"smsy",SR.pred)
# EG.Smsy -------  User defined Smsy profile based goal data -------------------
EG.Smsy <- reactive({smsyprof$EG.Smsy()})
EG.Smsy.st <- reactive({smsyprof$EG.Smsy.st()})
SA.BEG  <- reactive({smsyprof$SA.BEG()})
p.msy <- reactive({smsyprof$p.msy()})
p.msy.t <- reactive({smsyprof$p.msy.t()})

# Plt_Smsy.prof ----- Smsy Optimum Profile Plot --------------------------------
smsy_profile <- function(){
  # Import minimum Smsy %
  p.msy <- as.numeric(p.msy())/100
  # Import minimum % achieving Smsy  
  p.msy.t <- as.numeric(p.msy.t())/100  
  u <- as.numeric(input$ui)
  # Import S,   
  S <- EG.Smsy()$S
  Y.prof <- EG.Smsy()$S.prof
  Y.prof.st <- EG.Smsy.st()$Smsy.prof.st
  plot_prorile('MSY',Y.prof,Y.prof.st,S,p.msy,p.msy.t,minpt,0.9,u)
}

# Smsy.yield -------------------------------------------------------------------
smsy_yield <- function(){
  # Import S range   
  Sragge <- EG.Smsy()$S.Range 
  Y.prof <- EG.Smsy()$S.prof
  Y.prof.st <- EG.Smsy.st()$Smsy.prof.st
  plot_prorile('MSY',Y.prof,Y.prof.st,S,p.msy,p.msy.t,minpt,0.9,u)
}


#===============================================================================
#  Smax Goal Analyses 
#===============================================================================
smaxprof <- callModule(Smaxprof,"smax",SR.pred,as.numeric(input$ui))
# EG.Smsy -------  User defined Smsy profile based goal data -------------------
EG.Smax <- reactive({smaxprof$EG.Smax()})
EG.Smax.st <- reactive({smaxprof$EG.Smax.st()})
SM.BEG  <- reactive({smaxprof$SM.BEG()})   # Smax based goal range
p.max <- reactive({smaxprof$p.max()})
p.max.t <- reactive({smaxprof$p.max.t()})

# Plt_Smax.prof ----- Smax Optimum Profile Plot ------------------------------------
smax_profile <- function(){
  # Import minimum Smsy %
  p.max <- as.numeric(p.max())/100
  # Import minimum % achieving Smsy  
  p.max.t <- as.numeric(p.max.t())/100   
  u <- as.numeric(input$ui)
  S <- EG.Smax()$S
  R.prof.st <- EG.Smax.st()$Smax.prof.st
  R.prof <- EG.Smax()$S.prof
  plot_prorile('Rmax',R.prof,R.prof.st,S,p.max,p.max.t,minpt,0.9,u)
}


#===============================================================================
#  Smsy-Smax Goal Analyses Output 
#===============================================================================
#  Profile Plot   
output$Plt_Smsy.prof <- renderPlot({
  smsy_profile()
  u <- as.numeric(input$ui)
  S <- EG.Smsy()$S.Range/u
  polygon(c(S,rev(S)),c(c(0,0),c(1,1)),col=tcol(3,80),border=NA)
})    
output$Plt_Smax.prof <- renderPlot({
  smax_profile()
  u <- as.numeric(input$ui)
  S <- EG.Smax()$S.Range/u
  polygon(c(S,rev(S)),c(c(0,0),c(1,1)),col=tcol(4,80),border=NA)
})    

# Yield and Recruit Plot -------------------------
output$Plt_rec.pg <- renderPlot({
  u <- as.numeric(input$ui)
  x <- sr.data()
  xp <- x/u
  SRp <- SRp()/u
  replayPlot(base.r())
  Srange1 <- EG.Smsy()$S.Range/u
  Srange2 <- EG.Smax()$S.Range/u
  ymin <-c(0,0)
  ymax <-c(1.1*max(xp$R),1.1*max(xp$R))
  polygon(c(Srange1,rev(Srange1)),c(ymin,ymax),col=tcol(3,80),border=NA)
  # Plot escapement goal range 
  if(!is.na(sum(Srange2))) {
  polygon(c(Srange2,rev(Srange2)),c(ymin,ymax),col=tcol(4,80),border=NA)
  }
})

output$Plt_yield.pg <- renderPlot({
  u <- as.numeric(input$ui)
  x <- sr.data()
  xp <- x/u
  SRp <- SRp()/u
  replayPlot(base.y())
  Srange1 <- EG.Smsy()$S.Range/u
  Srange2 <- EG.Smax()$S.Range/u
  ymin <-c(min(SRp$Rl-SRp$S),min(SRp$Rl-SRp$S))
  ymax <-c(1.1*max(xp$R-xp$S),1.1*max(xp$R-xp$S))
  polygon(c(Srange1,rev(Srange1)),c(ymin,ymax),col=tcol(3,80),border=NA)
  # Plot escapement goal range 
  if(!is.na(sum(Srange2))) {
    polygon(c(Srange2,rev(Srange2)),c(ymin,ymax),col=tcol(4,80),border=NA)
  }
})

# Txt_Srange.smsy -------- Smsy goal output ------------------------------------
output$Txt_Srange.smsy <-renderText({
  paste(SA.BEG()$tst1,SA.BEG()$tst2,SA.BEG()$tst3,SA.BEG()$t1,sep='\n')  
})
# Txt_Srange.smax -------- Smax Goal range output table ------------------------
output$Txt_Srange.smax <-renderText({
  paste(SM.BEG()$tst1,SM.BEG()$tst2,SM.BEG()$tst3,SM.BEG()$t1,sep='\n')
})


#---------------------------------------------------------------------------
#  Smsy Yield and Recruit Plot
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#  Smsy Escapement based Yield and Recruit Distribution 
#---------------------------------------------------------------------------
SR.Smsy <- reactive({
  BEG.1 <- EG.Smsy()$S.Range[1]
  BEG.2 <- EG.Smsy()$S.Range[2]
  S <- SR.pred()$S
  sr <- S[S>=BEG.1 & S<=BEG.2]
  Y <- SR.pred()$Y[,S%in%sr] 
  Y.p <- SR.pred()$Y.p[,S%in%sr]
  R <- SR.pred()$R[,S%in%sr] 
  R.p <- SR.pred()$R.p[,S%in%sr] 
  out <- list(S = S, Y = Y, R = R, Y.p = Y.p, R.p = R.p)
  return(out)
 })

#-----------------------------------------------------------------------
#  Plot distribution of Recruit and Yield at Given Escapement Range
#-----------------------------------------------------------------------
output$Plt_Smsy.dist <- renderPlot({
  par(mfrow=c(2,1),xaxs='i',yaxs='i',bty='l')
  u <- as.numeric(input$ui)
  mult <- mult(u)
  # Mean estimate   
  # Bootstrap estimate  
#  plot(density(SR.Smsy()$R/u),main='Expected Mean Recruit',xlab=paste("Recruit",mult),ylab='')
  # Annualestimate   
  R.p <-SR.Smsy()$R.p/u
  R <- SR.Smsy()$R/u
  Y.p <-SR.Smsy()$Y.p/u
  Y <- SR.Smsy()$Y/u
  if(length(remove.out(R.p))>0) R.p <- R.p[remove.out(R.p)]
  if(length(remove.out(Y.p))>0) Y.p <- Y.p[remove.out(Y.p)]  
  mult.den.plt(R.p,R,'Expected Mean & Annual Yield',paste("Yield",mult))
  mult.den.plt(Y.p,Y,'Expected Mean & Annual Recruit',paste("Recruit",mult))
  
#  foo <- data.frame(Annual=R.p,Mean=R)/u
#  boxplot(foo,horizontal = TRUE, ylim = c(0, 0.6*max(foo)),xlab=paste('Recruit',mult))
#  foo2 <- data.frame(Annual=as.vector(SR.Smsy()$Y.p),Mean=as.vector(SR.Smsy()$Y))/u
#  boxplot(foo2,horizontal = TRUE, ylim = c(min(foo2), 0.6*max(foo2)),xlab=paste('Yield',mult))  
  # Bootstrap estimate
#  plot(density(R.p/u), main='Expected Annual Recruit',xlab=paste("Recruit",mult),ylab='')
  }) 

#-------------------------------------------------------------------------------
#  Summary 
#-------------------------------------------------------------------------------
output$Txt_sum.smsy <- renderPrint({
  u <- as.numeric(input$ui)
  mult <- mult(u)
  dat <- data.frame(R.Annual=as.vector(SR.Smsy()$R.p),R.Mean=as.vector(SR.Smsy()$R),
                    Y.Annual=as.vector(SR.Smsy()$Y.p),Y.Mean=as.vector(SR.Smsy()$Y))
  print(summary(dat,digits=c(0,0,0,0)))
  })

#---------------------------------------------------------------------------
#  Smas Yield and Recruit Plot
#---------------------------------------------------------------------------
#---------------------------------------------------------------------------
#  Smax Escapement based Yield and Recruit Distribution 
#---------------------------------------------------------------------------
SR.Smax <- reactive({
  BEG.1 <- EG.Smax()$S.Range[1]
  BEG.2 <- EG.Smax()$S.Range[2]
  S <- SR.pred()$S
  sr <- S[S>=BEG.1 & S<=BEG.2]
  Y <- SR.pred()$Y[,S%in%sr] 
  Y.p <- SR.pred()$Y.p[,S%in%sr] 
  R <- SR.pred()$R[,S%in%sr] 
  R.p <- SR.pred()$R.p[,S%in%sr] 
  out <- list(S = S, Y = Y, R = R, Y.p = Y.p, R.p = R.p)
  return(out)
  })

#-------------------------------------------------------------------------------
#  Plot distribution of Recruit and Yield at Given Escapement Range
#-------------------------------------------------------------------------------
output$Plt_Smax.dist <- renderPlot({
  par(mfrow=c(2,1),xaxs='i',yaxs='i',bty='l', cex=1)
  u <- as.numeric(input$ui)
  mult <- mult(u)
  R.p <-SR.Smax()$R.p/u
  R <- SR.Smax()$R/u
  Y.p <-SR.Smax()$Y.p/u
  Y <- SR.Smax()$Y/u
  if(length(remove.out(R.p))>0) R.p <- R.p[remove.out(R.p)]
  if(length(remove.out(Y.p))>0) Y.p <- Y.p[remove.out(Y.p)]  
  mult.den.plt(R.p,R,'Expected Mean & Annual Yield',paste("Yield",mult))
  mult.den.plt(Y.p,Y,'Expected Mean & Annual Recruit',paste("Recruit",mult))
    # Mean estimate   
  # Bootstrap estimate  
  #  plot(density(SR.Smsy()$R/u),main='Expected Mean Recruit',xlab=paste("Recruit",mult),ylab='')
  # Annualestimate   
  #foo <- data.frame(Annual=as.vector(SR.Smax()$R.p),Mean=as.vector(SR.Smax()$R))/u
  #boxplot(foo,horizontal = TRUE, ylim = c(0, 0.6*max(foo)),
  #        xlab=paste('Recruit',mult))
  #foo2 <- data.frame(Annual=as.vector(SR.Smax()$Y.p),Mean=as.vector(SR.Smax()$Y))/u
  #boxplot(foo2,horizontal = TRUE, ylim = c(min(foo2), 0.6*max(foo2)),
  #        xlab=paste('Yield',mult))  
  # Bootstrap estimate
  #  plot(density(R.p/u), main='Expected Annual Recruit',xlab=paste("Recruit",mult),ylab='')
}) 

#-------------------------------------------------------------------------------
#  Summary 
#-------------------------------------------------------------------------------
output$Txt_sum.smax <- renderPrint({
  u <- as.numeric(input$ui)
  mult <- mult(u)
  dat <- data.frame(R.Annual=as.vector(SR.Smax()$R.p),R.Mean=as.vector(SR.Smax()$R),
                    Y.Annual=as.vector(SR.Smax()$Y.p),Y.Mean=as.vector(SR.Smax()$Y))
  print(summary(dat,digits=c(0,0,0,0)))
  })

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  4.0 Target Yield based Escapement Goal
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#-------------------------------------------------------------------------------
#  SR.porf.m: Parameteric methods creating profiles 
#-------------------------------------------------------------------------------
SR.prof.m <- reactive({
  # Import recruit and yield goal   
  rg <- ifelse(is.null(input$r1),median(sr.data()$R),input$r1)
  yg <- ifelse(is.null(input$y1),median(sr.data()$R-sr.data()$S),input$y1)
  # Prediction model s range  
  D <- floor(mean(log10(sr.data()$S)))
  S <- SR.pred()$S
  sd <- S/(10^D)
  # Predicttion 
  pred <- predictSE(SR(), newdata=data.frame(s=sd))
  # Predicted ln(R/S)
  lRS <- pred$fit
  # Model SE
  se <- pred$se.fit
  # Model st.Residual
  res <- sd(as.vector(SR()$residuals))
  # Model df  
  foo <-as.data.frame(SR()$dims)
  dft <- foo$N-foo$p
  # Calculatge SE for Prediction interval
  pse <- sqrt(se^2+res^2)
#-----------------------------------------------------------------------
# Calculate Parameteric probability of achieving target run and yields
# at given escapement based on t-distribution. 
#-----------------------------------------------------------------------  
  # ln(R/S) at given Run target
  erg <- log(rg/S)
  # ln(R/S) at given Yield target
  eyg <- log(yg/S + 1)
  # Probability of achiving target Rercuits: CI (mean) and PI (annual) 
  prof.Rci <- 1-pt((erg-lRS)/se,dft)
  prof.Rpi <- 1-pt((erg-lRS)/pse,dft)
  # Probability of achiving target Yield: CI (mean) and PI (annual)
  prof.Yci <- 1-pt((eyg-lRS)/se,dft)
  prof.Ypi <- 1-pt((eyg-lRS)/pse,dft)
  out <- data.frame(cbind(S,prof.Rci,prof.Rpi,prof.Yci,prof.Ypi ))
  names(out) <- c('S','prof.Rci','prof.Rpi','prof.Yci','prof.Ypi')
  return(out)
})  


#---- UI Output----------------------------------------------------------------------
output$minYield = renderUI({
  msy <- median(sr.data()$R-sr.data()$S)
  D <- floor(log10(msy))
  # This makes largest numbers into integer (e.g. 100000)
  imsy <- 0.6*ceiling(msy/(10^D))*10^D  
  numericInput("y1", "Min Mean Yield", value=imsy,min=0, step=(10^(D-1)))
})

#-----------------------------------------------------------------------
#  3.0 Bootstrap Recruit & Yields Curve  Out
#-----------------------------------------------------------------------  
b.YA <- reactive({
  # Yield goal
  mp <- (1-input$p.i/100)/2
  boot.s <- Y.boot()$boot.s 
  boot.Y <- as.matrix(Y.boot()$Y.boot)
  boot.Ym <- colMeans(boot.Y)
  boot.Yu <- apply(boot.Y,2,function(x) quantile(x,1-mp))
  boot.Yl <- apply(boot.Y,2,function(x) quantile(x,mp))
  # Recruit goal  
  boot.R <- as.matrix(Y.boot()$R.boot)
  boot.Rm <- colMeans(boot.R)
  boot.Ru <- apply(boot.R,2,function(x) quantile(x,1-mp))
  boot.Rl <- apply(boot.R,2,function(x) quantile(x,mp))
  out <- data.frame(cbind(boot.s,boot.Ym,boot.Yu,boot.Yl,boot.Rm,boot.Ru,boot.Rl))
  return(out)
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  4.0 Target Yield and Recruit based Escapement Goal
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
Yield_gl_sim <- reactive({
  # Import User defined Yield Gaol 
  yg <- input$y1        # Target minimum yields 
  # Import MCMC Expected mean Yields 
  mc.Y <- SR.pred()$Y
  # Import MCMC Expected annual Yield 
  mc.Y.p <- SR.pred()$Y.p  
  # Import S 
  S <- SR.pred()$S
  # For each simulation, determin if expected yields exceed desired goal  
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
#-------------------------------------------------------------------------------  
# Add Parametric model probile 
#-------------------------------------------------------------------------------  
  s <- SR.prof.m()$S/u
  Yci <- SR.prof.m()$prof.Yci
  Ypi <- SR.prof.m()$prof.Ypi  
  lines(s,Yci,col='grey')  
  lines(s,Ypi,col='grey',lty=2)  
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
  x <- sr.data()
  xp <- x/u
  SRp <- SRp()/u
  replayPlot(base.y())
  yg <- input$y1/u
  BEG.p <- Yield_gl()/u 
  abline(h=yg,lwd=2,col=2)
  ymin <-c(min(SRp$Rl-SRp$S),min(SRp$Rl-SRp$S))
  ymax <-c(1.1*max(xp$R-xp$S),1.1*max(xp$R-xp$S))
  polygon(c(BEG.p[1,],rev(BEG.p[1,])),c(ymin,ymax),col=tcol(3,80),border=NA)
  polygon(c(BEG.p[2,],rev(BEG.p[2,])),c(ymin,ymax),col=tcol(4,80),border=NA)
}) 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  4.0 Target Recruitment  based Escapement Goal
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
#---- UI Output----------------------------------------------------------------------
output$minRec = renderUI({
  mr <-median(sr.data()$R)
  D <- floor(log10(mr))
  # This makes largest numbers into integer (e.g. 100000)
  imr <- 0.8*ceiling(mr/(10^D))*10^D  
  numericInput("r1", "Min Mean Recruit", value=imr,min=0, step=10^(D-1))
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
#  Calculate probability that intercects profile 
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
  x <- sr.data()
  xp <- x/u
  SRp <- SRp()/u
  replayPlot(base.r())
  rg <- input$r1/u
  BEG.p <- Rec_gl()/u 
  abline(h=rg,lwd=2,col=2)
  ymin <-c(0,0)
  ymax <-c(1.1*max(xp$R),1.1*max(xp$R))
  polygon(c(BEG.p[1,],rev(BEG.p[1,])),c(ymin,ymax),col=tcol(3,80),border=NA)
  polygon(c(BEG.p[2,],rev(BEG.p[2,])),c(ymin,ymax),col=tcol(4,80),border=NA)
}) 


#  Minimum Recruit Proflie Plot 
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
  abline(h = rpg,lwd=2,col=2)
#-------------------------------------------------------------------------------  
# Add Parametric model probile 
#-------------------------------------------------------------------------------  
  s <- SR.prof.m()$S/u
  Rci <- SR.prof.m()$prof.Rci
  Rpi <- SR.prof.m()$prof.Rpi
  lines(s,Rci,col='grey')
  lines(s,Rpi,col='grey',lty=2) 
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
#  User defined escapement goal analyses 
#===============================================================================
CG_sim <- eventReactive(input$Run,{
  #-----------------------------------------------------------------------  
  progress <- Progress$new(session, min=1, max=15)
  on.exit(progress$close())
  progress$set(message = 'Simulation in progress',
               detail = 'This may take a while...')
  for (i in 1:15) {progress$set(value = i)}
  #-----------------------------------------------------------------------   
  #  Import user defined lower and upper goal 
  lg <- input$lg
  ug <- input$ug
  # create goal range 
  S <- seq(lg,ug,length.out=201)   
  D <- floor(mean(log10(sr.data()$S)))
  srmodel <- SR.CR   # Ricker SR model 
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
    # mc.R.p adds obervation error (sigma)  
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

#-------------------------------------------------------------------------------
# SRp.G: Parametric PI and CI
#-------------------------------------------------------------------------------
SRp.G <- eventReactive(input$Run,{
  #-----------------------------------------------------------------------------  
  progress <- Progress$new(session, min=1, max=15)
  on.exit(progress$close())
  progress$set(message = 'Parametric profile  in progress',
               detail = 'This may take a while...')
  for (i in 1:15) {progress$set(value = i)}
  #-----------------------------------------------------------------------------   
  # Import Lower and Upper goal range
  lg <- input$lg
  ug <- input$ug
  # create S sequencce 
  S <- seq(lg,ug,length.out=201)
  # Prediction model s range  
  D <- floor(mean(log10(sr.data()$S)))
  s <- S/(10^D)
  # Run SR model to estimate predicted range 
  pred <- predictSE(SR(), newdata=data.frame(s=s), se.fit = TRUE)
  # Predicted ln(R/S)
  lRS <- pred$fit
  # Model SE
  se <- pred$se.fit
  # Model st.Residual
  res <- sd(as.vector(SR()$residuals))
  # Calculate degree of freedum dft
  foo <-as.data.frame(SR()$dims)
  dft <- foo$N-foo$p
  # Preidction interval is Var + var (residuals)
  pse <- sqrt(se^2+res^2)
  # Calculate tdist
  # tf <- qt((1-mp)/2,dft,lower.tail=FALSE)
  # Calculate CI-PI
  # Generate 1000 t-distribution   
  boot.t <- matrix(0,nrow=201,ncol=1000)
  for (i in 1:201){
    boot.t[i,] <- rt(1000,dft)
  }
  # CI is expected + tdist*se, PI is expected + tdist*pse  
  bRci <- as.vector(exp(boot.t*se+lRS)*S)
  bYci <- as.vector(exp(boot.t*se+lRS)*S - S)
  bRpi <- as.vector(exp(boot.t*pse+lRS)*S)
  bYpi <- as.vector(exp(boot.t*pse+lRS)*S - S)
  out <- data.frame(cbind(bRci,bYci,bRpi,bYpi))
  names(out) <- c('bRci','bYci','bRpi','bYpi')
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
  # Boootstrap estimate  
  d1 <- density(Y.p/u)
  d2 <- density(CG_sim()$Y/u)
  plot(d2,xlim =c(min(d1$x),max(d1$x)),axes = FALSE,main='',xlab='',ylab='')
  lines(density(SRp.G()$bYci/u), col = 'grey')
  par(new = TRUE)  
  plot(d1, main='Expected Mean and Annual Yields',lty=2,xlab=paste("Yield",mult),ylab='')
  lines(density(SRp.G()$bYpi/u), col = 'grey')  
  abline(v=yg,col=2)
  legend('topright',legend=c('Mean','Annual'),lty=c(1,2), bty ='n')
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
  # Parametric estimate
  lines(density(SRp.G()$bRci/u), col = 'grey')
  par(new = TRUE)
    # Mean estimate   
  # Bootstrap estimate  
  plot(d1, main='Expected Mean and Annual Recruit',xlab=paste("Recruit",mult),
       lty=2,ylab='')
  # Parametric estimate   
  lines(density(SRp.G()$bRpi/u), col = 'grey')  
  abline(v=rg,col=2)
  legend('topright',legend=c('Mean','Annual'),lty=c(1,2), bty ='n')
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
  prg <- sum(ifelse(CG_sim()$R>rg,1,0))/length(CG_sim()$R)
  prga <- sum(ifelse(CG_sim()$R.p>rg,1,0))/length(CG_sim()$R.p)
# Parametric method 
  prgp <- sum(ifelse(SRp.G()$bRci>rg,1,0))/length(SRp.G()$bRci)
  prgap <- sum(ifelse(SRp.G()$bRpi>rg,1,0))/length(SRp.G()$bRpi)
  t.prg <- paste('Meeting Mean Recruit Target:',round(100*prg,0),'%')
  t.prga <- paste('Meeting Annual Recruit Target:',round(100*prga,0),'%')
  t.prgp <- paste('Meeting Target Mean(p) :',round(100*prgp,0),'%')
  t.prgap <- paste('Meeting Target Annual(p) :',round(100*prgap,0),'%')
  paste(t.prg,t.prgp,t.prga,t.prgap,sep='\n')
#  paste(t.prg,t.pyg,sep='\n')
})


# Calculate Probability meeting target  
output$Txt_Yield_pb_cg <- renderText({
  yg <- input$yg
  pyg <- sum(ifelse(CG_sim()$Y>yg,1,0))/length(CG_sim()$Y)
  pyga <- sum(ifelse(CG_sim()$Y.p>yg,1,0))/length(CG_sim()$Y.p)
  t.pyg <- paste('Meeting Mean Yield Target:',round(100*pyg,0),'%')
  t.pyga <- paste('Meeting Annual Yiled Target:',round(100*pyga,0),'%')
  pygp <- sum(ifelse(SRp.G()$bYci>yg,1,0))/length(SRp.G()$bYci)
  pygap <- sum(ifelse(SRp.G()$bYpi>yg,1,0))/length(SRp.G()$YRpi)
  t.pygp <- paste('Meeting Target Mean(p) :',round(100*pygp,0),'%')
  t.pygap <- paste('Meeting Target Annual(p) :',round(100*pygap,0),'%')
  paste(t.pyg,t.pygp,t.pyga,t.pygap,sep='\n')  
#  paste(t.prg,t.pyg,sep='\n')
})


# Recruit Plot 
output$Plt_rec.cg <- renderPlot({
  u <- as.numeric(input$ui)
  x <- sr.data()
  xp <- x/u
  SRp <- SRp()/u
  replayPlot(base.r())
  BEG.p <- c(input$lg,input$ug)/u 
  ymin <-c(0,0)
  ymax <-c(1.1*max(xp$R),1.1*max(xp$R))
  polygon(c(BEG.p,rev(BEG.p)),c(ymin,ymax),col=tcol(3,80),border=NA)
  rg <- input$rg/u
  abline(h=rg,col=2)
}) 

# Yield Plot 
output$Plt_yield.cg <- renderPlot({
  u <- as.numeric(input$ui)
  x <- sr.data()
  xp <- x/u
  SRp <- SRp()/u
  replayPlot(base.y())
  BEG.p <- c(input$lg,input$ug)/u 
  ymin <-c(min(SRp$Rl-SRp$S),min(SRp$Rl-SRp$S))
  ymax <-c(1.1*max(xp$R-xp$S),1.1*max(xp$R-xp$S))
  polygon(c(BEG.p,rev(BEG.p)),c(ymin,ymax),col=tcol(3,80),border=NA)
  yg <- input$yg/u
  abline(h=yg,col=2)
}) 






#=======================================================================
#  Panel 4: Management Strategy Evaluation    
#=======================================================================
#-----------------------------------------------------------------------
#  Initialize 
#-----------------------------------------------------------------------
MSE.int <- eventReactive(input$InitRun,{
#-----------------------------------------------------------------------  
# import brood table
  x <- brood.out()$brood  
# Just retrive brood recuit by age 
  x <- x[complete.cases(x),-c(1,2,dim(x)[2])]
# Calculate brood recurit age prop  
  p.x <- x/rowSums(x)
# Calculate mean age recruit
  p.i <- colMeans(p.x)
  p.ip <- p.i*(1-p.i)
  p.i.v <- apply(p.x,2,var)
  nn <- p.ip/p.i.v
# Set Drishelet   
  D <- round(median(nn),0)
  phi <- coef(SR()$modelStruct$corStruct,unconstrained=FALSE)
  # If model is standard Ricker, phi is 0     
  phi <- ifelse(is.null(phi),0,phi)
# first age
  fage <- input$fage
# number of age groups  
  nages <- dim(x)[2]
# last age 
  lage <- fage + nages-1
  years <- input$simy
  burnin <-input$burnin
  train <- input$train
# Total Simulation Years 
  nyrs <- burnin+train+years
# AR1 Process: random walk phi=1
ar1 <- function(n,sd,phi){
    ar1 <- numeric(n)
    ar1[1] <- 0
    for(i in 2:n){
      ar1[i] <- phi*ar1[i-1]+rnorm(1,0,sd)
    }
    ar1
} 
  e.Rec <- ar1(nyrs,SR.out()$sigma,phi)
  e.p <- rdirichlet(nyrs,alpha=p.i*D)
  # output data  
  e.pred <- exp(rnorm(nyrs,0,input$spred/100))
  e.obsH <- exp(rnorm(nyrs,0,input$sobsH/100))
  e.obsS <- exp(rnorm(nyrs,0,input$sobsE/100))
  e.imp <- exp(rnorm(nyrs,0,input$simpH/100))
  out <- list(nages=nages,e.pred = e.pred, e.obsH = e.obsH, e.obsS = e.obsS,e.imp = e.imp, e.Rec = e.Rec, e.p = e.p)
  return(out) 
   })

#=================================================================================
#  MSE Simulation Rutine 
#=================================================================================
sim <- eventReactive(input$SimRun,{
#-----------------------------------------------------------------------
#  Import Error Data 
#-----------------------------------------------------------------------  
  Init <- MSE.int()
  nages <- MSE.int()$nages
  e.pred <- as.vector(MSE.int()$e.pred)
  e.obsH <- as.vector(MSE.int()$e.obsH)
  e.obsS <- as.vector(MSE.int()$e.obsS)
  e.imp <- as.vector(MSE.int()$e.imp)
  e.Rec <- as.vector(MSE.int()$e.Rec)
  e.p <- as.matrix(MSE.int()$e.p)
# Initial Run size   
  R0 <- median(sr.data()$R)
# first age
  fage <- input$fage
# last age 
  lage <- fage + nages-1
  years <- input$simy
  burnin <- input$burnin
  train <- input$train
# Total Simulation Years 
  nyrs <- burnin+train+years
#-----------------------------------------------------------------------
#  Import SR and management parameters    
#-----------------------------------------------------------------------  
  EGl <- input$EGlu[1]
  EGu <- input$EGlu[2]
  alpha <- SR.out()$alpha
  beta <- SR.out()$beta
  Umsy <- SR.out()$Umsy
#-----------------------------------------------------------------------
#  Create Empty vector  
#-----------------------------------------------------------------------  
# Recruit
  R <- numeric(nyrs)
  R.obs <- numeric(nyrs)
# Annual Run
  N <- numeric(nyrs)
# Annual Escapement
  S <- numeric(nyrs)
  S.obs <- numeric(nyrs)
# Annual Harvest 
  H <- numeric(nyrs)
  H.obs <- numeric(nyrs)
# Annuual Run by age 
  N.ta <- matrix(0,ncol=nages, nrow=nyrs+lage+2)
  N.ta.obs <- matrix(0,ncol=nages, nrow=nyrs+lage+2)
# Annual Escapement goals  
  Egoals <- matrix(0,ncol=2, nrow = nyrs+1)
# Annual SR parameters   
  SR.sim <- matrix(0,ncol=4, nrow = nyrs)
#---------------------------------------------------------------------------
#   Start simulation 
#---------------------------------------------------------------------------
for (y in 1:nyrs){
# First generaion is constant   
 if(y<=lage) {
   N.ta[y,] <- R0*exp(e.Rec[y])*e.p[y,]
  }  
# Anunual Run is sum of all ages
N[y] <- sum(N.ta[y,])
# Predicted Run
N.pred <- N[y]*e.pred[y]
# Determine target harvest criteria	
EG.l <- ifelse(input$cmode =='Middle',mean(Egoals[y,]),ifelse(input$cmode =='Upper',Egoals[y,2],Egoals[y,1]))
if(y<=(burnin+train)){
# Before management: Harvest is at Umsy 
    H.target <- N.pred*Umsy
  } else {
# Management based on Escapement goal 
    H.target <- ifelse(N.pred < EG.l,0,min(input$maxH,(N.pred-EG.l)*input$maxHr))
  }
# Actual Harvest
  H[y] <- min(H.target*e.imp[y],0.99*N[y])
# Actual Escapement 
  S[y] <- N[y] - H[y]
# Calculate Future Recruits based on SR 
  
  R[y] <- alpha*S[y]*exp(-beta*S[y]+e.Rec[y])
# Fill Future Return by age
  for (a in 1:nages){ N.ta[y+fage+a-1,a] <- R[y]*e.p[y,a] }
# Observed Escapement 
  S.obs[y] <- S[y]*e.obsS[y]
#Observed Harvest
  H.obs[y] <- H[y]*e.obsH[y]
#Age comp
  p.age <- N.ta[y,]/N[y]
#Observed age comp 
  p.age.ob <-rmultinom(1,input$Nobage,p.age)/input$Nobage
#Observed Run by age (Assume age comp est is accurate)
  N.ta.obs[y,] <- sum(S.obs[y],H.obs[y])*p.age.ob
# Create Recruitment data based on observed harvest and escapement
if(y>lage) {R.obs[y-lage] <- sum(diag(N.ta.obs[(y-nages):y,]))}
  
#-------------------------------------------------------------------------------
#   Active harvest management:  Set Escapment Goal 
#-------------------------------------------------------------------------------
  if(y>=(burnin+train)) {
# Start esimating SR model parameters
# Assume data have been collected since train    
    R.est <- R.obs[(burnin+1):(y-lage)]
    S.est <- S.obs[(burnin+1):(y-lage)]	
# Calcultate SR parameters   
    lnRPS <- log(R.est/S.est)
    srfit <- lm(lnRPS~S.est)
    lnalpha.est <- coef(srfit)[1]
    beta.est <- -coef(srfit)[2]
    Smsy.est <- lnalpha.est*(0.5-0.07*lnalpha.est)/beta.est
    Smax.est <- 1/beta.est	
    SR.sim[y,] <- c(lnalpha.est,beta.est,Smsy.est,Smax.est)
    EG.m <- ifelse(input$EGm =='Smsy',Smsy.est,Smax.est)
    EG.l <- round(EGl*EG.m,-floor(log10(EGl*EG.m))+1)
    EG.u <- round(EGu*EG.m,-floor(log10(EGu*EG.m))+1)
# Board of fish: change escapement goal every bord cycle
    if((y-burnin-train)%%input$EGY==0){Egoals[y+1,] <- c(EG.l,EG.u)}
    else {Egoals[y+1,] <- Egoals[y,]}	
    }# End of EG management 
 } # End simulation 
#-------------------------------------------------------------------------------	 
# Data Output
#-------------------------------------------------------------------------------	 
# Put NA on years Escaoement goals were not calculated  
Egoals[1:(burnin+train),] <- NA  
SR.sim[1:(burnin+train-1),] <- NA  
# data output
out <- list(N=N,S=S,H = H,R=R, R.obs=R.obs, SR.sim=SR.sim,Egoals=Egoals)
return(out)  
})
#=================================================================================

#--------------------------------------------------------------------------------
#  Simulation Annual Change 
#--------------------------------------------------------------------------------
output$runsim <- renderPlot({
  par(cex=1.3)
  years <- input$simy
  burnin <- input$burnin
  train <- input$train
  # Total Simulation Years 
  nyrs <- seq(1,burnin+train+years)
  N <- sim()$N
  S <- sim()$S
  H <- sim()$H
  Egoals <- sim()$Egoals
  SR <- sim()$SR.sim
  u <- as.numeric(input$ui)
  mult <- mult(u)
  par(xaxs='i',yaxs='i',bty='l')
  plot(nyrs,N/u,type='l',ylim=c(0,max(N))/u,col=1,
#       main=input$caption,
       xlab='Year',
       ylab=paste('Run / Escapement',mult))
  lines(nyrs,S/u,lty=2,col=2)
  lines(nyrs,H/u,lty=3,col=3)
  lines(nyrs,Egoals[nyrs,1]/u,col=4)
  lines(nyrs,Egoals[nyrs,2]/u,col=4)
  col <- ifelse(input$EGm =='Smsy',3,4)
  tn <- ifelse(input$EGm =='Smsy','Smsy','Smax')
  preS <- ifelse(input$EGm =='Smsy',SR.out()$Smsy,SR.out()$Smax)
  lines(nyrs,SR[nyrs,col]/u,col=6)
  abline(h=preS/u,col=6,lty=2)
  legend('topleft',c('Run','Escapement','Harvest',tn),lty=c(1,2,3,1),col=c(1,2,3,6),bty='n')  
})

#--------------------------------------------------------------------------------
#  Simulation Summary
#--------------------------------------------------------------------------------
output$simsum <- renderPrint({
#  MSE.int()
  options(scipen=999)
  years <- input$simy
  burnin <- input$burnin
  train <- input$train
  x <- data.frame(N = sim()$N, S = sim()$S, H = sim()$H)
  xs <- x[(burnin+train+1):(burnin+train+years),]
  print(summary(xs,digits=c(0,0,0)))
})

#--------------------------------------------------------------------------------
#  Simulation Histogram
#--------------------------------------------------------------------------------
output$simhist <- renderPlot({
  par(mfrow=c(1,3),mar = c(2,2,2,2), cex=1.2)
  options(scipen=999)
  years <- input$simy
  burnin <- input$burnin
  train <- input$train
  x <- data.frame(N = sim()$N, S = sim()$S, H = sim()$H)
  xs <- x[(burnin+train+1):(burnin+train+years),]
  hist(xs$N, main='Run size',xlab='',ylab='')
  hist(xs$S, main='Escapement',xlab='',ylab='')
  hist(xs$H, main='Harvest',xlab='',ylab='')
 })

#===============================================================================
#  Save multiple Simulation Results
#===============================================================================
# Set temporary memory: M
memory <- reactiveValues(dat = NULL)
# Retrieve simulaiton retuslts data 
yvals <- reactive({ 
    x <- data.frame(N = sim()$N, S = sim()$S, H = sim()$H)
    return(x)
  })
# Save to data.frame 
xvals <- reactive({
# Keep previous data   
  isolate(dat <- memory$dat)
  if (is.null(dat)) {
    memory$dat <- data.frame(yvals())
  } else {
    alt <- data.frame(yvals())
    memory$dat <- data.frame(dat,alt)
   }
  return(memory$dat)
})

observe({ if (input$SimClear == 0) 
  return()
  memory$dat <- NULL
})

# Download data  ----
output$simdownload <- downloadHandler(
  filename = function() {"Simdata.csv"},
  content = function(file) {
    write.csv(xvals(), file, row.names = FALSE)
  })

#===============================================================================

#-------------------------------------------------------------------------------
#  Simulation Table Output
#-------------------------------------------------------------------------------

output$altsim.sum <- renderPrint({
  options(scipen=999)  
  years <- input$simy
  burnin <- input$burnin
  train <- input$train 
# Total Simulation Years 
  nyrs <- burnin+train+years
  x <- xvals()[(burnin+train+1):nyrs,]
  alts <- dim(x)[2]
  N.alt <- data.frame(x[,seq(1,alts,3)])
  S.alt <- data.frame(x[,seq(2,alts,3)])
  H.alt <- data.frame(x[,seq(3,alts,3)])
  out <- list(N=summary(N.alt),S=summary(S.alt),H=summary(H.alt))
  return(out)
   })

#--------------------------------------------------------------------------------
#  Simulation Comparative figures
#--------------------------------------------------------------------------------
output$altsim.N <- renderPlot({
  par(mfrow=c(3,1),mar = c(2,2,2,2),cex=1.1)
  u <- as.numeric(input$ui)
  mult <- mult(u) 
  years <- input$simy
  burnin <- input$burnin
  train <- input$train 
# Total Simulation Years 
  nyrs <- seq(1,burnin+train+years)
  x <- xvals()
  alts <- dim(x)[2]
  N.alt <- data.frame(x[,seq(1,alts,3)])
  S.alt <- data.frame(x[,seq(2,alts,3)])
  H.alt <- data.frame(x[,seq(3,alts,3)])
  n.rep <- dim(N.alt)[2]
  par(xaxs='i',yaxs='i',bty='l')
  plot(nyrs,N.alt[,1]/u,type='l',ylim=c(0,max(N.alt))/u,col=1,
       xlab='Year', main=paste('Run',mult))
  for(i in 1:n.rep){
  lines(nyrs,N.alt[,i]/u, col = i)
  }
  legend('topleft',paste('Alt',seq(1,n.rep)),lty=1,col=seq(1,n.rep),bty='n',cex=0.8) 

  plot(nyrs,S.alt[,1]/u,type='l',ylim=c(0,max(S.alt))/u,col=1,
       xlab='Year', main=paste('Escapement',mult))
  for(i in 1:n.rep){
    lines(nyrs,S.alt[,i]/u, col = i)
  }
  legend('topleft',paste('Alt',seq(1,n.rep)),lty=1,col=seq(1,n.rep),bty='n',cex=0.8) 

  plot(nyrs,H.alt[,1]/u,type='l',ylim=c(0,max(H.alt))/u,col=1,
       main=paste('Harvest',mult), xlab='Year')
  for(i in 1:n.rep){
    lines(nyrs,H.alt[,i]/u, col = i)
  }
  legend('topleft',paste('Alt',seq(1,n.rep)),lty=1,col=seq(1,n.rep),bty='n',cex=0.8) 
  })

output$altsim.H <- renderPlot({
  par(mfrow=c(1,3),mar = c(2,2,2,2),cex=1.1)
  years <- input$simy
  burnin <- input$burnin
  train <- input$train 
  # Total Simulation Years 
  nyrs <- burnin+train+years
  x <- xvals()[(burnin+train+1):nyrs,]
  alts <- dim(x)[2]
  N.alt <- data.frame(x[,seq(1,alts,3)])
  S.alt <- data.frame(x[,seq(2,alts,3)])
  H.alt <- data.frame(x[,seq(3,alts,3)])
  tN.alt <- melt(N.alt)
  tS.alt <- melt(S.alt)
  tH.alt <- melt(H.alt)
  plot(value~variable,data=tN.alt, main='Run', xlab = 'Models')
  plot(value~variable,data=tS.alt, main='Escapement', xlab = 'Models')
  plot(value~variable,data=tH.alt, main='Harvest', xlab = 'Models')
})

})  #End Server 
# Create Shiny app ----
shinyApp(ui, server)
                               