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
source("Shiny_modules.R")
source("Shiny_SR_functions.R")
source("Shiny_Boot_modules.R")

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
#-------------------------------------------------------------------------------
    numericInput("bn", "Number bootstrap replicates", value=10000,min=1000,step=1000)
  ), # End of Sidebar  Panel
# output
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

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 2  SR Analyses 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
#      textInput(inputId='caption',label='Figue Caption Title',value=''),           
      selectInput(inputId="ui","Axis Dislpay Unit", choices = c(1,1000,1000000)),  
        p("Escapement Goal Range"),
        numericInput(inputId='egl','Lower Goal',value=0,min=0), 
        numericInput(inputId='egu','Upper Goal',value=0,min=0),    
        checkboxInput(inputId="show.eg", "Show Escapement Goal", FALSE),
        checkboxInput(inputId="show.points", "show Years", TRUE), 
        checkboxInput(inputId="show.smsy", "show Smsy", TRUE),
        checkboxInput(inputId="show.smax", "show Smax", TRUE),
        checkboxInput(inputId="show.int", "show Interval", TRUE),
        numericInput(inputId="p.i", "% Interval", value=90,min=0,max=100,step=5),
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
        p(strong("Anova Table")), 
        verbatimTextOutput('Txt_ANOA'),
        p(strong("SR Parameters")), 
        verbatimTextOutput('Txt_sr.pars')                      
            ),#End tabPanel

#------------------ Yield Plot-------------------------------------------        
      tabPanel("Yield Plot",
        plotOutput(height='500px','Plt_yield')
            ),#End tabPanel

#------------------ Residuals  --------------------------------------------- 
      tabPanel("Residuals", 
        plotOutput("Plt_residual"),
        p(strong("Durbin-Watson Serial Correlation Analyses")), 
        verbatimTextOutput('Txt_dwtest')
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
    tabPanel("Smsy Goal Analyses",      
      sidebarPanel(width = 3,
        p(strong("Smsy Analyses")),  
        sliderInput("p.msy", "Min % of MSY", value=90,min=0,max=100,step=5),
        sliderInput("p.msy.t", "% Meeting MSY Target", value=90,min=0,max=100,step=5)
            ),  #End sidebarPanel
#-------  mainPanel ------------------------------------------------------------      
      mainPanel(
        tabsetPanel(
          
#------------------ Smsy Profile ----------------------------------------------- 
          tabPanel("Smsy Profile",
            plotOutput(height='500px','Plt_Smsy.prof'),
            verbatimTextOutput("Txt_Srange.smsy")
                    ), #End tabPanel

#------------------ Smsy Yield Profile -----------------------------------------
          tabPanel("Yield  & Recruit Profile",
#           splitLayout(cellWidths = c("50%", "50%"),
            plotOutput(height='300px','Plt_yield.smsy'),
            plotOutput(height='300px','Plt_rec.smsy')
                    ), #End tabPanel

#------------------ Smsy Recruit Yield Profile -----------------------------------------
          tabPanel("Recruit & Yiled distribution",
#               splitLayout(cellWidths = c("50%", "50%"),
            plotOutput(height='600px','Plt_Smsy.dist'),
            verbatimTextOutput('Txt_sum.smsy')
#                plotOutput(height='300px','')
#              )
                    )#End tabPanel
                  )#End tabsetPanel
                )#End mainPanel
              ),#End tabPanel Smsy Goal Analyses
#------------------------------------------------------------------------    
#  Smax Goal Analyses 
#------------------------------------------------------------------------    
    tabPanel("Smax Goal Analyses",
      sidebarPanel(width = 3,
        p(strong("Smax Analyses")),            
        sliderInput("p.rmx", "Min % of Rmax", value=90,min=0,max=100,step=5),
        sliderInput("p.rmx.t", "% Meeting Rmax Target", value=90,min=0,max=100,step=5)
          ), # End sidebarPanel 

#-------------  mainPanel ------------------------------------------------------
      mainPanel(
        tabsetPanel(
#------------------ Smax Profile -----------------------------------------------                     
          tabPanel("Smax Profile",
            plotOutput(height='500px','Plt_Smax.prof'),
            verbatimTextOutput('Txt_Srange.smax')
                      ), # End tabrPanel 
                  
#------------------ Run Profile -----------------------------------------------                  
          tabPanel("Run Profile",
#                    splitLayout(cellWidths = c("50%", "50%"),                 
            plotOutput(height='300px',"Plt_rec.smax"),
            plotOutput(height='300px',"Plt_yield.smax")
                      ),  # End tabPanel

#------------------ Smax Recruit Yield Profile -----------------------------------------
          tabPanel("Recruit & Yiled distribution",
#               splitLayout(cellWidths = c("50%", "50%"),
            plotOutput(height='600px','Plt_Smax.dist'),
            verbatimTextOutput("Txt_sum.smax")
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
            textOutput("Txt_Srange_yg"))
                       ), #End tabPanel

#------------------ Recruit Goal Profile ---------------------------------------   
          tabPanel("Recruit Goal Analyses",
#                      splitLayout(cellWidths = c("50%", "50%"),
            plotOutput(height='300px','Plt_rec.gl'),
            plotOutput(height='300px','Plt_rec.prof'),
#                      )
            splitLayout(cellWidths = c("50%", "50%"),
#                      verbatimTextOutput("brt"),
            textOutput("Txt_Srange_rg"))
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
            plotOutput(height='600px',"Plt_yield.cs"),
            splitLayout(cellWidths = c("50%", "50%"),
              p(strong("Recruit and Yields Summary")),
              p(strong("Probability of Meeting Target"))
                    ),
            splitLayout(cellWidths = c("50%", "50%"),
#              verbatimTextOutput("bGAs"),
              verbatimTextOutput("Txt_prob.tgt.yld")
                    )
                  ), #End tab Panel

#------------------ Recruit Annual Recruit and Yields ---------------------------------   
          tabPanel("Expected Recruit Mean & Annual",
            plotOutput(height='600px',"EGR.Rec"),
              splitLayout(cellWidths = c("50%", "50%"),
                p(strong("Recruit and Yields Summary")),
                p(strong("Probability of Meeting Target"))
                    ),
              splitLayout(cellWidths = c("50%", "50%"),
#                verbatimTextOutput("bGASRs"),
                verbatimTextOutput("Txt_prob.tgt.rec")
                    )
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
              sliderInput(inputId="phi", "AR1 correlation", value=0.6,min=0,max=1,step=.1),
              sliderInput(inputId="D", "Drishelet D", value=50,min=0,max=200,step=10)
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
  print(strong(paste("Hamazaki, T.",format(Sys.Date(), "%Y"),". Missing Passage Estimation Analyses(source: https://shiny.rstudio.com/). Available from https://hamachan.shinyapps.io/Missed_Run/")))
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
  data <- dataInputServer("datain")
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
    # Add Escapement Goal range  
    if(input$show.eg==TRUE) {
      with(x,polygon(c(Yr,rev(Yr)),c(rep(input$egl/u,length(Yr)),rev(rep(input$egu/u,length(Yr)))),col=tcol('grey',50),border=NA))
    }
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
    # Add Escapement Goal range  
    if(input$show.eg==TRUE) {
      with(x,polygon(c(Yr,rev(Yr)),c(rep(input$egl/u,length(Yr)),rev(rep(input$egu/u,length(Yr)))),col=tcol('grey',50),border=NA))
    }
    lines(S/u~Yr,data=x,lty=2)
    legend('topright',c('Run','Escapement'),lty=c(1,2),box.lty=0)  
  })
  
  
#------------------ Data Download   ---------------------------------  
output$downloadData <- downloadHandler(
    filename = function() {"broodtable.csv"},
    content = function(file) {
      write.csv(brood.table(), file, row.names = FALSE)
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
  names(out) <- c('ln.alpha','alpha', 'beta', 'sigma','Seq','Smsy','Umsy','Rmsy','MSY','Smax','Rmax')
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
  mp <- input$p.i/100
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
  names(out) <- c('s','fit','lwr','upr')
  return(out)
  })  

  
#-----------------------------------------------------------------------
#  6.0 Base SR plot 
#-----------------------------------------------------------------------
base.plot.SR <- reactive({
  u <- as.numeric(input$ui)
  mult <- mult(u)
  x <- sr.data()
  x0 <- sr.data.0()    
  par <-SR.out()
  xp <- x/u
  xp0 <- x0/u    
  SRp <- SR.pred.m()[,1:4]/u
  par(xaxs='i',yaxs='i',bty='l')
  plot(R~S,data=xp,pch=19,col=1, 
#         main= input$caption,
      xlab=paste("Escapement",mult),ylab=paste('Recruit',mult),
      xlim=c(0,max(SRp$s)),ylim=c(0,1.1*max(xp0$R)))
    abline(0,1,col=2)
    # Add Predicted   
    lines(fit~s,data=SRp,col=1,lw=2)
    out <-recordPlot()
    return(out)
  })
  
#-----------------------------------------------------------------------
#  7.0 Base Yield plot 
#-----------------------------------------------------------------------
base.plot.Yield <- reactive({
    #  dev.control("enable")
  u <- as.numeric(input$ui)
  mult <- mult(u)
  x <- sr.data()
  x0 <- sr.data.0()    
  xp <- x/u
  xp0 <- x0/u     
  par <-SR.out()
  SRp <- SR.pred.m()[,1:4]/u
    # Plot Basic Yield plot  
  par(xaxs='i',yaxs='i',bty='l')
  plot((R-S)~S,data=xp,pch=19,col=1, 
#         main= input$caption,
         xlab=paste("Escapement",mult),ylab=paste('Yield',mult),
         xlim=c(0,max(SRp$s)),ylim=c(min(SRp$lwr-SRp$s),1.1*max(xp0$R-xp0$S)))
  lines((fit-s)~s,data=SRp,col=1,lw=2)
  abline(h=0,col=2)
  out <-recordPlot()
    #  dev.off()
  return(out)
  })


#===============================================================================
# Panel 1: SR Analyses SEction 
#===============================================================================
#-------------------------------------------------------------------------------
#  SR plot 
#-------------------------------------------------------------------------------
srplot <- function(){
  u <- as.numeric(input$ui)
  x <- sr.data()
  par <-SR.out()
  xp <- x/u
  SRp <- SR.pred.m()[,1:4]/u
  # Draw Base SR Plot
  replayPlot(base.plot.SR())
  # Add CI    
  if(input$show.int==TRUE){
    with(SRp,polygon(c(s,rev(s)),c(upr,rev(lwr)),col=tcol('grey',50),border=NA))
  }
  # Add Years
  if(input$show.points==TRUE) {
    pointLabel(xp$S,xp$R, labels=as.character(x$Yr), cex= 1,col=4)}
  # Add Escapment Goal Range
  if(input$show.eg==TRUE) {
    abline(v=input$egl/u,col=2,lty=1,lwd=2)
    abline(v=input$egu/u,col=2,lty=1,lwd=2)  
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
  replayPlot(base.plot.Yield())
  # Add CI    
  if(input$show.int==TRUE){
    with(SRp,polygon(c(s,rev(s)),c(upr-s,rev(lwr-s)),col=tcol('grey',50),border=NA))
  }
  # Add Years
  if(input$show.points==TRUE) {
    pointLabel(xp$S,(xp$R-xp$S), labels=as.character(x$Yr), cex= 1,col=4)
    #    text((R-S)~S,data=xp, labels=x$Yr, cex= 1, pos=3,col=4)
  }
  # Add Escapement Goadl Range  
  if(input$show.eg==TRUE) {
    abline(v=input$egl/u,col=2,lty=1,lwd=2)
    abline(v=input$egu/u,col=2,lty=1,lwd=2)  
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
  par(bty='l')
  plot(resid~year,xlab='Year',ylab='Residuals'
  )
  abline(h=0)
  model <- gam(resid~s(year),family=gaussian, fit =TRUE)
  pred.year <- data.frame(year, predict.gam(model,se = TRUE))    
  lines(pred.year$year, pred.year$fit,lwd=2,col=4)
  pred.year$ciu <- pred.year$fit + 2*pred.year$se.fit
  pred.year$cil <- pred.year$fit - 2*pred.year$se.fit
  with(pred.year,polygon(c(year,rev(year)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA))
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
#-------------------------------------------------------------------------------
#  1.0: Create Bootstrap Data and output boot SR parameters
#-------------------------------------------------------------------------------
boot.SR <- reactive({
# The number of bootstrap replicates
  boot.n <- isolate(input$bn)
  # Calculate presdicted ln(R/S)    
  SRP <-as.vector(predict(SR()))
  # Calculate resicuals     
  SRR <-as.vector(residuals(SR()))
  # Extract AR1 phi
  phi <- coef(SR()$modelStruct$corStruct,unconstrained=FALSE)
  # If model is standard Ricker, phi is 0     
  phi <- ifelse(is.null(phi),0,phi)
  # The number of sample size     
#-----------------------------------------------------------------------  
  progress <- Progress$new()
  on.exit(progress$close())
  progress$set(message = paste(boot.n,'Bootstrap Calculation in progress'),
                detail = 'This will take a while. Be patient please....')
  for (i in 1:15) {progress$set(value = i)}
#-----------------------------------------------------------------------   
  D <- floor(mean(log10(sr.data()$S))) 
  s <- as.vector(sr.data()$S/(10^D))
  boot <- bootSR(boot.n,s,phi,SRP,SRR)
 return(boot)
  })

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
SR.pred.b <- reactive({
#-----------------------------------------------------------------------  
  progress <- Progress$new(session, min=1, max=15)
  on.exit(progress$close())
  progress$set(message = 'Profile Calculation in progress',
               detail = 'This may take a while...')
  for (i in 1:15) { progress$set(value = i) }
#-----------------------------------------------------------------------  
#---------- Model Estmated Spawner Range is either 90 percentile Seq 
#---------- or max observed spawner size -------------------------------   
  Seq <- quantile(boot.SR()$Seq,0.9)
  max.s <- max(Seq,max(sr.data()$S))
  D <- floor(log10(max.s))
# This makes largest numbers into integer (e.g. 100000)
  maxb <- ceiling(max.s/(10^D))*10^D
# Set 200 segments of s  
  S <- seq(0,maxb, length.out=201)
# Import bootstrap SR parameters  
  lnalpha <- boot.SR()$ln.alpha
  beta <- boot.SR()$beta
  sigma <- boot.SR()$sigma
  nrows <- length(lnalpha)
# calculate predicted mean recruitment   
  R <- t(S*t(exp(lnalpha-beta%o%S)))
# Calculte predicted annnual recruitment 
   R.p <- matrix(NA,nrow = nrows,ncol=201)
  for(i in 1:nrows){
   R.p[i,] <- exp(rnorm(201,log(R[i,]),sigma[i]))
  }
# calculate predicted Yield     
  Y <- t(t(R)-S)
# Calculate predicted Annual Yield
  Y.p <- t(t(R.p)-S)
# Calculate   
  out <- list(S = S, Y = Y, R = R, Y.p = Y.p, R.p = R.p)
  return(out) 
  })
 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  3.0 Create Bootstrap Recruit & Yields
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
b.YA <- reactive({
# Yield goal
  mp <- (1-input$p.i/100)/2
# Import S 
  S <- SR.pred.b()$S
# Import Yield matrix
  Y <- as.matrix(SR.pred.b()$Y)
  Y.p <- as.matrix(SR.pred.b()$Y.p)
# Calculate mean Yield
  Yme <- colMeans(Y)
  Ymd <- apply(Y,2,median) 
# Calculate Uppe and lower CI bonds
  Yu <- apply(Y,2,function(x) quantile(x,1-mp))
  Yl <- apply(Y,2,function(x) quantile(x,mp))
# Calculate Uppe and lower PI bonds
  Yu.p <- apply(Y.p,2,function(x) quantile(x,1-mp))
  Yl.p <- apply(Y.p,2,function(x) quantile(x,mp))
# Import Recruitment matrix  
  R <- as.matrix(SR.pred.b()$R)
  R.p <- as.matrix(SR.pred.b()$R.p)  
# Calculate mean Recruit  
  Rme <- colMeans(R)
  Rmd <- apply(R,2,median)   
# Calculate Uppe and lower CI bonds  
  Ru <- apply(R,2,function(x) quantile(x,1-mp))
  Rl <- apply(R,2,function(x) quantile(x,mp))
  Ru.p <- apply(R.p,2,function(x) quantile(x,1-mp))
  Rl.p <- apply(R.p,2,function(x) quantile(x,mp))
  out <- data.frame(cbind(S, Yme, Ymd, Yu, Yl, Yu.p, Yl.p, Rme, Rmd,Ru, Rl, Ru.p, Rl.p))
  return(out)
  })

#---------------------------------------------------------------------------
#  Plot Base Predicted Yield 
#---------------------------------------------------------------------------
boot.Yldp <- reactive({
  mp <- input$p.i
  u <- as.numeric(input$ui)
  mult <- mult(u)
# Import S, mean, lower and upper Yields 
  S <- b.YA()$S/u
  Yu <- b.YA()$Yu/u
  Yl <- b.YA()$Yl/u
  Yu.p <- b.YA()$Yu.p/u
  Yl.p <- b.YA()$Yl.p/u  
  Yme <- b.YA()$Yme/u
  Ymd <- b.YA()$Ymd/u
  x0 <- sr.data.0() /u   
  par(xaxs='i',yaxs='i',bty='l')
  plot(S,Yme,type='l',ylim=c(min(x0$R-x0$S),1.1*max(x0$R-x0$S)),
       ylab=paste('Expected Mean Yield',mult),xlab=paste("Escapement",mult)) 
  lines(S,Ymd,lty=2)
  polygon(c(S,rev(S)),c(Yu,rev(Yl)),col=tcol('grey',50),border=NA)
  lines(S,Yu.p,lty=2,col='grey')
  lines(S,Yl.p,lty=2,col='grey')
  points((R-S)~S,data=x0,pch=19)
  abline(h=0,col=4,lwd=2)
  legend('topright',paste(mp,'% CI'),lty=2,box.lty=0)
  out <-recordPlot()
  return(out)
})

#---------------------------------------------------------------------------
#  Plot Base Predicted Run
#---------------------------------------------------------------------------
boot.Recp <- reactive({
  u <- as.numeric(input$ui)
  mp <- input$p.i
  mult <- mult(u)
# Import S, mean, lower and upper Yields 
  S <- b.YA()$S/u
  Ru <- b.YA()$Ru/u
  Rl <- b.YA()$Rl/u
  Ru.p <- b.YA()$Ru.p/u
  Rl.p <- b.YA()$Rl.p/u  
  Rmd <- b.YA()$Rmd/u
  Rme <- b.YA()$Rme/u
  x0 <- sr.data.0() /u   
  par(xaxs='i',yaxs='i',bty='l')
  plot(S,Rme,type='l',ylim=c(0,1.1*max(x0$R)),
       xlab=paste("Escapement",mult),ylab=paste('Expected Mean Recruit',mult)) 
  lines(S,Rmd,lty=2)
  polygon(c(S,rev(S)),c(Ru,rev(Rl)),col=tcol('grey',50),border=NA)
  lines(S,Ru.p,lty=2,col='grey')
  lines(S,Rl.p,lty=2,col='grey')
  legend('topright',paste(mp,'% CI'),lty=2,box.lty=0)   
  abline(0,1,col=1)
  points(R~S,data=x0,pch=19)
  out <-recordPlot()
  return(out)
  })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 3: Smsy based Escapement Goal  
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#-----------------------------------------------------------------------
#  Calculate Smsy Yield Profile     
#-----------------------------------------------------------------------
b.Smsy.prof <- reactive({
# Import user determined minimum MSY Yield %   
  mp <- input$p.msy/100
# Import user determined minimum % achieving target   
  ap <- input$p.msy.t/100
# Import S and predicted mean Yield  
  S <- SR.pred.b()$S 
  Y <- as.matrix(SR.pred.b()$Y)
# Generate profle using profile function 
  Smsy.prof  <- profile(S, Y, mp, ap)
# Rename Output
  names(Smsy.prof) <- c('S','Y.prob','BEG')
  return(Smsy.prof)   
  })

#  Calculate Smsy Optimum profile and escapement goal 
b.Smsy.prof.st <- reactive({
  S <- SR.pred.b()$S 
  Y <- as.matrix(SR.pred.b()$Y)
# Standard Minimum MSY % 
  st <- c(0.9,0.8,0.7)
# Standard target % is 90%
  tp <- 0.9
ncols <- length(S)  
  Y.prob.st <- matrix(0,nrow = 3,ncol=ncols)
  BEG.st <- matrix(0,nrow=3,ncol=2)
  for(i in 1:3){
    St.prof  <- profile(S, Y, st[i], tp)
    Y.prob.st[i,] <- St.prof$M.prof
    BEG.st[i,] <- St.prof$S.range
  }
  # Find Smsy Profile Intersections 
  out <- list(Y.prob.st = Y.prob.st, BEG.st = BEG.st)
  return(out)   
  })

#-----------------------------------------------------------------------
#  Smsy Optimum Profile Plot  
#-----------------------------------------------------------------------
output$Plt_Smsy.prof <- renderPlot({
  mp.1 <- input$p.msy/100
  ap <- input$p.msy.t/100
  u <- as.numeric(input$ui)
  mult <- mult(u)
  S <- b.Smsy.prof()$S/u
  Y.prob.st <- b.Smsy.prof.st()$Y.prob.st
  Y.prob <- b.Smsy.prof()$Y.prob
#---------------------------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l')
  plot(S,Y.prob.st[1,],type='l',col=1, ylim=c(0,1),ylab = 'Probability',
       xlab=paste('Escapement',mult),main=paste0('MSY Yield probability curve')) 
  lines(S,Y.prob.st[2,],lty = 2,col=1)
  lines(S,Y.prob.st[3,],lty = 4,col=1)
  abline(h = 0.9,lwd=2,col=1)
  lines(S,Y.prob,lty = 1,col=4)
  abline(h = ap,lwd=2,col=2)
  tex <- c('90% MSY','80% MSY','70% MSY',paste0(input$p.msy,'% MSY'))
  legend('topright',tex,lty=c(1,2,4,1), col=c(1,1,1,4),box.lty=0)  
  })  


# Smsy Goal BEG Out
SA.BEG <- reactive({
  u <- as.numeric(input$ui)
  BEG.st <- b.Smsy.prof.st()$BEG.st/u
  BEG.1 <- b.Smsy.prof()$BEG/u
  t.BEG.st1 <- paste('90% MSY achieving 90% Probability',BEG.st[1,1],'-',BEG.st[1,2])
  t.BEG.st2 <- paste('80% MSY achieving 90% Probability',BEG.st[2,1],'-',BEG.st[2,2])
  t.BEG.st3 <- paste('70% MSY achieving 90% Probability',BEG.st[3,1],'-',BEG.st[3,2])
  t.BEG.1 <- paste(paste0(input$p.msy,'% MSY achieving ',input$p.msy.t,'% Probability'),BEG.1[1],'-',BEG.1[2])
  out <- list(tst1=t.BEG.st1,tst2=t.BEG.st2,tst3=t.BEG.st3,t1=t.BEG.1)
  return(out)
  })

output$Txt_Srange.smsy <-renderText({
  paste(SA.BEG()$tst1,SA.BEG()$tst2,SA.BEG()$tst3,SA.BEG()$t1,sep='\n')
  })

#---------------------------------------------------------------------------
#  Smsy Yield and Recruit Plot
#---------------------------------------------------------------------------
#-------- Yield Plot -------------------------------------------------------
output$Plt_yield.smsy <- renderPlot({
  u <- as.numeric(input$ui)
  BEG <- b.Smsy.prof()$BEG/u
  # Plot base Yield Plot
  replayPlot(boot.Yldp())
  # Plot escapement goal range 
  abline(v=BEG,lty=1,col=3)
  })
#-------- Recruit Plot -------------------------------------------------------
output$Plt_rec.smsy <- renderPlot({
  u <- as.numeric(input$ui)
  BEG <- b.Smsy.prof()$BEG/u
  # Plot base recruit Plot  
  replayPlot(boot.Recp())
  # Plot escapement goal range 
  abline(v=BEG,lty=1,col=3)
  })

#---------------------------------------------------------------------------
#  Smsy Escapement based Yield and Recruit Distribution 
#---------------------------------------------------------------------------
SR.Smsy <- reactive({
  BEG.1 <- b.Smsy.prof()$BEG[1]
  BEG.2 <- b.Smsy.prof()$BEG[2]
  S <- SR.pred.b()$S
  sr <- S[S>=BEG.1 & S<=BEG.2]
  Y <- SR.pred.b()$Y[,S%in%sr] 
  Y.p <- SR.pred.b()$Y.p[,S%in%sr]
  R <- SR.pred.b()$R[,S%in%sr] 
  R.p <- SR.pred.b()$R.p[,S%in%sr] 
  out <- list(S = S, Y = Y, R = R, Y.p = Y.p, R.p = R.p)
  return(out)
 })

#-----------------------------------------------------------------------
#  Plot distribution of Recruit and Yield at Given Escapement Range
#-----------------------------------------------------------------------
output$Plt_Smsy.dist <- renderPlot({
  par(mfrow=c(2,1),xaxs='i',yaxs='i',bty='l', cex=1.2)
  u <- as.numeric(input$ui)
  mult <- mult(u)
  # Mean estimate   
  # Bootstrap estimate  
#  plot(density(SR.Smsy()$R/u),main='Expected Mean Recruit',xlab=paste("Recruit",mult),ylab='')
  # Annualestimate   
  R.p <-SR.Smsy()$R.p
  R.p <- as.vector(R.p)
  R <-as.vector(SR.Smsy()$R)
  foo <- data.frame(Annual=R.p,Mean=R)/u
  boxplot(foo,horizontal = TRUE, ylim = c(0, 0.6*max(foo)),xlab=paste('Recruit',mult))
  foo2 <- data.frame(Annual=as.vector(SR.Smsy()$Y.p),Mean=as.vector(SR.Smsy()$Y))/u
  boxplot(foo2,horizontal = TRUE, ylim = c(min(foo2), 0.6*max(foo2)),xlab=paste('Recruit',mult))  
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
  print(summary(dat/u,digits=c(0,0,0,0)))
  })

#=============================================================================
#  Panel 3.2: SR Smax based Escapement Goal  
#=============================================================================
#  Calculate Smax Profile and escapement goal 
b.SMX <- reactive({
  mp <- input$p.rmx/100
  ap <- input$p.rmx.t/100
  S <- SR.pred.b()$S 
  R <- as.matrix(SR.pred.b()$R)
# Generate profle using profile function 
  Smax.prof  <- profile(S, R, mp, ap)
# Rename Output
  names(Smax.prof) <- c('S','R.prob','BEG')
  return(Smax.prof)   
  })


b.SMX.st <- reactive({
  S <- SR.pred.b()$S 
  R <- as.matrix(SR.pred.b()$R)
# Standard Minimum R % 
  st <- c(0.9,0.8,0.7)
# Standard target % is 90%
  tp <- 0.9
  ncols <- length(S)  
  R.prob.st <- matrix(0,nrow = 3,ncol=ncols)
  BEG.st <- matrix(0,nrow=3,ncol=2)
  for(i in 1:3){
    St.prof  <- profile(S, R, st[i], tp)
    R.prob.st[i,] <- St.prof$M.prof
    BEG.st[i,] <- St.prof$S.range
  }
  out <- list(R.prob.st = R.prob.st, BEG.st = BEG.st)
  return(out)   
  }) 

#-----------------------------------------------------------------------
#  Smax Profile Plot  
#-----------------------------------------------------------------------
output$Plt_Smax.prof <- renderPlot({
  mp <- input$p.rmx/100
  ap <- input$p.rmx.t/100
  u <- as.numeric(input$ui)
  mult <- mult(u)
  S <- b.SMX()$S/u
  R.prob.st <- b.SMX.st()$R.prob.st
  R.prob <- b.SMX()$R.prob
  #---------------------------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l')
  plot(S,R.prob.st[1,],type='l', col=1, ylim=c(0,1),ylab = 'Probability',
       xlab=paste('Escapement',mult),main=paste0('Smax probability curve')) 
  lines(S,R.prob.st[2,],lty = 2,col=1)
  lines(S,R.prob.st[3,],lty = 4,col=1)
  abline(h = 0.9,lwd=2,col=1)
  lines(S,R.prob,lty = 1,col=4)  
  abline(h = ap,lwd=2,col=2)
  tex <- c('90% Rmax','80% Rmax','70% Rmax',paste0(input$p.rmx,'% Rmax'))
  legend('topright',tex,lty=c(1,2,4,1),col=c(1,1,1,4),box.lty=0)  
  })  

#------------------------------------------------------------------------
#  Escapement goal table output 
#------------------------------------------------------------------------
# Smax Goal BEG Out
SM.BEG <- reactive({
  u <- as.numeric(input$ui)
  BEG.st <- b.SMX.st()$BEG.st/u
  BEG <- b.SMX()$BEG/u
  t.BEG.st1 <- paste('90% Rmax achieving 90% Probability',BEG.st[1,1],'-',BEG.st[1,2])
  t.BEG.st2 <- paste('80% Rmax achieving 90% Probability',BEG.st[2,1],'-',BEG.st[2,2])
  t.BEG.st3 <- paste('70% Rmax achieving 90% Probability',BEG.st[3,1],'-',BEG.st[3,2])
  t.BEG.1 <- paste(paste0(input$p.rmx,'% Rmax achieving ',input$p.rmx.t,'% Probability'),BEG[1],'-',BEG[2])
  out <- list(tst1=t.BEG.st1,tst2=t.BEG.st2,tst3=t.BEG.st3,t1=t.BEG.1)
  return(out)
  })

output$Txt_Srange.smax <-renderText({
  paste(SM.BEG()$tst1,SM.BEG()$tst2,SM.BEG()$tst3,SM.BEG()$t1,sep='\n')
 })


SR.Smax <- reactive({
  BEG.1 <- b.SMX()$BEG[1]
  BEG.2 <- b.SMX()$BEG[2]
  S <- SR.pred.b()$S
  Sr <- S[S>=BEG.1 & S<=BEB.2]
  Y <- SR.pred.b()$Y[,S%in%sr] 
  Y.p <- SR.pred.b()$Y.p[,S%in%sr] 
  R <- SR.pred.b()$R[,S%in%sr] 
  R.p <- SR.pred.b()$R.p[,S%in%sr] 
  out <- list(S = S, Y = Y, R = R, Y.p = Y.p, R.p = R.p)
  return(R)
  })



#-----------------------------------------------------------------------
#  Recruit plot with Smax Goal Range
#-----------------------------------------------------------------------
output$Plt_rec.smax <- renderPlot({
  u <- as.numeric(input$ui)
  BEG.1 <- b.SMX()$BEG/u
  # Plot Base Recruit Plot
  replayPlot(boot.Recp())
  # Plot Smax Escapement Goal Range  
  abline(v=BEG.1,lwd=1,col=4)
  })

#-----------------------------------------------------------------------
#  Distribution of Mean Recruit in Smax Goal Range
#-----------------------------------------------------------------------
output$Plt_yield.smax <- renderPlot({
  u <- as.numeric(input$ui)
  BEG.1 <- b.SMX()$BEG/u
  # Plot base Yield Plot
  replayPlot(boot.Yldp())
  # Plot escapement goal range 
  abline(v=BEG.1,col=4)
  }) 

#---------------------------------------------------------------------------
#  Smsy Escapement based Yield and Recruit Distribution 
#---------------------------------------------------------------------------
SR.Smax <- reactive({
  BEG.1 <- b.SMX()$BEG[1]
  BEG.2 <- b.SMX()$BEG[2]
  S <- SR.pred.b()$S
  sr <- S[S>=BEG.1 & S<=BEG.2]
  Y <- SR.pred.b()$Y[,S%in%sr] 
  Y.p <- SR.pred.b()$Y.p[,S%in%sr]
  R <- SR.pred.b()$R[,S%in%sr] 
  R.p <- SR.pred.b()$R.p[,S%in%sr] 
  out <- list(S = S, Y = Y, R = R, Y.p = Y.p, R.p = R.p)
  return(out)
})

#-------------------------------------------------------------------------------
#  Plot distribution of Recruit and Yield at Given Escapement Range
#-------------------------------------------------------------------------------
output$Plt_Smax.dist <- renderPlot({
  par(mfrow=c(2,1),xaxs='i',yaxs='i',bty='l', cex=1.2)
  u <- as.numeric(input$ui)
  mult <- mult(u)
  # Mean estimate   
  # Bootstrap estimate  
  #  plot(density(SR.Smsy()$R/u),main='Expected Mean Recruit',xlab=paste("Recruit",mult),ylab='')
  # Annualestimate   
  foo <- data.frame(Annual=as.vector(SR.Smax()$R.p),Mean=as.vector(SR.Smax()$R))/u
  boxplot(foo,horizontal = TRUE, ylim = c(0, 0.6*max(foo)),
          xlab=paste('Recruit',mult))
  foo2 <- data.frame(Annual=as.vector(SR.Smax()$Y.p),Mean=as.vector(SR.Smax()$Y))/u
  boxplot(foo2,horizontal = TRUE, ylim = c(min(foo2), 0.6*max(foo2)),
          xlab=paste('Yield',mult))  
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
  print(summary(dat/u,digits=c(0,0,0,0)))
  })

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  4.0 Target Yield based Escapement Goal
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
SR.prof.m <- reactive({
  # Import recruit and yield goal   
  rg <- ifelse(is.null(input$r1),median(sr.data()$R),input$r1)
  yg <- ifelse(is.null(input$y1),median(sr.data()$R-sr.data()$S),input$y1)
  # Prediction model s range  
  D <- floor(mean(log10(sr.data()$S)))
  S <- SR.pred.b()$S
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


 b.YAg <- reactive({
# Import User defined Yiedle Goal 
   yg <- input$y1
# Import S and Expected Yilds 
   S <- SR.pred.b()$S 
   Y <- SR.pred.b()$Y
   Y.p <- SR.pred.b()$Y.p
# For each bootstrap, determin if expected yields exceed desired goal  
   boot.Yb <- apply(Y,2,function(x) ifelse(x >yg,1,0))
   boot.Ybp <- apply(Y.p,2,function(x) ifelse(x >yg,1,0))   
# calculate mean: This is the same as probability   
   boot.Ypm <- colMeans(boot.Yb)
   boot.Ypa <- colMeans(boot.Ybp)
# Import Mean, Upper, Lower Yield range    
   Ym <- b.YA()$Ym
   Yu <- b.YA()$Yu
   Yl <- b.YA()$Yl  
# Find Intersection S range  
   b.l <- S[Yl > yg]
   b.m <- S[Ym > yg]
   b.u <- S[Yu > yg]
# Calculate and output S range    
   BEG.l <- c(NA,NA)
   BEG.m <- c(NA,NA)
   BEG.u <- c(NA,NA)
   if(sum(b.l) != 0) {BEG.l <- c(min(b.l),max(b.l))}
   if(sum(b.m) != 0) {BEG.m <- c(min(b.m),max(b.m))}
   if(sum(b.u) != 0) {BEG.u <- c(min(b.u),max(b.u))}
   out <- list(boot.Yp=boot.Ypm, boot.Ypa=boot.Ypa, BEG.l = BEG.l, BEG.m = BEG.m, BEG.u=BEG.u)
   return(out)
 })

# Optimum Mean and annual Yield Proflie Plot 
output$Plt_yield.prof <- renderPlot({
   u <- as.numeric(input$ui)
   mult <- mult(u)
   yg <- input$y1
   ypg <- input$y1p/100
# Bootstrap probile 
   S <- b.YA()$S/u
   boot.Yp <- b.YAg()$boot.Yp
   boot.Ypa <- b.YAg()$boot.Ypa
# Parametric model probile 
   s <- SR.prof.m()$S/u
   Yci <- SR.prof.m()$prof.Yci
   Ypi <- SR.prof.m()$prof.Ypi
# Create a plot 
   par(xaxs='i',yaxs='i',bty='l')
   plot(S,boot.Yp,type='l',ylim=c(0,1),ylab = 'Probability',xlab=paste("Escapement",mult),
        main=paste('Minimum',yg,'Yield probability plot')) 
   lines(s,Yci,col='grey')
   lines(S,boot.Ypa,lty=2)
   lines(s,Ypi,col='grey',lty=2)
   abline(h = ypg,lwd=2,col=2)
 }) 
 
 # Find Yield Target Intersection 
 b.YApg <- reactive({
   ypg <- input$y1p/100
   S <- SR.pred.b()$S 
   Yp <- b.YAg()$boot.Yp
   # Find Intersections 
   b.p <- S[Yp > ypg]
   BEG.p <- c(NA,NA)
   if(sum(b.p) > 0){BEG.p <- c(min(b.p),max(b.p))}
   out <- BEG.p
   return(out)
 })
 
 # Print Optimum Yield Proflie Goal Range  
 output$Txt_Srange_yg <-renderText({
   u <- as.numeric(input$ui)
   BEG.p <- b.YApg()
   paste('Escapement Goal Range:',BEG.p[1],'-',BEG.p[2])
 })
 
 
# Yield Plot 
 output$Plt_yield.gl <- renderPlot({
   u <- as.numeric(input$ui)
   yg <- input$y1/u
   replayPlot(boot.Yldp())
   abline(h=yg,lwd=2,col=2)
   abline(v=b.YApg(),lwd=2,col=3)
 }) 
 
 # Print Escapement Goal Range
 output$by<- renderText({
   mp <- input$p.i
   u <- as.numeric(input$ui)
   BEG.l <- b.YAg()$BEG.l/u
   BEG.m <- b.YAg()$BEG.m/u
   BEG.u <- b.YAg()$BEG.u/u
   t.BEG.l <- paste('Lower',mp,'% Limit',BEG.l[1],'-',BEG.l[2])
   t.BEG.m <- paste('Mean              ',BEG.m[1],'-',BEG.m[2])
   t.BEG.u <- paste('Upper',mp,'% Limit',BEG.u[1],'-',BEG.u[2])
   paste(t.BEG.l,t.BEG.m,t.BEG.u,sep='\n')
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
 
b.RAg <- reactive({
# Import Targe Recruitment goal   
  rg <- input$r1
# Import bootstrap S and Recruit 
  S <- SR.pred.b()$S 
  R <- as.matrix(SR.pred.b()$R)
  R.p <- as.matrix(SR.pred.b()$R.p)
# For each bootstrap, determin if expected Recruitment exceed desired goal  
  Rp <- apply(R,2,function(x) ifelse(x >rg,1,0))
# Recruitment target probabilty profil   
  Rp <- colMeans(Rp)
  Rpa <- apply(R.p,2,function(x) ifelse(x >rg,1,0))
  # Recruitment target probabilty profil   
  Rpa <- colMeans(Rpa)   
   Rm <- b.YA()$Rm
   Ru <- b.YA()$Ru
   Rl <- b.YA()$Rl  
# Find Intersections 
   b.l <- S[Rl > rg]
   b.m <- S[Rm > rg]
   b.u <- S[Ru > rg]
   BEG.l <- c(NA,NA)
   BEG.m <- c(NA,NA)
   BEG.u <- c(NA,NA)   
   if(sum(b.l) != 0) {BEG.l <- c(min(b.l),max(b.l))}
   if(sum(b.m) != 0) {BEG.m <- c(min(b.m),max(b.m))}
   if(sum(b.u) != 0) {BEG.u <- c(min(b.u),max(b.u))}
   out <- list(Rp=Rp, Rpa = Rpa,BEG.l = BEG.l, BEG.m = BEG.m, BEG.u=BEG.u)
   return(out)
 })


#------------------------------------------------------------------------------
#  Calculate probability that intercects profile 
#------------------------------------------------------------------------------
b.RApg <- reactive({
   rpg <- input$r1p/100
   S <- SR.pred.b()$S 
   Rp <- b.RAg()$Rp
   # Find Intersections 
   b.p <- S[Rp>rpg]
   BEG.p <- c(NA,NA)
   if(sum(b.p) >0) {BEG.p <- c(min(b.p),max(b.p))}
   out <- BEG.p
   return(out)
 })

# Recruit Plot 
output$Plt_rec.gl <- renderPlot({
  u <- as.numeric(input$ui)
  rg <- input$r1/u
  # Plot Base Recruit Plot
  replayPlot(boot.Recp())
  abline(h=rg,lwd=2,col=2)
  abline(v=b.RApg(),lwd=2,col=3)
}) 

# Print Base Recruit Goal  
output$brt <-renderText({
  u <- as.numeric(input$ui)
  mp <- input$p.i
  BEG.l <- b.RAg()$BEG.l
  BEG.m <- b.RAg()$BEG.m
  BEG.u <- b.RAg()$BEG.u
  t.BEG.l <- paste('Lower',mp,'% Limit',BEG.l[1],'-',BEG.l[2])
  t.BEG.m <- paste('Mean              ',BEG.m[1],'-',BEG.m[2])
  t.BEG.u <- paste('Upper',mp,'% Limit',BEG.u[1],'-',BEG.u[2])
  paste(t.BEG.l,t.BEG.m,t.BEG.u,sep='\n')
})


#  Minimum Recruit Proflie Plot 
output$Plt_rec.prof <- renderPlot({
  rg <- input$r1
  rpg <- input$r1p/100
  u <- as.numeric(input$ui)
  mult <- mult(u)
  S <- b.YA()$S/u
  Rp <- b.RAg()$Rp
  Rpa <- b.RAg()$Rpa  
  # Parametric model probile 
  s <- SR.prof.m()$S/u
  Rci <- SR.prof.m()$prof.Rci
  Rpi <- SR.prof.m()$prof.Rpi
  par(xaxs='i',yaxs='i',bty='l')
  plot(S,Rp,type='l',ylim=c(0,1),ylab = 'Probability',xlab=paste("Escapement",mult),
       main=paste('Minimum',rg,'Recruit probability Plot')) 
  lines(s,Rci,col='grey')
  lines(S,Rpa,lty=2)
  lines(s,Rpi,col='grey',lty=2)  
  abline(h = rpg,lwd=2,col=2)
})  


# Optimum Recruit Proflie Escapement Goal 
output$Txt_Srange_rg <-renderText({
  u <- as.numeric(input$ui)
  BEG.p <- b.RApg()
  paste('Escapement Goal Range:',BEG.p[1],'-',BEG.p[2])
})

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#   User defined escapement goal range analyse s
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  1.0 Bootstrap estimation of Mean and Annual Yield and Recruit    
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
b.GA <- eventReactive(input$Run,{
#-----------------------------------------------------------------------  
  progress <- Progress$new(session, min=1, max=15)
  on.exit(progress$close())
  progress$set(message = 'Bootstrap EG (7.0) in progress',
               detail = 'This may take a while...')
  for (i in 1:15) {progress$set(value = i)}
#-----------------------------------------------------------------------   
#  Import user defined lower and upper goal 
   lg <- input$lg
   ug <- input$ug
# create goal range 
   S <- seq(lg,ug,length.out=201)   
# Import bootstrap SR model parameters 
   lnalpha <- boot.SR()$ln.alpha
   beta <- boot.SR()$beta
   sigma <- boot.SR()$sigma
   nrows <- length(lnalpha)
# Calculate predicted mean Recruit    
   R <- t(S*t(exp(lnalpha-beta%o%S)))
# Calculate predicted mean Yield
   Y <- t(t(R)-S)
# Calculte predicted annnual Recruitment 
# PI us expected + rnorm with sigma   
   R.p <- matrix(NA,nrow = nrows,ncol=201)
   for(i in 1:nrows){
     R.p[i,] <- exp(rnorm(201,log(R[i,]),sigma[i]))
   }
# Calculte predicted annnual Yield 
   Y.p <- t(t(R.p)-S)   
# Outputs are mean and annual Yields and recruits within proposed S range.    
   out <- list(S = S, R = R, Y = Y, R.p = R.p, Y.p = Y.p)
   return(out) 
   })


#-----------------------------------------------------------------------
#  3.0: SRp.G: SR model based Parametric PI and CI
#-----------------------------------------------------------------------
SRp.G <- eventReactive(input$Run,{
  #-----------------------------------------------------------------------  
  progress <- Progress$new(session, min=1, max=15)
  on.exit(progress$close())
  progress$set(message = 'Bootstrap Profiling in progress',
               detail = 'This may take a while...')
  for (i in 1:15) {progress$set(value = i)}
  #-----------------------------------------------------------------------   
  # Import Lower and Upper goal range
  lg <- input$lg
  ug <- input$ug
  # create S sequencce 
  S <- seq(lg,ug,length.out=101)
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
  boot.t <- matrix(0,nrow=101,ncol=1000)
  for (i in 1:101){
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
output$Plt_yield.cs <- renderPlot({
  par(mfrow=c(2,1),xaxs='i',yaxs='i',bty='l')
  u <- as.numeric(input$ui)
  mult <- mult(u)
  yg <- input$yg/u
# Boootstrap estimate  
  plot(density(b.GA()$Y/u),main='Expected Mean Yields',xlab=paste("Yield",mult),ylab='')
# Parameteric estimate   
  lines(density(SRp.G()$bYci/u), col = 'grey')
  abline(v=yg,lty=2,col=2)
# Plot Annual Yield  
  Y.p <-b.GA()$Y.p
  Y.p <- Y.p[Y.p < quantile(Y.p,0.995)]
  plot(density(Y.p/u), main='Expected Annual Yields',xlab=paste("Yield",mult),ylab='')
  lines(density(SRp.G()$bYpi/u), col = 'grey')
  abline(v=yg,lty=2,col=2)
}) 

#-----------------------------------------------------------------------
#  Plot distribution of Recruit and Yield at Given Escapement Range
#  SR model Parameteric based CI and PI
#-----------------------------------------------------------------------
output$EGR.Rec <- renderPlot({
  par(mfrow=c(2,1),xaxs='i',yaxs='i',bty='l', cex=1.2)
  u <- as.numeric(input$ui)
  mult <- mult(u)
  rg <- input$rg/u
# Mean estimate   
# Bootstrap estimate  
  plot(density(b.GA()$R/u),main='Expected Mean Recruit',xlab=paste("Recruit",mult),ylab='')
# Parametric estimate
  lines(density(SRp.G()$bRci/u), col = 'grey')
  abline(v=rg,lty=2,col=2)
# Annualestimate   
  R.p <-b.GA()$R.p
  R.p <- R.p[R.p < quantile(R.p,0.995)]
# Bootstrap estimate
  plot(density(R.p/u), main='Expected Annual Recruit',xlab=paste("Recruit",mult),ylab='')
# Parametric estimate   
  lines(density(SRp.G()$bRpi/u), col = 'grey')
  abline(v=rg,lty=2,col=2)
}) 


output$bGAs <- renderPrint({
  print(summary(),digits=0)
})

# Calculate Probability meeting target  
output$Txt_prob.tgt.yld <- renderText({
  yg <- input$yg
  pyg <- sum(ifelse(b.GA()$Y>yg,1,0))/length(b.GA()$Y)
  pygp <- sum(ifelse(SRp.G()$bYci>yg,1,0))/length(SRp.G()$bYci)
  pyga <- sum(ifelse(b.GA()$Y.p>yg,1,0))/length(b.GA()$Y.p)
  pygap <- sum(ifelse(SRp.G()$bYpi>yg,1,0))/length(SRp.G()$bYpi)
  t.pyg <- paste('Meeting Target Mean :',round(100*pyg,0),'%')
  t.pygp <- paste('Meeting Target Mean(p) :',round(100*pygp,0),'%')
  t.pyga <- paste('Meeting Target Annual:',round(100*pyga,0),'%')
  t.pygap <- paste('Meeting Target Annual(p) :',round(100*pygap,0),'%')
  paste(t.pyg,t.pygp,t.pyga,t.pygap,sep='\n')
})


output$bGAfp <- renderPlot({
  par(mfrow=c(2,1))
  u <- as.numeric(input$ui)
  rg <- input$rg/u
  lg <- input$lg/u
  ug <- input$ug/u
  yg <- input$yg/u
  # Plot Base Recruit Plot
  replayPlot(boot.Recp())
  abline(h=rg,lwd=2,col=2)
  abline(v=c(lg,ug),lty=2,col=2)
  replayPlot(boot.Yldp())
  abline(h=yg,lwd=2,col=2)
  abline(v=c(lg,ug),lty=2,col=2)   
})  


output$bGASRs <- renderPrint({
  dat1 <- SRp.G()$bRci
  dat2 <- as.vector(b.GA()$R)
  dat3 <- SRp.G()$bRpi
  dat4 <- as.vector(b.GA()$R.p)
  names(dat) <- c('Recruit','Yields')
  print(summary(dat),digits=0)
})

# Calculate Probability meeting target  
output$Txt_prob.tgt.rec <- renderText({
  rg <- input$rg
  prg <- sum(ifelse(b.GA()$R>rg,1,0))/length(b.GA()$R)
  prgp <- sum(ifelse(SRp.G()$bRci>rg,1,0))/length(SRp.G()$bRci)
  prga <- sum(ifelse(b.GA()$R.p>rg,1,0))/length(b.GA()$R.p)
  prgap <- sum(ifelse(SRp.G()$bRpi>rg,1,0))/length(SRp.G()$bRpi)
  t.prg <- paste('Meeting Target Mean :',round(100*prg,0),'%')
  t.prgp <- paste('Meeting Target Mean(p) :',round(100*prgp,0),'%')
  t.prga <- paste('Meeting Target Annual:',round(100*prga,0),'%')
  t.prgap <- paste('Meeting Target Annual(p) :',round(100*prgap,0),'%')
  paste(t.prg,t.prgp,t.prga,t.prgap,sep='\n')
})

# Optimum Recruit Proflie Plot 
output$SRrp <- renderPlot({
  par(mfrow=c(1,2),xaxs='i',yaxs='i',bty='l')
  rg <- input$rg
  yg <- input$yg
  u <- as.numeric(input$ui)
  mult <- mult(u)
  s <- SR.pred.m()$s/u
  Rci <- SR.pred.m()$prof.Rci
  Rpi <- SR.pred.m()$prof.Rpi
  Yci <- SR.pred.m()$prof.Yci
  Ypi <- SR.pred.m()$prof.Ypi
  plot(s,Rci,type='l',ylim=c(0,1),ylab = 'Probability',xlab=paste("Escapement",mult),
       main=paste('Minimum',rg,'Recruit probability Plot')) 
  lines(s,Rpi,lty=2)
  plot(s,Yci,type='l',ylim=c(0,1),ylab = 'Probability',xlab=paste("Escapement",mult),
       main=paste('Minimum',yg,'Yield probability Plot')) 
  lines(s,Ypi,lty=2)
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
  x <- brood.table()  
# Just retrive brood recuit by age 
  x <- x[complete.cases(x),-c(1,2,dim(x)[2])]
# Calculate brood recurit age prop  
  p.x <- x/rowSums(x)
# Calculate mean age recruit
  p.i <- colMeans(p.x)
# Set Drishelet   
  D <- input$D
  phi <- input$phi
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
  ar1 <- function(n,cv,alpha){
    ar1 <- numeric(n)
    ar1[1] <- 0
    for(i in 2:n){
      ar1[i] <- alpha*ar1[i-1]+runif(1,-cv,cv)
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
  mult <- mult()
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
  mult <- mult() 
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
                               