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
library(datasets)
library(mgcv)         # used for spline 
library(maptools)     # used for unoverlapping labels 
library(coda)         # used to read MCMC data 
library(R2jags)       # used to run JAGS
library(openxlsx)     # used for creating EXCEL output table  
#=======================================================================    
#  UI:  
#=======================================================================
ui<-fluidPage(
 navbarPage(
    theme = shinytheme("cerulean"),  
    "Pacific Salmon Escapement Goal Analyses",
#-----------------------------------------------------------------------
#  Panel 1:  Data Input and Submit 
#-----------------------------------------------------------------------
#navbarMenu("Data Input & Bayes model",
  tabPanel("Data Input",
   sidebarPanel(width = 3,
                
  selectInput(inputId="dataType","Data Type", choices = c('S-R','Run')),
#---------------------------------------------------    
#  File Inuput
#---------------------------------------------------
# Input: Select a file ----
  fileInput("file1", "ChooseFile",
         multiple = TRUE,
          accept = c("text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")),
# Input: Checkbox if file has header ----
    checkboxInput("header", "Header", TRUE),

# Input: Select separator ----
    radioButtons("sep", "Separator",
      choices = c(Comma = ",", Tab = "\t"), selected = ","),
    # Horizontal line ----
    tags$hr(),
#-----------Run data Input UI--------------------------------------------
  conditionalPanel(
    condition = "input.dataType == 'Run'",
# Input: Checkbox if file has header ----
    p("Select First age of run"),
# Input: Select what to display
    numericInput("fage", "First Retun Age", value=4,min=1,max=20,step=1)
  ),# End Conditional Panel
p(strong("Choose brood year range")),
uiOutput('yrange')
   ), # End SidePanel

#=========================MainPanel=========================================
  mainPanel(
    tabsetPanel(
#------------------ Show Input data --------------------------------------------      
      tabPanel("Table",    
          conditionalPanel(condition = "input.dataType == 'S-R'",
              h4("S-R Data file column orders: Year, Spawner (Escapement), Recruit")
               ),
          conditionalPanel(condition = "input.dataType == 'Run'",
              h4("Run Data file column orders: Year, Escapement, Run,
                   Run by age (or proportion) from youngest to oldest")
                 ),
               dataTableOutput("Tbl_data")),
#------------------ Brood Table ------------------------------------------------      
      tabPanel("Brood Table",
               conditionalPanel(condition = "input.dataType == 'Run'",
                 dataTableOutput("Tbl_data.brood"))
               # Horizontal line ----
                  ),
#------------------ Time Series ------------------------------------------------
      tabPanel("Time Series",
          conditionalPanel(condition = "input.dataType == 'Run'",
              plotOutput('Plt_runesc')
                ),  
              plotOutput("Plt_srt")
                ),#End tabPanel
#------------------ Data summary  ----------------------------------------------
      tabPanel("Summary",
             verbatimTextOutput('Txt_sum.data'),
               plotOutput('Plt_hist.sry')
#             ),
#      tabPanel("To be included",
           
          )  # End tabPanel
        )  # End tabsetPanel
      )  # End mainPanel
     ), # End tabpanle

#tabPanel("Run Bayesian Model",
#    sidebarPanel(width = 3,
#        p("Bayesian Model Setting"),
#        numericInput(inputId='n.iter','Simulation Length',value=10000,min=0,step=10000), 
#        numericInput(inputId='n.burnin','Burn-in Length',value=1000,min=0,step = 1000),
#        numericInput(inputId='n.thin','Thinning',value=10,min=0,step = 1),
#        numericInput(inputId='n.chain','Number of Chains',value=1,min=1,step = 1),
#        selectInput('Model',"Select SR Model",choice=list('Ricker','Beverton-Holt'),selected ='Ricker'),
#        checkboxInput(inputId="ar1", "Add AR(1) Error", FALSE),
#        p("Start Bayesian Analyses")
#        actionButton("RunBayes","Run")                                 
#         ),  # End sidebarPanel
#=========================MainPanel=============================================           
#         mainPanel(
#           tabsetPanel(
#             tabPanel("Bayes Model",
#                      p(strong("Trace Plots")),
#                      plotOutput("Plt_trace"),
#                      p(strong("Model summary")), 
#                      verbatimTextOutput('BayesSum')  
#                 ),
#             tabPanel("Trace Plots",
#                      plotOutput("Plt_trace")
#             ),
#             tabPanel("Bayesian Model",
#                      verbatimTextOutput('test'),
#             ),
#             id = "conditionedPanels"
#           )#End tabsetPanel
#        )#End mainPanel
#   ),#End tabPanel
 #),  # End NavbarMenue

#-------------------------------------------------------------------------------
#  Panel 2  Data Analyses 
#-------------------------------------------------------------------------------
#navbarMenu("SR Model Analyses",
  tabPanel("SR Model",
    sidebarPanel(width = 3,
#---------  Bayes Model Control Sidebar ----------------------------------------                 
      conditionalPanel(condition="input.Panel == 'Bayes Model'|| input.Panel == 'Model Codes'",
        p("Bayesian Model Setting"),
        numericInput(inputId='n.iter','Simulation Length',value=10000,min=0,step=10000), 
        numericInput(inputId='n.burnin','Burn-in Length',value=1000,min=0,step = 1000),
        numericInput(inputId='n.thin','Thinning',value=10,min=0,step = 1),
        numericInput(inputId='n.chain','Number of Chains',value=1,min=1,step = 1),
        selectInput('Model',"Select SR Model",choice=list('Ricker','Beverton-Holt'),selected ='Ricker'),
        checkboxInput(inputId="ar1", "Add AR(1) Error", FALSE),
        p("Start Bayesian Analyses"),
        actionButton("RunBayes","Run")
          ),

#---------  SR Modle Sidebar ----------------------------------------
      conditionalPanel(condition="input.Panel == 'SR Plot'||
                   input.Panel == 'Yield Plot'||input.Panel == 'Residuals'
                   ||input.Panel == 'MCMC'",                  
        selectInput(inputId="ui","Axis Dislpay Unit", choices = c(1,1000,1000000)),  
        p("Escapement Goal Range"),
        numericInput(inputId='egl','Lower Goal',value=0,min=0), 
        numericInput(inputId='egu','Upper Goal',value=0,min=0),    
        checkboxInput(inputId="show.eg", "Show Escapement Goal", FALSE),
        checkboxInput(inputId="show.points", "show Years", FALSE), 
        checkboxInput(inputId="show.smsy", "show Smsy", FALSE),
        checkboxInput(inputId="show.smax", "show Smax", FALSE),
        checkboxInput(inputId="show.int", "show Interval", TRUE),
        numericInput("CI", "% Credible Interval", value=90,min=0,max=100,step=5),
        selectInput(inputId="Li","Interval Type", choices = c('confidence','prediction'))
          )  
       ), # End sidebarPanel
       
#=========================MainPanel=============================================       
  mainPanel(tabsetPanel(
#------------------ Bayes Model ------------------------------------------------        
    tabPanel("Bayes Model",
        p(strong("Trace Plots")),
        plotOutput("Plt_trace"),
        p(strong("Model summary")), 
        verbatimTextOutput('BayesSum')  
          ),#End tabPanel
#------------------ SR Plot-----------------------------------------------------        
    tabPanel("SR Plot",
        plotOutput(height='500px','Plt_SR'),
        downloadButton("down", label = 'Download the plot')
            ),#End tabPanel
#------------------ Yield Plot--------------------------------------------------        
    tabPanel("Yield Plot",
        plotOutput(height='500px','Plt_yield')
            ),#End tabPanel
#------------------ Residuals  ------------------------------------------------- 
    tabPanel("Residuals", 
        plotOutput("Plt_residual"),
        p(strong("Durbin-Watson Serial Correlation Analyses")), 
        verbatimTextOutput('dwtest')
            ),#End tabPanel
#------------------ MCMC ------------------------------------------------------- 
    tabPanel("MCMC",
        verbatimTextOutput('sumpost'),
        plotOutput("Plt_hist.mc")
            ),#End tabPanel
#------------------ Bayes Model Code ------------------------------------------- 
    tabPanel("Model Codes",
        verbatimTextOutput('test')
            ),#End tabPanel
    id = "Panel"
           )#End tabsetPanel
        )#End mainPanel
      ),#End tabPanel
#   ),#End nabVarMenu

#-----------------------------------------------------------------------
#  Panel 3: Escapement Goal Analyses 
#-----------------------------------------------------------------------
navbarMenu("Escapement Goal Analyses",
#------------------------------------------------------------------------    
#  tabPanel: Smsy Goal Analyses 
#------------------------------------------------------------------------    
  tabPanel("Smsy Goal Analyses",      
    sidebarPanel(width = 3,
      p(strong("Smsy Analyses")),  
      sliderInput("p.msy", "Min % of MSY", value=90,min=0,max=100,step=5),
      sliderInput("p.msy.t", "% Meeting MSY Target", value=90,min=0,max=100,step=5)
          ),#End sidebarPanel

#=========================MainPanel=============================================       
    mainPanel(
      tabsetPanel(
#------------------ Smsy Profile ----------------------------------------------- 
        tabPanel("Smsy Profile",
          plotOutput(height='500px','Plt_Smsy.prof'),
          verbatimTextOutput("Txt_Srange.smsy")
            ), #End tabPanel
#------------------ Smsy Yield Profile -----------------------------------------
        tabPanel("Yield  & Recruit Profile",
#               splitLayout(cellWidths = c("50%", "50%"),
          plotOutput(height='300px','bsmsy.y'),
          plotOutput(height='300px','bsmsy.r')
#              )
              ) #End tabPanel
            )#End tabsetPanel
        )#End mainPanel
      ),#End tabPanel: Smsy Goal Analyses 
#------------------------------------------------------------------------    
# tabPanel  Smax Goal Analyses 
#------------------------------------------------------------------------    
  tabPanel("Smax Goal Analyses",
    sidebarPanel(width = 3,
      p(strong("Smax Analyses")),            
      sliderInput("p.max", "Min % of Rmax", value=90,min=0,max=100,step=5),
      sliderInput("p.max.t", "% Meeting Rmax Target", value=90,min=0,max=100,step=5)
        ), # End sidebarPanel 

#=========================MainPanel=============================================       
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
#                      )
            )# End tabPanel
          )#End tabsetPanel
        )#End mainPanel
      ),#End tabPanel: Smax Goal Analyses
              
#------------------------------------------------------------------------    
#  tabPanel Recruit & Yield Goal Analyses 
#------------------------------------------------------------------------    
  tabPanel("Yield & Recruit Goal Analyses",
    sidebarPanel(width = 3,
      conditionalPanel(condition="input.cPanel == 'Recruit Goal Analyses'",  
        p(strong("Recruit Goal Analyses")), 
        numericInput("r1", "Min Mean Recruit", value=100000,min=0, step=10000),
        sliderInput("r1p", "Min % Achieve", value=90,min=0, max=100,step=5)
        ), # End conditionalPanel
      conditionalPanel(condition="input.cPanel == 'Yield Goal Analyses'",                        
        p(strong("Yield GoalAnalyses")),            
        numericInput("y1", "Min Mean Yield", value=100000,min=0, step=10000),
        sliderInput("y1p", "Min % Achieve", value=90,min=0, max=100,step=5)
        ) # End conditionalPanel
      ),  # End sidebarPanel

#=========================MainPanel=============================================       
    mainPanel(
      tabsetPanel(
#------------------ Recruit Goal Profile ----------------------------------------------   
        tabPanel("Recruit Goal Analyses",
#                      splitLayout(cellWidths = c("50%", "50%"),
          plotOutput(height='300px','Plt_rec.gl'),
          plotOutput(height='300px','Plt_rec.prof'),
                 #                      )
          splitLayout(cellWidths = c("50%", "50%"),
#            verbatimTextOutput("brt"),
            textOutput("brpt"))
          ),# End tabPanel
        
#------------------ Yield Goal  Profile -----------------------------------------------                
        tabPanel("Yield Goal Analyses",
#                      splitLayout(cellWidths = c("50%", "50%"),
          plotOutput(height='300px','Plt_yield.gl'),
          plotOutput(height='300px','Plt_yield.prof')
#                      )
            ,
          splitLayout(cellWidths = c("50%", "50%"),
#            verbatimTextOutput("byt"),
            textOutput("bypt"))),
                           
        id = "cPanel"
          )#End tabsetPanel
        )#End maiPanel
      ),#End tabPanel Recruit & Yield Goal Analyses 
              
#-------------------------------------------------------------------------------    
# tabPanel Custom Escapement Goal Evaluation 
#-------------------------------------------------------------------------------    
  tabPanel("Escapment Goal Evaluation",
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

#============= Main Panel ======================================================    
    mainPanel(
      tabsetPanel(
#------------------ Expected Mean Recruit and Yields ----------------------------------   
        tabPanel("Expected Mean & Annual Yield",
          plotOutput(height='600px',"EGR.Yield"),
            splitLayout(cellWidths = c("50%", "50%"),
              p(strong("Recruit and Yields Summary")),
              p(strong("Probability of Meeting Target"))),
            splitLayout(cellWidths = c("50%", "50%"),
              verbatimTextOutput("bGAs"),
              verbatimTextOutput("bGAt"))
          ), #End tab Panel
                           
#------------------ Recruit Annual Recruit and Yields ---------------------------------   
        tabPanel("Expected Mean & Annual Recruit",
          plotOutput(height='600px',"EGR.Rec"),
            splitLayout(cellWidths = c("50%", "50%"),
              p(strong("Recruit and Yields Summary")),
              p(strong("Probability of Meeting Target"))),
            splitLayout(cellWidths = c("50%", "50%"),
              verbatimTextOutput("bGASRs"),
              verbatimTextOutput("bGASRt"))
          ) #End tab Panel
        )#End tabsetPanel
      )#End main Panel 
    )#End tabPanel Custom Escapement Goal Evaluation
    )#End nabVarMenu
  ),#End nabVarPage
#-------------------------------------------------------------------------------
# Citation Discraimer  
#-------------------------------------------------------------------------------
# withMathJax(),
hr(),
h5("Disclaimer"),
print(strong("This App is developed by Toshihide Hamachan Hamazaki, Alaska Department of Fish and Game Division of Commercial Fisheries")),

h5("Contact about this applicaiton"), 
print(strong("Questions and improvement suggestions? Please contact",
a(href="mailto:toshihide.hamazaki@alaska.gov", "Hamachan"))),
h5("Suggested Citation"),
print(strong(paste("Hamazaki, T.",format(Sys.Date(), "%Y"),". Missing Passage Estimation Analyses(source: https://shiny.rstudio.com/). Available from https://hamachan.shinyapps.io/Missed_Run/")))
 )#End fluidPage

#===============================================================================    
#  Server:  
#===============================================================================
server<-shinyServer(function(input, output, session){
#-------------------------------------------------------------------------------
#  Helper Unit functions  
#-------------------------------------------------------------------------------
#----  Show multiples ----------------------------------------------------------  
  mult <- reactive({
    u <- as.numeric(input$ui)
    mult <- ifelse(u==1000000,paste0('(x million)'),ifelse(u>1,paste0('(x',u,')'),''))  
    return(mult)
  })
  
#----- Show Color Shades -------------------------------------------------------   
  tcol <- function(color, percent = 50, name = NULL) {
    #	  color = color name
    #	percent = % transparency
    #	   name = an optional name for the color
    ## Get RGB values for named color
    rgb.val <- col2rgb(color)
    ## Make new color using input color as base and alpha set by transparency
    t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
          max = 255,
          alpha = (100-percent)*255/100,
          names = name)
    return(t.col)
    }
  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 1: Data upload and output 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# data ---- Uploaded data ------------------------------------------------------
data <- reactive({
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.
  req(input$file1)
  df <- read.csv(input$file1$datapath,
                 header = input$header,
                 sep = input$sep)
  return(df)
  })  

# table ---- Uploaded data table output ----------------------------------------
output$Tbl_data <- renderDataTable({
      data()
  })  
  

# Plt_runesc --- Plot Run-Escapement Time series (when data is "Run") --------------
output$Plt_runesc <- renderPlot({
  x <- data()[,c(1:3)]
  names(x) <-c('Yr','S','R')
  u <- as.numeric(input$ui)
  mult <- mult()
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
  
# brood.table--- Construct brood table (when data is "Run") --------------------   
brood.table <- reactive({
   if(input$dataType== "Run"){
    x <- data()
# sum first run age: if p   
    p <- round(sum(x[1,-c(1:3)]),0)
    fage <- input$fage
    nages <- dim(x)[2]-3
    lage <- fage+nages-1
    yr <- c(min(x[,1])-seq(lage,1),x[,1])
    brood <- matrix(0,ncol=nages+2,nrow = length(yr))
    brood[,1] <- yr
    brood[,2] <- c(rep(NA,lage),x[,2])
    for(i in 1:nages){
        if(p==1){
        brood[,i+2] <- c(rep(NA,lage-fage+1-i),x[,3+i]*x[,3],rep(NA,fage+i-1))
        }
        else{
        brood[,i+2] <- c(rep(NA,lage-fage+1-i),x[,3+i],rep(NA,fage+i-1))  
          }
        }
    brood.c <- data.frame(brood)
    names(brood.c) <- c('b.Year','Spawner',paste0('b.Age',seq(fage,lage)))
    brood.c$Recruit <- rowSums(brood.c[,-c(1:2)])
    return(brood.c)
      }
  })

# Tbl_data.brood ----- show brood table ------------------------------------------------
  output$Tbl_data.brood <- renderDataTable({
      round(brood.table(),0)
    }) 

#-------------------------------------------------------------------------------
#  Create SR data 
#-------------------------------------------------------------------------------

# sr.data.0 --- Original sr dataset --------------------------------------------
sr.data.0 <- reactive({
   if(input$dataType== "Run"){
     x <- brood.table()
     x <- x[complete.cases(x),c(1,2,dim(x)[2])]
   } else if (input$dataType== "S-R"){
     x <- data()
   }
   names(x) <- c('Yr','S','R')
   return(x)     
 })
 
# yrange --- UI for user selecting data range ----------------------------------
output$yrange = renderUI({
   year <- sr.data.0()$Yr   # Extract brood year data range 
   fyear <- min(year)       # First brood year 
   lyear <- max(year)       # Last brood year
#  Slider input UI 
   sliderInput("sryears", label = "year range", min = fyear, max = lyear, value = c(fyear, lyear),step=1,sep = "")
 })
 
# sr.data --- final dataset used for SR analyses -------------------------------
sr.data <- reactive({
   x <- sr.data.0()
   fyear <- input$sryears[1]
   lyear <- input$sryears[2]    
   x <- x[x$Yr>=fyear & x$Yr<=lyear,]
   return(x)     
  })
 
# summaty -------- sr data summary Output --------------------------------------
output$Txt_sum.data <- renderPrint({
  dat <- sr.data()
  dat$Y <- dat$R-dat$S
  summary(dat[,-1])  # Remove year 
  })

# Plt_hist.sry ---------- sr data histogranm ------------------------------------------- 
output$Plt_hist.sry <- renderPlot({
  par(mfrow=c(1,3))
  x <- sr.data()
  x$Y <- x$R-x$S
  hist(x$S,main='',xlab='Spawnter')
  hist(x$R,,main='',xlab='Recruit')
  hist(x$Y,,main='',xlab='Yield') 
  })

# Plt_srt ---------- Plot SR time series --------------------------------------------
output$Plt_srt <- renderPlot({
  x <- sr.data.0()
  u <- as.numeric(input$ui)
  mult <- mult()
  par(yaxs='i',bty='l')
  plot(R/u~Yr,data=x,type='l',ylim=c(0,with(x,max(R,S)/u)),
       main=input$caption,
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

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 2: Bayesian Model  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Bayesedata --- Create dataset for Bayesian modeling --------------------------
Bayesdata <- reactive({
  #  Import SR data 
  x <- sr.data()
  # nyrs is the number of years (i.e. number of rows) 
  nyrs <- dim(x)[1]
  R <- x$R
  S <- x$S
  # d is S multiplier
  d <- floor(log10(mean(S)))
  #  ar1: 1 if ar1 is included, 0 if not 
  if(input$ar1==TRUE){ar1 <- 1} else {ar1 <- 0}
  out <-list(nyrs=nyrs, S=S, R=R,d=d,ar1=ar1)
  return(out)
  })


#===============================================================================
#  Bayesmodel  Model section for JAG Models 
#===============================================================================
Bayesmodel <- reactive({
#---------------------------------------------------------------
#  Ricker 
#---------------------------------------------------------------
jag.model.CR <- function(){
    for(y in 1:nyrs){
      s[y] <- S[y]/(10^d)
      fit[y] = log(S[y]) + lnalpha - beta * s[y]
      e[y] = log(R[y]) - fit[y]
    }
# ar1 = 0 in standard analysis   
# ar1 = 1 when AR1 error moddel is considered. 
      mu[1] = fit[1] + ar1*phi * e0;	  
    for(y in 2:nyrs){	   
      mu[y] = fit[y] + ar1*phi*e[y-1]
    }
    #     Define Priors
    lnalpha ~ dunif(0,10)
    beta ~ dunif(0,10)
    sigma ~ dunif(0,10)
    phi ~ dunif(-1,1)
    e0 ~ dnorm(0,0.001) 
    Tau <- 1/(sigma*sigma)
# Likelihood 
    for(y in 1:nyrs){     
      R[y] ~ dlnorm(mu[y],Tau)
    }  
  }

# SR model function for poost processing ---------------------------------------
SR.CR <- function(lnalpha,beta,S,d){
  s <- S/(10^d)
  lnR <- log(S) + lnalpha - beta*s
  R <- exp(lnR)
  return(R)
  }

#-------------------------------------------------------------------------------
#  Beverton Holt  
#-------------------------------------------------------------------------------
jag.model.BH <- function(){
    for(y in 1:nyrs){
      s[y] <- S[y]/(10^d)
      fit[y] = lnalpha + log(S[y]) -log(1+beta*s[y])
      e[y] = log(R[y]) - fit[y]
    }
# ar1 = 0 in standard analysis   
# ar1 = 1 when AR1 error moddel is considered.   
      mu[1] = fit[1] + ar1*phi * e0	  
    for(y in 2:nyrs){	   
      mu[y] = fit[y] + ar1*phi*e[y-1]    
    }
    # Define Priors
    lnalpha ~ dunif(0,10)
    beta ~ dunif(0,10)
    phi ~ dunif(-1,1)
    e0 ~ dnorm(0,0.001)     
    sigma ~ dunif(0,10)
    Tau <- 1/(sigma*sigma)
    # Likelihood 
    for(y in 1:nyrs){     
      R[y] ~ dlnorm(mu[y],Tau)
    }  
  }
# SR model function for poost processing ---------------------------------------
SR.BH <- function(lnalpha,beta,S,d){
  s <- S/(10^d)
  lnR <- lnalpha +log(S) - log(1+beta*s)
  R <- exp(lnR)
  return(R)
  }

#-------------------------------------------------------------------------------
#  Deriso-Shunute  Inactive 
#-------------------------------------------------------------------------------
jag.model.DS <- function(){
    for(y in 1:nyrs){
      s[y] <- S[y]/(10^d)
      lnS[y] <- log(S[y])
      fit[y] = lnalpha + log(S[y]) - log(1 + beta*c*s[y])/c 
      e[y] = log(R[y]) - fit[y]
    }
# ar1 = 0 in standard analysis   
# ar1 = 1 when AR1 error moddel is considered.   
  mu[1] = fit[1] + ar1*phi * e0;	  
  for(y in 2:nyrs){	   
  mu[y] = fit[y] + ar1*phi*e[y-1]    
   } 
  #     Define Priors
    lnalpha ~ dunif(0,10)
    beta ~ dunif(0,10)
    sigma ~ dunif(0,10)
    c ~ dunif(0,1)
    phi ~ dunif(-1,1)
    e0 ~ dnorm(0,0.001)     
    Tau <- 1/(sigma*sigma)
    # Likelihood 
    for(y in 1:nyrs){     
      R[y] ~ dlnorm(mu[y],Tau)
    }  
  }
# SR model function for poost processing ---------------------------------------  
SR.DS <- function(lnalpha,beta,c,S,d){
  s <- S/(10^d)
  lnR <- log(S) + lnalpha - log(1 + beta*c*s)/c 
  R <- exp(lnR)
  return(R)
  }

#===============================================================================
#  Model Selection  
#===============================================================================
if(input$Model=='Ricker'){
    jagmodel <- jag.model.CR
    parameters <- c('lnalpha','beta','sigma') 
    if(input$ar1==TRUE){parameters <- c(parameters,'phi','e0')}
    model <- SR.CR
  }
if(input$Model=='Beverton-Holt'){
    jagmodel <- jag.model.BH
    parameters <- c('lnalpha','beta','sigma')
    if(input$ar1==TRUE){parameters <- c(parameters,'phi','e0')}
    model <- SR.BH
  } 
  if(input$Model=='Deriso-Shunute'){
    jagmodel <- jag.model.DS
    parameters <- c('lnalpha','beta','c','sigma')
    if(input$ar1==TRUE){parameters <- c(parameters,'phi','e0')}
    model <- SR.DS
    }
  out <- list(jagmodel=jagmodel,parameters=parameters,model=model)
  return(out)
  })

# test ---- Bayes Model code Outputs ------------------------------------------- 
output$test <- renderPrint({ Bayesmodel() })

#-------------------------------------------------------------------------------
#  run.JAGS: Run JAG Model 
#-------------------------------------------------------------------------------
run.JAGS <- eventReactive(input$RunBayes,{
#-------------------------------------------------------------------------------  
  progress <- Progress$new(min=1,max=100)
  on.exit(progress$close())
  progress$set(message = paste('JAG Model in progress'),
               detail = 'This will take a while. Be patient please....')
  for (i in 1:100) {
    progress$set(value = i)
  }
#-------------------------------------------------------------------------------   
#----  Import model data ------------------------------------------------------- 
  datnew <- Bayesdata()   
  niter <- input$n.iter      
  nburn <- input$n.burnin
  nthin <- input$n.thin
  nchain <- input$n.chain
#  JAGS model selection 
  jagmodel <- Bayesmodel()$jagmodel
  pars <- Bayesmodel()$parameters
# Run JAGS 
  output <- jags(data=datnew,parameters.to.save=pars, model.file= jagmodel,
            n.chains=nchain, n.iter=niter,n.burnin=nburn,n.thin=nthin,DIC=TRUE)
  return(output)
  })

#-------------------------------------------------------------------------------
#  Extract JAG results 
#-------------------------------------------------------------------------------
# sim ------ Straight JAG model output -----------------------------------------
sim <- reactive({
  x <- run.JAGS()
  return(x)
  })
# mcmc ----------- MCMC Data for SR analyses ------------------------------------
mcmc <- reactive({
  # Read mcmc data
  mcmc <- as.mcmc(sim())
  # post is mcmc data 
  post <- as.matrix(mcmc)  
  return(post)
  })

# post.summary ----------- MCMC Summary ---------------------------------------- 
post.summary <- reactive({
  sim_sum <- print(sim())
  post <- sim_sum$summary
  return(post)
  })
# BayesSum ------------ Output MCMC sumaary ------------------------------------
output$BayesSum <- renderPrint({
  sim_sum <- print(sim())
  print(sim_sum$summary)
  })

# Plt_trace -------- Trace and dencity plot -----------------------------------
output$Plt_trace <- renderPlot({
  pg <- input$p1
  mcmc <- as.mcmc(sim())
  par(mfrow=c(2,6))
  plot(mcmc,auto.layout=FALSE)
  })


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 3: SR Model Analyses   
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# SR.post --- Create SR parameters: alpha, beta, Seq, Smsy, Umsy, Smax ---------
SR.post <- reactive({
  D <- floor(mean(log10(sr.data()$S)))
# Read mcmc data
  post <- as.data.frame(as.matrix(mcmc()))
  post$alpha <- exp(post$lnalpha)
# Calculate Seq, Smsy, Umsy, Smax  
  if(input$Model=='Ricker')
    {
    post$Seq <- with(post,lnalpha/beta)*(10^D)
    post$Smsy <- with(post,Seq*(0.5-0.07*lnalpha))
    post$Umsy <- with(post,lnalpha*(0.5-0.07*lnalpha))
    post$Smax <- with(post,1/beta)*(10^D)
    }
  if(input$Model=='Beverton-Holt')
    {
    post$Seq <- with(post,(alpha-1)/beta)*(10^D)
    post$Smsy <- with(post,(sqrt(alpha)-1)/beta)*(10^D)
    post$Umsy <- with(post, 1-sqrt(1/alpha))
    post$Smax <- NA
    }
  # Remove obvious outlier data   
  post <- post[post$beta>0,]
  post <- post[post$Umsy>0,]
  post <- post[post$beta>=quantile(post$beta,0.001),] 
  post <- post[post$Seq<quantile(post$Seq,0.995),] 
  post <- post[post$alpha<quantile(post$alpha,0.995),] 
  return(post)
  })

# sumpost --- SR Parameter Summaries output------------------------------------- 
output$sumpost <- renderPrint({
  parname <-c('alpha','lnalpha','beta','Seq','Smsy','Umsy','Smax')
  summary(SR.post()[,parname],digits=c(3,3,3,0,0,0,0))
  })

# Plt_hist.mc --- SR Parameters Density plots ------------------------------------- 
output$Plt_hist.mc <- renderPlot({
  par(mfrow=c(2,4),mar = c(1.75,1.5,1.5,1.75),xaxs='i',yaxs='i',bty='l')
  D <- floor(mean(log10(sr.data()$S)))
  plot(density(SR.post()$alpha),main='alpha',xlab='',ylab='')
  plot(density(SR.post()$beta),main=paste0('beta',' x 10^(',-D,')'),xlab='',ylab='')
  if(input$ar1==TRUE){
    plot(density(SR.post()$phi),main='Phi',xlab='',ylab='')
      }
  plot(density(SR.post()$Seq), main='SEQ',xlab='',ylab='')
  plot(density(SR.post()$Smsy),main='Smsy',xlab='',ylab='')
  plot(density(SR.post()$Umsy), main='Umsy',xlab='',ylab='')
# Smax esists only Ricker SR model.   
  if(input$Model=='Ricker'){
  plot(density(SR.post()$Smax), main='Smax',xlab='',ylab='')
  }
  })

#===============================================================================
#  Create SR Model Predictions    
#===============================================================================
# SR.resid ----- Model Residuals -----------------------------------------------
SR.resid <-reactive({
  D <- floor(mean(log10(sr.data()$S)))
  srmodel <- Bayesmodel()$model
  #---------- Extract MCMC SR Model Parameters ---------------------------------
  lnalpha <-SR.post()$lnalpha
  beta <- SR.post()$beta
  sigma <- SR.post()$sigma
  S <- sr.data()$S
  ncol <- length(S) # 
  nrow <- length(SR.post()$Seq)  #  Extract number of MCMC sample 
  # Create Residuals  MCMC matrix    
  RD <- matrix(NA,nrow=nrow,ncol=ncol) 
  for(i in 1:nrow){
    # Calculated expected Returns form each MCMC SR model paramters   
    Ey <- srmodel(lnalpha[i],beta[i],S,D)
    RD[i,] <- log(Ey) - log(sr.data()$R)
    }  
  # Create residuals -----------------------------------------------------------  
  out <- RD
  return(out)
  })  

# SR.pred ---- Bayesian Model Prediction ---------------------------------------
SR.pred <-reactive({
  D <- floor(mean(log10(sr.data()$S)))
  srmodel <- Bayesmodel()$model
  
  #---------- Extract MCMC SR Model Parameters ---------------------------------
  lnalpha <-SR.post()$lnalpha
  beta <- SR.post()$beta
  sigma <- SR.post()$sigma
  
  #---------- Determine model S length -----------------------------------------
  Seq <- quantile(SR.post()$Seq,0.9)   # Extract 90 percentile Seq
  max.s <- max(Seq,max(sr.data.0()$S)) # Extract max spanwer 
  # This makes largest numbers into integer (e.g. 100000)
  maxb <- ceiling(max.s/(10^D))*(10^D)
  # Cut into 201 segments (can be increased) 
  S <- seq(0,maxb, length.out=201) 
  nrow <- length(SR.post()$Seq)  #  Extract number of MCMC sample 
  # Create Expected mean and observed Recruit MCMC matrix    
  mc.R <- matrix(NA,nrow=nrow,ncol=201)   # Model expected recruit -------------
  mc.R.p <- matrix(NA,nrow=nrow,ncol=201) # Model expected observed recruit-----

  #---------- Calculate expecte rueturns from each MCMC ------------------------ 
  for(i in 1:nrow){
  # Calculated expected Returns form each MCMC SR model paramters   
    Ey <- srmodel(lnalpha[i],beta[i],S,D)
    mc.R[i,] <- Ey
  # mc.R.p adds obervation error (sigma)  
  mc.R.p[i,] <- exp(rnorm(201,log(Ey),sigma[i]))
    }  
  # Create expecte mean and observed Yield matric
  mc.Y <-  t(t(mc.R)-S) 
  mc.Y.p <-  t(t(mc.R.p)-S) 

  #------  Create Output list file ---------------------------------------------  
  out <- list()
  out$S <- S
  out$mc.R <- mc.R
  out$mc.R.p <- mc.R.p
  out$mc.Y <- mc.Y
  out$mc.Y.p <- mc.Y.p
  return(out)
  })  

# SRp ------ Model predicted mean, CI, PI  SR range ----------------------------
SRp <- reactive({
  #--- Import predicted data ---------------------------------------------------
  Pred <-SR.pred()
  #--- User defined % interval range -------------------------------------------  
  pci <- (100-input$CI)/200
  #--- user defined minimum recruitment, yield goal ----------------------------
  rg <- input$r1
  yg <- input$y1
  S <- SR.pred()$S
  # Median RS 
  RS.md <- apply(Pred$mc.R,2,median)
  RS.me <- apply(Pred$mc.R,2,mean)
  # Calculate CI-PI
  Rl <- apply(Pred$mc.R,2,function(x) quantile(x, pci))
  Ru <-  apply(Pred$mc.R,2,function(x) quantile(x, 1-pci))
  Rl.p <- apply(Pred$mc.R.p,2,function(x) quantile(x, pci))
  Ru.p <- apply(Pred$mc.R.p,2,function(x) quantile(x, 1-pci))
  out <- data.frame(cbind(S,RS.md,RS.me,Rl,Ru,Rl.p,Ru.p))
  names(out) <- c('S','RS.md','RS.me','Rl','Ru','Rl.p','Ru.p')
  return(out)
  })  

# downloadData ---- Results download -------------------------------------------
output$downloadData <- downloadHandler(
  filename = function() {
    "downloaddata.xlsx"
  },
  content = function(file) {
    foo <-pred.yd()
    foo$sum <- pred.y()
    write.xlsx(foo, file)
  }
)

#===============================================================================
#  Plots and Tables Outputs  
#=============================================================================== 
# base.p ------- Base SR plot  -------------------------------------------------
base.p <- reactive({
  #  dev.control("enable")
  u <- as.numeric(input$ui)
  mult <- mult()
  x <- sr.data()
  xp <- x/u
  SRp <- SRp()/u
  par(xaxs='i',yaxs='i',bty='l')
  plot(R~S,data=xp,pch=19,col=1, 
       main= input$caption,
       xlab=paste("Escapement",mult),ylab=paste('Recruit',mult),
       xlim=c(0,max(SRp$S)),ylim=c(0,1.1*max(xp$R)))
  # Plot 1:1 line 
  abline(0,1)
  # Add Predicted   
  lines(RS.md~S,data=SRp,col=1,lw=2,lty=2)
  lines(RS.me~S,data=SRp,col=1,lw=2)
  out <-recordPlot()
  #  dev.off()
  return(out)
  })

# base.py --- Base Yield plot -------------------------------------------------- 
base.py <- reactive({
  #  dev.control("enable")
  u <- as.numeric(input$ui)
  mult <- mult()
  x <- sr.data()
  SRp <- SRp()/u
  xp <- x/u
  # Plot Basic Yield plot  
  par(xaxs='i',yaxs='i',bty='l')
  plot((R-S)~S,data=xp,pch=19,col=1, 
       main= input$caption,
       xlab=paste("Escapement",mult),ylab=paste('Yield',mult),
       xlim=c(0,max(SRp$S)),ylim=c(min(SRp$Rl-SRp$S),1.1*max(xp$R-xp$S)))
  lines((RS.md-S)~S,data=SRp,col=1,lw=2,lty=2)
  lines((RS.me-S)~S,data=SRp,col=1,lw=2)
  abline(h=0)
  out <-recordPlot()
  #  dev.off()
  return(out)
  })


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 4: SR Model Analyses plots   
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# srplot ------- SR plot function ----------------------------------------------
srplot <- function(){
  u <- as.numeric(input$ui)
  x <- sr.data()
  xp <- x/u
  SRp <- SRp()/u
  # Draw Base SR Plot
  replayPlot(base.p())
  
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
    abline(v=median(SR.post()$Smsy)/u,col=1,lty=2)
    t1 <- 'Smsy'
    l1 <- 2
    }
  # Add Smax       
  t2 <- ''
  l2 <- 0
  if(input$show.smax==TRUE) {
    abline(v=median(SR.post()$Smax)/u,col=1,lty=3)
    t2 <- 'Smax'
    l2 <- 3
    }
  legend('topright',c(t1,t2),lty=c(l1,l2),bty='n')    
  }

# Plt_SR ------ SR plot -------------------------------------------------------
output$Plt_SR <- renderPlot({srplot()})

# down --------- SR plot download ----------------------------------------------
output$down <- downloadHandler(
  filename = function() {
    paste("myreport","png", sep = ".")
    #    paste("myreport", input$report, sep = ".")
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


# Plt_yield -------- Yield plot ----------------------------------------------- 
output$Plt_yield <- renderPlot({
  u <- as.numeric(input$ui)
  x <- sr.data()
  SRp <- SRp()/u
  xp <- x/u
  # Plot base Yiled plot
  replayPlot(base.py())
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
    polygon(c(SRp$S,rev(SRp$S)),c(upr-SRp$S,rev(lwr-SRp$S)),col=tcol('grey',50),border=NA)
  }
  # Add Years
  if(input$show.points==TRUE) {
    pointLabel(xp$S,(xp$R-xp$S), labels=as.character(x$Yr), cex= 1,col=4)
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
    abline(v=median(SR.post()$Smsy)/u,col=1,lty=2)
    t1 <- 'Smsy'
    l1 <- 2
  }
  # Add Smax       
  t2 <- ''
  l2 <- 0
  if(input$show.smax==TRUE) {
    abline(v=median(SR.post()$Smax)/u,col=1,lty=3)
    t2 <- 'Smax'
    l2 <- 3
  }
  # Add legend  
  legend('topright',c(t1,t2),lty=c(l1,l2),box.lty=0)    
  })

# Plt_residual --- Plot Residual Plot -------------------------------------------------
output$Plt_residual <- renderPlot({
  year <- sr.data()$Yr
  resid <-SR.resid()
  resid.m <- apply(resid,2,mean)
  par(bty='l')
  plot(resid.m~year,xlab='Year',ylab='Residuals')
  abline(h=0)
  #  model <- gam(resid.m~s(year),family=gaussian, fit =TRUE)
  #  pred.year <- data.frame(year, predict.gam(model,se = TRUE))    
  #  lines(pred.year$year, pred.year$fit,lwd=2,col=4)
  ciu <- apply(resid,2,function(x) quantile(x, 0.025))
  cil <- apply(resid,2,function(x) quantile(x, 0.975))
  polygon(c(year,rev(year)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA)
  })

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 5: Escapement Goal Analyses 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#====== functions list =========================================================
# profile --- Profile Calculation Function  ------------------------------------    
profile <- function(S, M.rep, mp,tp) {
  #	S: Spawner 
  #	M.rep: Yield, Run, simulaitn matrix
  # M.rep: nrow:  number of simulation replicated 
  # M.rep: ncol:  number of S  nocl = length(S)
  # mp: Minimum Yield, Run percentage (0-1)
  # tp: Target percentage  (0-1)
  # Determine the dimpention of temporal matrix  
  nrows <- dim(M.rep)[1]
  ncols <- dim(M.rep)[2]
  # Create an empty matrix  
  temp <-  matrix(0,nrow = nrows,ncol=ncols) 
  # For each simulation, assign 1 if valu of M.rep is > miimum % M.rep
  # Assign 0  0 if not
  for(j in 1:nrows){
    temp[j,] <- ifelse(M.rep[j,] > mp*max(M.rep[j,]),1,0)
  }   
  # Mean of temp matrix is a  profille probability  
  M.Rep.prof <- colMeans(temp)
  # Find range of S that intersect with target probabilty  
  S.prof <- S[M.Rep.prof > tp]
  # Exract min and max S: Profile determined S range
  S.range <- c(NA,NA)
  if(sum(S.prof) > 0){S.range <- c(min(S.prof),max(S.prof))}
  # Output  
  out <- list(S = S, M.prof = M.Rep.prof, S.range = S.range)
  return(out)
}

# srplot.g : plot goal range in SR plot-----------------------------------------
srplot.g <- function(Srange){
  u <- as.numeric(input$ui)
  S.range <- Srange/u
  # Plot base recruit Plot  
  replayPlot(base.p())
  SRp <- SRp()/u
  with(SRp,polygon(c(S,rev(S)),c(Ru,rev(Rl)),col=tcol('grey',50),border=NA))
  with(SRp,lines(S,Ru.p,lty=2,col='grey'))
  with(SRp,lines(S,Rl.p,lty=2,col='grey'))
  abline(v=S.range,lty=1,col=3)
}

# syplot.g : plot goal range in Yield plot--------------------------------------
syplot.g <- function(Srange){
  u <- as.numeric(input$ui)
  S.range <- Srange/u
  # Plot base Yield Plot
  replayPlot(base.py())
  SRp <- SRp()/u
  with(SRp,polygon(c(S,rev(S)),c(Ru-S,rev(Rl-S)),col=tcol('grey',50),border=NA))
  with(SRp,lines(S,Ru.p-S,lty=2,col='grey'))
  with(SRp,lines(S,Rl.p-S,lty=2,col='grey'))
  # Plot escapement goal range 
  abline(v=S.range,lty=1,col=3)
}

#===============================================================================
#  Smsy Goal Analyses 
#===============================================================================
# EG.Smsy -------  User defined Smsy profile based goal data -------------------
EG.Smsy <- reactive({
  # Import MCMC Expected Mean Yield 
  mc.Y <- SR.pred()$mc.Y 
  # Import S 
  S <- SR.pred()$S
  # Import minimum Smsy %
  p.msy <- input$p.msy/100
  # Import minimum % achieving Smsy  
  p.msy.t <- input$p.msy.t/100
  # Generate profle using profile function 
  S.prof  <- profile(S, mc.Y, p.msy, p.msy.t)
  # Rename Output
  names(S.prof) <- c('S','S.prof','S.Range')
  return(S.prof)  
  })

# EG.Smsy.st  ------- Standard Smsy profile based goal data --------------------
EG.Smsy.st <- reactive({
  # Import MCMC Expected Mean Yield
  mc.Y <- SR.pred()$mc.Y
  # Import S 
  S <- SR.pred()$S
  # Standard min Smsy probability 
  st <- c(0.9,0.8,0.7)
  # Create a dummy matrics 
  ncols <- length(S)
  # Create Standard probablity matrix 
  prof.st <- matrix(0,nrow=3,ncol=ncols)
  # Create BEG 
  Srange.st <- matrix(NA,nrow=3,ncol=2)
  for(i in 1:3){
    profile <- profile(S,mc.Y, st[i],0.9)
    prof.st[i,] <- profile$M.prof
    Srange.st[i,] <- profile$S.range
   }
  # Find Smsy Profile Intersections 
  out <- list(Smsy.prof.st = prof.st, BEG.st = Srange.st)
  return(out)   
  }) 


output$citable <- renderTable(
  EG.Smsy()$S
)

# Plt_Smsy.prof ----- Smsy Optimum Profile Plot ------------------------------------
output$Plt_Smsy.prof <- renderPlot({
  # Import minimum Smsy %
  p.msy <- input$p.msy/100
  # Import minimum % achieving Smsy  
  p.msy.t <- input$p.msy.t/100  
  u <- as.numeric(input$ui)
  mult <- mult()
# Import S,   
  S <- EG.Smsy()$S/u
  Y.prof.st <- EG.Smsy.st()$Smsy.prof.st
  Y.prof <- EG.Smsy()$S.prof
  #---------------------------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l')
  #  Standard proflie plots 
  plot(S,Y.prof.st[1,],type='l',col=1, ylim=c(0,1),ylab = 'Probability',
       xlab=paste('Escapement',mult),main=paste0('MSY Yield probability curve')) 
  lines(S,Y.prof.st[2,],lty = 2,col=1)
  lines(S,Y.prof.st[3,],lty = 4,col=1)
  abline(h = 0.9,lwd=1,col=1)
  #  Userdefline profile plot 
  lines(S,Y.prof,lty = 1,lwd=2,col=4)
  abline(h = p.msy.t,lwd=2,col=2)
  tex <- c('90% MSY','80% MSY','70% MSY',paste0(input$p.msy,'% MSY'))
  legend('topright',tex,lty=c(1,2,4,1),
         col=c(1,1,1,4),box.lty=0)  
  })  

# SA.BEG  ------- Smsy goal table ----------------------------------------------
SA.BEG <- reactive({
  u <- as.numeric(input$ui)
  BEG.st <- EG.Smsy.st()$BEG.st/u
  BEG <- EG.Smsy()$S.Range/u
  t.BEG.st1 <- paste('90% MSY achieving 90% Probability',BEG.st[1,1],'-',BEG.st[1,2])
  t.BEG.st2 <- paste('80% MSY achieving 90% Probability',BEG.st[2,1],'-',BEG.st[2,2])
  t.BEG.st3 <- paste('70% MSY achieving 90% Probability',BEG.st[3,1],'-',BEG.st[3,2])
  t.BEG.1 <- paste(paste0(input$p.msy,'% MSY achieving ',input$p.msy.t,'% Probability'),BEG[1],'-',BEG[2])
  out <- list(tst1=t.BEG.st1,tst2=t.BEG.st2,tst3=t.BEG.st3,t1=t.BEG.1)
  return(out)
  })

# Txt_Srange.smsy -------- Smsy goal output ---------------------------------------------
output$Txt_Srange.smsy <-renderText({
  paste(SA.BEG()$tst1,SA.BEG()$tst2,SA.BEG()$tst3,SA.BEG()$t1,sep='\n')
  })


# bsmsy.y -------- Yield output with Smsy goal range ---------------------------
output$bsmsy.y <- renderPlot({
  u <- as.numeric(input$ui)
  BEG <- EG.Smsy()$S.Range/u
  # Plot base Yield Plot
  replayPlot(base.py())
  SRp <- SRp()/u
  with(SRp,polygon(c(S,rev(S)),c(Ru-S,rev(Rl-S)),col=tcol('grey',50),border=NA))
  with(SRp,lines(S,Ru.p-S,lty=2,col='grey'))
  with(SRp,lines(S,Rl.p-S,lty=2,col='grey'))
  # Plot escapement goal range 
  abline(v=BEG,lty=1,col=3)
  })

# bsmsy.r -------- Recruit output with Smsy goal range -------------------------

output$bsmsy.r <- renderPlot({srplot.g(EG.Smsy()$S.Range)})

output$bsmsy.y <- renderPlot({syplot.g(EG.Smsy()$S.Range)})

#===============================================================================
#  Smax Goal Analyses 
#===============================================================================
# EG.Smax -------  User defined Smax profile based goal data -------------------
EG.Smax <- reactive({
# Import MCMC Expected Recruit 
  mc.R <- SR.pred()$mc.R
# Import S 
  S <- SR.pred()$S
# Import minimum Smax %
  p.max <- input$p.max/100
# Import minimum % achieving Smsy  
  p.max.t <- input$p.max.t/100
# Generate profle using profile function 
  Smax.prof  <- profile(S, mc.R, p.max, p.max.t)
# Rename Output
  names(Smax.prof) <- c('S','Smax.prof','BEG.Smax')
  return(Smax.prof)  
  })

# EG.Smax.st -------  Standard Smax profile based goal data -------------------
EG.Smax.st <- reactive({
  # Import MCMC Expected Recruit
  mc.R <- SR.pred()$mc.R
  # Import S 
  S <- SR.pred()$S
  # Standard min Smsy probability 
  st <- c(0.9,0.8,0.7)
  # Create a dummy matrics 
  ncols <- dim(mc.R)[2]
  # Create Standard probablity matrix 
  Smax.prof.st <- matrix(0,nrow = 3,ncol=ncols)
  # Create BEG 
  BEG.st <- matrix(NA,nrow=3,ncol=2)
  for(i in 1:3){
    profile <- profile(S, mc.R, st[i],0.9)
    Smax.prof.st[i,] <- profile$M.prof
    BEG.st[i,] <- profile$S.range
  }
  # Find Smsy Profile Intersections 
  out <- list(Smax.prof.st = Smax.prof.st, BEG.st = BEG.st)
  return(out)   
  }) 

# Plt_Smax.prof ----- Smax Optimum Profile Plot ------------------------------------
output$Plt_Smax.prof <- renderPlot({
  # Import minimum Smsy %
  p.max <- input$p.max/100
  # Import minimum % achieving Smsy  
  p.max.t <- input$p.max.t/100  
  u <- as.numeric(input$ui)
  mult <- mult()
  S <- EG.Smax()$S/u
  R.prof.st <- EG.Smax.st()$Smax.prof.st
  R.prof <- EG.Smax()$Smax.prof
  #-----------------------------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l')
  plot(S,R.prof.st[1,],type='l', col=1, ylim=c(0,1),ylab = 'Probability',
       xlab=paste('Escapement',mult),main=paste0('Smax probability curve')) 
  lines(S,R.prof.st[2,],lty = 2,col=1)
  lines(S,R.prof.st[3,],lty = 4,col=1)
  abline(h = 0.9,lwd=2,col=1)
  lines(S,R.prof,lty = 1,col=4)  
  abline(h = p.max.t,lwd=2,col=2)
  tex <- c('90% Rmax','80% Rmax','70% Rmax',paste0(input$p.max,'% Rmax'))
  legend('topright',tex,lty=c(1,2,4,1),col=c(1,1,1,4),box.lty=0)  
  })  

# SM.BEG -------- Smax Goal range output table ---------------------------------
SM.BEG <- reactive({
  u <- as.numeric(input$ui)
  BEG.st <- u*round(EG.Smax.st()$BEG.st/u)
  BEG <- u*round(EG.Smax()$BEG/u)
  t.BEG.st1 <- paste('90% Rmax achieving 90% Probability',BEG.st[1,1],'-',BEG.st[1,2])
  t.BEG.st2 <- paste('80% Rmax achieving 90% Probability',BEG.st[2,1],'-',BEG.st[2,2])
  t.BEG.st3 <- paste('70% Rmax achieving 90% Probability',BEG.st[3,1],'-',BEG.st[3,2])
  t.BEG.1 <- paste(paste0(input$p.max,'% Rmax achieving ',input$p.max.t,'% Probability'),BEG[1],'-',BEG[2])
  out <- list(tst1=t.BEG.st1,tst2=t.BEG.st2,tst3=t.BEG.st3,t1=t.BEG.1)
  return(out)
})

# Txt_Srange.smax -------- Smax Goal range output table ---------------------------------
output$Txt_Srange.smax <-renderText({
  paste(SM.BEG()$tst1,SM.BEG()$tst2,SM.BEG()$tst3,SM.BEG()$t1,sep='\n')
 })

# Plt_rec.smax -------- Recruit output with Smax goal range -------------------------
output$Plt_rec.smax <- renderPlot({srplot.g(EG.Smax()$BEG.Smax)})

# Plt_yield.smax -------- Recruit output with Smax goal range -------------------------
output$Plt_yield.smax <- renderPlot({syplot.g(EG.Smax()$BEG.Smax)})


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  4.0 Target Yield based Escapement Goal
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
b.YAg <- reactive({
# Import User defined Yiedle Gaol 
  yg <- input$y1
  mp <- (1-input$CI/100)/2
# Import MCMC Expected mean Yields 
  mc.Y <- SR.pred()$mc.Y
# Import MCMC Expected annual Yield 
  mc.Y.p <- SR.pred()$mc.Y.p  
# Import S 
  S <- SR.pred()$S
# For each bootstrap, determin if expected yields exceed desired goal  
  mc.Yb <- apply(mc.Y,2,function(x) ifelse(x >yg,1,0))
  mc.Ybp <- apply(mc.Y.p,2,function(x) ifelse(x >yg,1,0))   
# calculate mean: This is the same as probability   
  mc.Ypm <- colMeans(mc.Yb)
  mc.Ypa <- colMeans(mc.Ybp)
  # Import Mean, Upper, Lower Yield range    
  Ym <- apply(mc.Y,2,mean)
  Yu <- apply(mc.Y,2,function(x) quantile(x,1-mp))
  Yl <- apply(mc.Y,2,function(x) quantile(x,1-mp))  
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
  out <- list(mc.Yp=mc.Ypm, mc.Ypa=mc.Ypa, BEG.l = BEG.l, BEG.m = BEG.m, BEG.u=BEG.u)
  return(out)
  })

# Optimum Mean and annual Yield Proflie Plot 
output$Plt_yield.prof <- renderPlot({
  u <- as.numeric(input$ui)
  mult <- mult()
  yg <- input$y1
  ypg <- input$y1p/100
  # Bootstrap probile 
  S <- SR.pred()$S/u
  mc.Yp <- b.YAg()$mc.Yp
  mc.Ypa <- b.YAg()$mc.Ypa
  # Create a plot 
  par(xaxs='i',yaxs='i',bty='l')
  plot(S,mc.Yp,type='l',ylim=c(0,1),ylab = 'Probability',xlab=paste("Escapement",mult),
       main=paste('Minimum',yg,'Yield probability plot')) 
  lines(S,mc.Ypa,lty=2)
  abline(h = ypg,lwd=2,col=2)
  }) 

# Find Yield Target Intersection 
b.YApg <- reactive({
  ypg <- input$y1p/100
  S <- SR.pred()$S 
  Yp <- b.YAg()$mc.Yp
  # Find Intersections 
  b.p <- S[Yp > ypg]
  BEG.p <- c(NA,NA)
  if(sum(b.p) > 0){BEG.p <- c(min(b.p),max(b.p))}
  out <- BEG.p
  return(out)
  })

# Print Optimum Yield Proflie Goal Range  
output$bypt <-renderText({
  u <- as.numeric(input$ui)
  BEG.p <- u*round(b.YApg()/u)
  paste('Escapement Goal Range:',BEG.p[1],'-',BEG.p[2])
  })


# Yield Plot 
output$Plt_yield.gl <- renderPlot({
  u <- as.numeric(input$ui)
  yg <- input$y1/u
  replayPlot(base.py())
  SRp <- SRp()/u
  with(SRp,polygon(c(S,rev(S)),c(Ru-S,rev(Rl-S)),col=tcol('grey',50),border=NA))
  with(SRp,lines(S,Ru.p-S,lty=2,col='grey'))
  with(SRp,lines(S,Rl.p-S,lty=2,col='grey'))
  abline(h=yg,lwd=2,col=2)
  }) 

# Print Escapement Goal Range
output$by<- renderText({
  mp <- input$CI
  u <- as.numeric(input$ui)
  BEG.l <- u*round(b.YAg()$BEG.l/u)
  BEG.m <- u*round(b.YAg()$BEG.m/u)
  BEG.u <- u*round(b.YAg()$BEG.u/u)
  t.BEG.l <- paste('Lower',mp,'% Limit',BEG.l[1],'-',BEG.l[2])
  t.BEG.m <- paste('Mean              ',BEG.m[1],'-',BEG.m[2])
  t.BEG.u <- paste('Upper',mp,'% Limit',BEG.u[1],'-',BEG.u[2])
  paste(t.BEG.l,t.BEG.m,t.BEG.u,sep='\n')
  })

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  4.0 Target Recruitment  based Escapement Goal
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++  
b.RAg <- reactive({
  mp <- (1-input$CI/100)/2
  # Import Targe Recruitment goal   
  rg <- input$r1
  # Import bootstrap S and Recruit 
  S <- SR.pred()$S 
  R <- SR.pred()$mc.R
  R.p <- SR.pred()$mc.R.p
  # For each bootstrap, determin if expected Recruitment exceed desired goal  
  Rp <- apply(R,2,function(x) ifelse(x >rg,1,0))
  # Recruitment target probabilty profil   
  Rp <- colMeans(Rp)
  Rpa <- apply(R.p,2,function(x) ifelse(x >rg,1,0))
  # Recruitment target probabilty profil   
  Rpa <- colMeans(Rpa)   
  Rm <- apply(R,2,mean) 
  Ru <- apply(R,2,function(x) quantile(x,1-mp)) 
  Rl <- apply(R,2,function(x) quantile(x,mp)) 
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
  S <- SR.pred()$S 
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
  replayPlot(base.p())
  SRp <- SRp()/u
  with(SRp,polygon(c(S,rev(S)),c(Ru,rev(Rl)),col=tcol('grey',50),border=NA))
  with(SRp,lines(S,Ru.p,lty=2,col='grey'))
  with(SRp,lines(S,Rl.p,lty=2,col='grey'))  
  abline(h=rg,lwd=2,col=2)
  }) 

# Print Base Recruit Goal  
output$brt <-renderText({
  u <- as.numeric(input$ui)
  mp <- input$CI
  BEG.l <- u*round(b.RAg()$BEG.l/u)
  BEG.m <- u*round(b.RAg()$BEG.m/u)
  BEG.u <- u*round(b.RAg()$BEG.u/u)
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
  mult <- mult()
  S <- SR.pred()$S/u
  Rp <- b.RAg()$Rp
  Rpa <- b.RAg()$Rpa  
  par(xaxs='i',yaxs='i',bty='l')
  plot(S,Rp,type='l',ylim=c(0,1),ylab = 'Probability',xlab=paste("Escapement",mult),
       main=paste('Minimum',rg,'Recruit probability Plot')) 
  lines(S,Rpa,lty=2)
  abline(h = rpg,lwd=2,col=2)
  })  


# Optimum Recruit Proflie Escapement Goal 
output$brpt <-renderText({
  u <- as.numeric(input$ui)
  BEG.p <- u*round(b.RApg()/u)
  paste('Escapement Goal Range:',BEG.p[1],'-',BEG.p[2])
  })

#=======================================================================
#=======================================================================
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  1.0 MCMC  estimation of Mean and Annual Yield and Recruit    
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
b.GA <- eventReactive(input$Run,{
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
  D <- floor(mean(log10(sr.data()$S)))
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


#-----------------------------------------------------------------------
#  Plot distribution of Mean and Annual  Yield at Given Escapement Range
#-----------------------------------------------------------------------
output$EGR.Yield <- renderPlot({
  par(mfrow=c(2,1),xaxs='i',yaxs='i',bty='l')
  u <- as.numeric(input$ui)
  mult <- mult()
  yg <- input$yg/u
  # Boootstrap estimate  
  plot(density(b.GA()$Y/u),main='Expected Mean Yields',xlab=paste("Yield",mult),ylab='')
  # Parameteric estimate   
  abline(v=yg,lty=2,col=2)
  # Plot Annual Yield  
  Y.p <-b.GA()$Y.p
  Y.p <- Y.p[Y.p < quantile(Y.p,0.995)]
  plot(density(Y.p/u), main='Expected Annual Yields',xlab=paste("Yield",mult),ylab='')
  abline(v=yg,lty=2,col=2)
  }) 

#-----------------------------------------------------------------------
#  Plot distribution of Recruit and Yield at Given Escapement Range
#  SR model Parameteric based CI and PI
#-----------------------------------------------------------------------
output$EGR.Rec <- renderPlot({
  par(mfrow=c(2,1),xaxs='i',yaxs='i',bty='l', cex=1.2)
  u <- as.numeric(input$ui)
  mult <- mult()
  rg <- input$rg/u
  # Mean estimate   
  # Bootstrap estimate  
  plot(density(b.GA()$R/u),main='Expected Mean Recruit',xlab=paste("Recruit",mult),ylab='')
  abline(v=rg,lty=2,col=2)
  # Annualestimate   
  R.p <-b.GA()$R.p
  R.p <- R.p[R.p < quantile(R.p,0.995)]
  # Bootstrap estimate
  plot(density(R.p/u), main='Expected Annual Recruit',xlab=paste("Recruit",mult),ylab='')
  abline(v=rg,lty=2,col=2)
  }) 


output$bGASRs <- renderPrint({
    R <- as.vector(b.GA()$R)    
    R.p <- as.vector(b.GA()$R.p)
    dat <- data.frame(R,R.p)
    names(dat) <- c('Mean Recruit','Annual Recruit')
    print(summary(dat),digits=0)
  })
  

output$bGAs <- renderPrint({
  Y <- as.vector(b.GA()$Y)
  Y.p <- as.vector(b.GA()$Y.p)
  dat <- data.frame(Y,Y.p)
  names(dat) <- c('Mean Yields','Annual Yields')
  print(summary(dat),digits=0)
  })

# Calculate Probability meeting target  
output$bGASRt <- renderText({
  rg <- input$rg
  prg <- sum(ifelse(b.GA()$R>rg,1,0))/length(b.GA()$R)
  pyg <- sum(ifelse(b.GA()$R.p>rg,1,0))/length(b.GA()$R.p)
  t.prg <- paste('Meeting Mean Recruit Target:',round(100*prg,0),'%')
  t.pyg <- paste('Meeting Annual Recruit Target:',round(100*pyg,0),'%')
  paste(t.prg,t.pyg,sep='\n')
  })

# Calculate Probability meeting target  
output$bGAt <- renderText({
  yg <- input$yg
  prg <- sum(ifelse(b.GA()$Y>yg,1,0))/length(b.GA()$Y)
  pyg <- sum(ifelse(b.GA()$Y.p>yg,1,0))/length(b.GA()$Y.p)
  t.prg <- paste('Meeting Mean Yield Target:',round(100*prg,0),'%')
  t.pyg <- paste('Meeting Annual Yiled Target:',round(100*pyg,0),'%')
  paste(t.prg,t.pyg,sep='\n')
  })

 



})# End of Server 




# Create Shiny app ----
shinyApp(ui, server)
