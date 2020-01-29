#initialize
library(shiny)
library(shinythemes)
library(datasets)
library(coda)
library(R2jags)
library(openxlsx)
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
  tabPanel("Data Input",
   sidebarPanel(width = 3,
  selectInput(inputId="dataType","Data Type", choices = c('S-R','Run')),
#---------------------------------------------------    
#  File Inuput
#---------------------------------------------------
# Input: Select a file ----
  fileInput("file1", "Choose CSV File",
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
uiOutput('fyear'),
uiOutput('lyear'),

   ), # End SidePanel
  # output
  mainPanel(
    tabsetPanel(
      tabPanel("Table",    
               conditionalPanel(
                 condition = "input.dataType == 'S-R'",
                 h4("S-R Data file column orders: Year, Spawner (Escapement), Recruit")
               ),
               conditionalPanel(
                 condition = "input.dataType == 'Run'",
                 h4("Run Data file column orders: Year, Escapement, Run,
                   Run by age (or proportion) from youngest to oldest")
                 ),
               dataTableOutput("table")),
      tabPanel("Brood Table",
               conditionalPanel(
                 condition = "input.dataType == 'Run'",
                 dataTableOutput("btable"))
               # Horizontal line ----
                  ),
      tabPanel("Summary",
             verbatimTextOutput('summary'),
               plotOutput('hist')),
      tabPanel("To be included",
           
          )  # End tabPanel
        )  # End tabsetPanel
      )  # End mainPanel
    ), # End tabpanle
#-----------------------------------------------------------------------
#  Panel 2
#-----------------------------------------------------------------------
navbarMenu("SR Model Analyses",
  tabPanel("Run Bayesian Model",
  sidebarPanel(width = 3,
  p("Bayesian Model Setting"),
  numericInput(inputId='n.iter','Simulation Length',value=10000,min=0,step=10000), 
  numericInput(inputId='n.burnin','Burn-in Length',value=1000,min=0,step = 1000),
  numericInput(inputId='n.thin','Thinning',value=10,min=0,step = 1),
  numericInput(inputId='n.chain','Number of Chains',value=1,min=1,step = 1),
  selectInput('Model',"Select SR Model",choice=list('Ricker','Beverton-Holt'),selected ='Ricker'),
  checkboxInput(inputId="ar1", "Add AR(1) Error", FALSE),
  p("Start Bayesian Analyses"),
  actionButton("RunBayes","Run")                                 
          ),  # End sidebarPanel
           
     mainPanel(
       tabsetPanel(
             tabPanel("Bayesian Model",
                      verbatimTextOutput('test'),
                      ),
             tabPanel("Model summary",
                      p(strong("Model summary")), 
                      verbatimTextOutput('BayesSum')  
                      ),
             tabPanel("Trace Plots",
                      plotOutput("plot.trace")
                      ),
             tabPanel("Model Parameter", 
                      tableOutput("partable")
                      ),
             tabPanel("Trace Plots",
  
                      ),
               id = "conditionedPanels"
               )#End tabsetPanel
             )#End mainPanel
           ),#End tabPanel
  
  tabPanel("SR Model",
       sidebarPanel(width = 3,
       selectInput(inputId="ui","Axis Dislpay Unit", choices = c(1,1000,1000000)),  
       p("Escapement Goal Range"),
       numericInput(inputId='egl','Lower Goal',value=0,min=0), 
       numericInput(inputId='egu','Upper Goal',value=0,min=0),    
       checkboxInput(inputId="show.eg", "Show Escapement Goal", FALSE),
       checkboxInput(inputId="show.points", "show Years", TRUE), 
       checkboxInput(inputId="show.smsy", "show Smsy", TRUE),
       checkboxInput(inputId="show.smax", "show Smax", TRUE),
       checkboxInput(inputId="show.int", "show Interval", TRUE),
       numericInput("CI", "% Credible Interval", value=90,min=0,max=100,step=5),
       selectInput(inputId="Li","Interval Type", choices = c('confidence','prediction'))
         ),  # End sidebarPanel
    mainPanel(tabsetPanel(
           #------------------ SR Plot----------------------------------------------        
           tabPanel("SR Plot",
                    plotOutput(height='500px','plot.SR'),
                    downloadButton("down", label = 'Download the plot')
           ),#End tabPanel
           
#------------------ Yield Plot-------------------------------------------        
           tabPanel("Yield Plot",
                    plotOutput(height='500px','plot.Yield')
           ),#End tabPanel
           
           #------------------ Time Series ------------------------------------------- 
           tabPanel("Time Series",
                    plotOutput("srt"),
                    conditionalPanel(condition = "input.dataType == 'Run'",
                                     plotOutput('runesc')
                    )
             ),#End tabPanel
           
           #------------------ Residuals  --------------------------------------------- 
           tabPanel("Residuals", 
                    plotOutput("Resid"),
                    p(strong("Durbin-Watson Serial Correlation Analyses")), 
                    verbatimTextOutput('dwtest')),
           
           #------------------ Bootstrap ----------------------------------------------- 
           tabPanel("Bootstrap",
                    verbatimTextOutput('bsummary'),
                    verbatimTextOutput('bquantile'),
                    plotOutput("bhist"))

           )#End tabsetPanel
          )#End mainPanel
         )#End tabPanel
        ),#End nabVarMenu
#-----------------------------------------------------------------------
#  Panel 3 
#-----------------------------------------------------------------------
   navbarMenu("Escapement Goal Analyses",
       tabPanel("Panel 1",      
         sidebarPanel(width = 3,
            conditionalPanel(condition="input.conditionedPanel == 'Tab 1'",       
                             helpText("TAB 1 SELECTED") 
                      ),    
            conditionalPanel(condition="input.conditionedPanel == 'Tab 2'", 
                            helpText("TAB 2 SELECTED")
                      )
         ),
         mainPanel(
             tabsetPanel(
              tabPanel("Tab 1",
                     p(strong("To be completed"))
                   ),
              tabPanel("Tab 2",
                  p(strong("To be completed"))
                    ),
              id = "conditionedPanel"
               )#End tabsetPanel
             )#End mainPanel
          ),#End tabPanel
      tabPanel("Panel 2",
            sidebarPanel(width = 3,
            p(strong("Extra")
              )            
                  ), 
                mainPanel(
                  tabsetPanel(
                    tabPanel("Tab 1",
                             p(strong("To be completed"))
                             ),
                  tabPanel("Tab 2",
                           p(strong("To be completed"))
                           )
                          
                         )#End tabsetPanel
                      )#End mainPanel
                   )#End tabPanel
    )#End nabVarMenu
  ),#End nabVarPage
#------------------------------------------------------------------------
# Citation Discraimer  
#------------------------------------------------------------------------
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


#=======================================================================    
#  Server:  
#=======================================================================
server<-shinyServer(function(input, output, session){
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 1: Data upload and output 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-----------------------------------------------------------------------
#  1.0 Upload File 
#-----------------------------------------------------------------------  
# Uplaed file 
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

#------------------ Brood Table Construction  ---------------------------------  
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
#-----------------------------------------------------------------------
#  1.0: Output page range determined by data 
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#  1.1: Data output table 
#-----------------------------------------------------------------------
# Data output display
output$table <- renderDataTable(
  {
    data()
  })

# Brood table display
output$btable <- renderDataTable(
  {
    round(brood.table(),0)
  })

# Data summary output disply
output$summary <- renderPrint({
  summary(sr.data())
})
# SR Histogrom 

output$hist <- renderPlot({
  par(mfrow=c(1,2))
  x <- sr.data()
  hist(x$S,main='',xlab='Spawnter')
  hist(x$R,main='',xlab='Recruit')
})

#------------------ Create SR Data  ---------------------------------  
#------------------ Original Data -----------------------------------
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
#------------------ Find first and last brood year ------------------
output$fyear = renderUI({
  fyear <- sr.data.0()$Yr
  selectInput('fyear', 'first brood year', fyear)
})

output$lyear = renderUI({
  year <- sr.data.0()$Yr
  fyear <- input$fyear
  lyear <- rev(year[year>fyear])
  selectInput('lyear', 'last brood year', lyear)
})

#------------------ Trimmed Data for Bayesian Analyses --------------
sr.data <- reactive({
  x <- sr.data.0()
  fyear <- input$fyear
  lyear <- input$lyear    
  x <- x[x$Yr>=fyear & x$Yr<=lyear,]
  return(x)     
})



#-----------------------------------------------------------------------
#  1.2: Create Bayese data 
#-----------------------------------------------------------------------
Bayesdata <- reactive({
  #  Import SR data 
  x <- sr.data()
  # nyrs is the number of years (i.e. number of rows) 
  nyrs <- dim(x)[1]
  R <- x$R
  S <- x$S
  # d is S multiplier
  d <- floor(log10(mean(S)))
  # out is Bayesian data  
  if(input$ar1==TRUE){ar1 <- 1} else {ar1 <- 0}
  out <-list(nyrs=nyrs, S=S, R=R,d=d,ar1=ar1)
  return(out)
})

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Panel 2: SR Data Analyses and Output  
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#===============================================================================
#  JAG Modelse 
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
#---------------------------------------------------------------
#  Beverton Holt  
#---------------------------------------------------------------
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
#---------------------------------------------------------------
#  Deriso-Shunute  
#---------------------------------------------------------------
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
  
#-------------------------------------------------------------------
# SR functions for post pcoessing 
#-------------------------------------------------------------------
# Ricker
  SR.CR <- function(lnalpha,beta,S,d){
    s <- S/(10^d)
    lnR <- log(S) + lnalpha - beta*s
    R <- exp(lnR)
    return(R)
  }
#  Beverton-Holt
  SR.BH <- function(lnalpha,beta,S,d){
    s <- S/(10^d)
    lnR <- lnalpha +log(S) - log(1+beta*s)
    R <- exp(lnR)
    return(R)
  }
#  Deriso-Shunute   
  SR.DS <- function(lnalpha,beta,c,S,d){
    s <- S/(10^d)
    lnR <- log(S) + lnalpha - log(1 + beta*c*s)/c 
    R <- exp(lnR)
    return(R)
  }
if(input$Model=='Ricker'){
    jagmodel <- jag.model.CR
    parameters <- c('lnalpha','beta','sigma') 
    if(input$ar1==TRUE){
    parameters <- c(parameters,'phi','e0')    
    }
    model <- SR.CR
  }
if(input$Model=='Beverton-Holt'){
    jagmodel <- jag.model.BH
    parameters <- c('lnalpha','beta','sigma')
    if(input$ar1==TRUE){
    parameters <- c(parameters,'phi','e0')    
    }
    model <- SR.BH
  } 
if(input$Model=='Deriso-Shunute'){
    jagmodel <- jag.model.DS
    parameters <- c('lnalpha','beta','c','sigma')
    if(input$ar1==TRUE){
    parameters <- c(parameters,'phi','e0')    
    }
    model <- SR.DS
  }
  out <- list(jagmodel=jagmodel,parameters=parameters,model=model)
  return(out)
})

output$test <- renderPrint({ Bayesmodel() })

#===============================================================================
#  End of JAG Modelse 
#===============================================================================

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Pane 2: Run JAG Model 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
run.JAGS <- eventReactive(input$RunBayes,{
  #-----------------------------------------------------------------------  
  # The number of replicates
  boot.n <- (input$n.iter*input$n.chain)
  #-----------------------------------------------------------------------  
  progress <- Progress$new(min=1,max=boot.n)
  on.exit(progress$close())
  progress$set(message = paste('JAG Model in progress'),
               detail = 'This will take a while. Be patient please....')
  for (i in 1:15) {
    progress$set(value = i)
  }
  
#-----------------------------------------------------------------------   
# Import data 
  datnew <-  Bayesdata()
  niter <- input$n.iter
  nburn <- input$n.burnin
  nthin <- input$n.thin
  nchain <- input$n.chain
  #  JAGS model selection 
  jagmodel <- Bayesmodel()$jagmodel
  pars <- Bayesmodel()$parameters
  output <- jags(data=datnew,parameters.to.save=pars, model.file= jagmodel,
                 n.chains=nchain, n.iter=niter,n.burnin=nburn,n.thin=nthin,DIC=TRUE)
  return(output)
})

#-----------------------------------------------------------------------
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Pane 3: Post Data processing 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-----------------------------------------------------------------------
#  1.3: Extract JAG results 
#-----------------------------------------------------------------------
sim <- reactive({
  x <- run.JAGS()
  return(x)
})
#----------- MCMC Data -------------------------------------------------
mcmc <- reactive({
  # Read mcmc data
  mcmc <- as.mcmc(sim())
  # post is mcmc data 
  post <- as.matrix(mcmc)  
  return(post)
})

post.summary <- reactive({
  sim_sum <- print(sim())
  post <- sim_sum$summary
  return(post)
})

output$BayesSum <- renderPrint({
  sim_sum <- print(sim())
  print(sim_sum$summary)
})

#-------------------------------------------------------------------------------
#  5.4 Calculate empirical 
#-------------------------------------------------------------------------------
model.pars <- reactive({
  D <- floor(mean(log10(sr.data()$S)))
# CI interval probability   
  pci <- (100-input$CI)/200
# Read mcmc data
  post <- as.matrix(mcmc())
  ln.alpha <- post[,'lnalpha']
  alpha <- exp(post[,'lnalpha'])
  beta <- post[,'beta']/(10^D)
  sigma <- post[,'sigma']
 if(input$input$Model=='Ricker')
 {
   Seq <- ln.alpha/beta
   Smsy <- Seq*(0.5-0.07*ln.alpha)
   Umsy <- ln.alpha*(0.5-0.07*ln.alpha)
   Smax <- 1/beta
 }
if(input$input$Model=='Beverton-Holt')
  {
    Seq <- (alpha-1)/beta
    Smsy <- (sqrt(alpha)-1)/beta
    Umsy <- 1-sqrt(1/alpha)
    Smax <- NA
  }
  
# calculate mean, lci, uci
  ym <- apply(model.pars,2,mean)
  yl <- apply(model.pars,2,function(x) quantile(x, pci))
  yu <-  apply(model.pars,2,function(x) quantile(x, 1-pci))
# extract model par names 
  parname <- names(ym)
  # create data.frame and output
  out <- data.frame(pars=parname,mean=ym, LCI = yl, UCI = yu) 
  return(out)

})


#-----------------------------------------------------------------------
#  2.0: SR Parameters
#-----------------------------------------------------------------------
SR.out <- reactive({
  post <- as.matrix(mcmc())
  # import model parameter column names
  # extract only mcmc model parameters
  model.pars <- post
  D <- floor(mean(log10(sr.data()$S)))
  ln.alpha <- coef(SR())[1]
  alpha <- exp(ln.alpha)
  beta <- -coef(SR())[2]/(10^D) 
  sigma <- sigma(SR())
  ln.alpha.c <- ln.alpha+0.5*(sigma(SR()))^2

  Seq <- ln.alpha.c/beta
  Smsy <- Seq*(0.5-0.07*ln.alpha)
  Umsy <- ln.alpha*(0.5-0.07*ln.alpha)
  Rmsy <- Smsy*exp(ln.alpha-beta*Smsy)
  MSY <- Rmsy-Smsy
  Smax <- 1/beta
  Rmax <- exp(ln.alpha-1)/beta
  out <- data.frame(t(c(ln.alpha,alpha, beta, sigma, ln.alpha.c,Seq,Smsy,Umsy,Rmsy,MSY,Smax,Rmax)))
  names(out) <- c('ln.alpha','alpha', 'beta', 'sigma','ln.alpha.c','Seq','Smsy','Umsy','Rmsy','MSY','Smax','Rmax')
  return(out)
})



#-----------------------------------------------------------------------
#  1.3: Extract mcmc of missing passage by day, year 
#-----------------------------------------------------------------------
post.samp <- reactive({
# post is mcmc data 
  post <- as.matrix(mcmc())
# Get original data 
  x <- data()
# nyrs is the number of years (i.e. number of columns) 
  nyrs <- dim(x)[2] -1
# ndays is the number of days (i.e. number of rows)  
  ndays <- dim(x)[1]
# Create na.list matrix   
  na.list <- matrix(NA,nyrs,ndays)
# Find Row and column of NA and insert location y[,] name
  for (i in 1:ndays){
    for (j in 1:nyrs){
      na.list[j,i]<- ifelse(is.na(x[i,j+1]),paste0('y[',j,',',i,']'),NA) 
    }
  }
# Vectorize the matrix, and remove NA
# navector is a vector of the y matrix with NA   
  navector <- na.list[which(!is.na(na.list))]
# out is mcmc matrix that include only missing passage estimates   
  out <- post[,navector]
  return(out)  
})


#-------------------------------------------------------------------------------
#  5.4 Extract Annual 95% CI passage 
#-------------------------------------------------------------------------------
pred.y <- reactive({
# CI interval probability   
  pci <- (100-input$CI)/200
# Read mcmc data
  y.pred <- post.samp()
#extract names: this extracts only First part of bracket (year id)
tyear <- substr(colnames(y.pred),3,4)
# Remove comma 
tyear <- ifelse(substr(tyear,2,2)== ',',substr(tyear,1,1),tyear)
# Replace names to year id 
colnames(y.pred) <- tyear
# Combine columns based on year id 
y.pred.yr <- as.data.frame(sapply(unique(colnames(y.pred)), function(x) rowSums(y.pred[, colnames(y.pred) == x, drop = FALSE])))
# Calculate median, CI of missing dates  
# ym, ylow, and yup are median, CI of annual passage of missing dates
ym <- round(apply(y.pred.yr,2,median),0)
ym <- ifelse(ym<0,0,ym)
ylow <- round(apply(y.pred.yr,2,function(x) quantile(x, pci)),0)
ylow<- ifelse(ylow<0,0,ylow)
yup <- round(apply(y.pred.yr,2,function(x) quantile(x, 1-pci)),0)
year <- years()
nyrs <- length(year)
tname <- as.numeric(names(ym))
# Create vector that will include missing years
ym2 <- vector('numeric',nyrs)
yl2 <- vector('numeric',nyrs)
yu2 <- vector('numeric',nyrs)

for(i in tname){
  ym2[i] <- ym[as.numeric(names(ym))==i] 
  yu2[i] <- yup[as.numeric(names(yup))==i] 
  yl2[i] <- ylow[as.numeric(names(ylow))==i] 
}
obs <- data()
esc.ob <- colSums(obs[,-1],na.rm = TRUE)
p.est <- round(100*ym2/(ym2+esc.ob),1)
sum <- data.frame(year, esc.ob, ym2,yl2,yu2,p.est)
names(sum) <- c('year','observed','Median','LCI','UCI','% Est')
return(sum)
})

#-----------------------------------------------------------------------
#  1.1: Data output table 
#-----------------------------------------------------------------------
# Annual passage 
output$ptable <- renderTable({
  pred.y()
})

# Model parameters
output$partable <- renderTable({
  model.pars()
})

# Daily passage by year 
output$ytable <- renderTable({
  pred.yd()[[input$columns2]]
})

#-------------------------------------------------------------------------------
#  5.4 Create  Missing passage by date, year   
#-------------------------------------------------------------------------------
pred.yd <- reactive({
  # Read predicted daily estimate 
  ypred <- y.pred.yd()
  # Get original data 
  x <- data()
  # nyrs is the number of years (i.e. number of columns) 
  nyrs <- dim(x)[2] -1
  # ndays is the number of days (i.e. number of rows)  
  ndays <- dim(x)[1]
  
#  Create Data Matrix of Meidan, L U CI 	  
  # y2m is a matrix of median passage 
  y2m <- matrix(NA,ndays,nyrs)
  # y2u is a matrix of upper CI passage    
  y2u <- matrix(NA,ndays,nyrs)
  # y2l is a matrix of lower CI passage   
  y2l <- matrix(NA,ndays,nyrs)
  
# Fill in predicted passage   
  for (j in 1:nyrs){
    for (i in 1:ndays){
     if(paste0('y[',j,',',i,']') %in% ypred$y){
      y2m[i,j] <- ypred[ypred$y==paste0('y[',j,',',i,']'),2]
      y2l[i,j] <- ypred[ypred$y==paste0('y[',j,',',i,']'),3]
      y2u[i,j] <- ypred[ypred$y==paste0('y[',j,',',i,']'),4]
      }
    }
  }
# Create a list file   
 yd <- list()  
for(i in 1:nyrs){
  # Create a data.frame with observed escapement, estimated, lower CI, and upper CI
  yd[[i]] <- data.frame(x$Date,x[,1+i],y2m[,i],y2l[,i],y2u[,i])
  # Rename the column name 
  names(yd[[i]]) <- c('Date','observed','Median','LCI','UCI')
 }
 names(yd) <- years()
 return(yd)
})

#-----------------------------------------------------------------------
#  Results download
#-----------------------------------------------------------------------
# Downloadable csv of selected dataset ----
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

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Pane 4: Model Diagnoses  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-----------------------------------------------------------------------
#  Trace and density plots 
#-----------------------------------------------------------------------
output$plot.trace <- renderPlot({
  pg <- input$p1
  mcmc <- as.mcmc(sim())
  par(mfrow=c(2,6))
  plot(mcmc,auto.layout=FALSE)
})

#-----------------------------------------------------------------------
#  Passage figures  
#-----------------------------------------------------------------------
output$pfig2 <- renderPlot({
# page number
  pg <- input$pf
# Daily estimate 
  predyd <- pred.yd()
# Annual estimate 
  predy <- pred.y()
# Observed  data 
  x <- data()
# Date format 
  dates <- as.Date(x$Date,"%d-%b")
# Model parameter estimates 
  modelpar <- model.pars()  
# Model parameter names
  pars <- par.cols()  
# nyrs is the number of years (i.e. number of columns) 
  nyrs <- dim(x)[2] -1
# ndays is the number of days (i.e. number of rows)  
  ndays <- dim(x)[1]
  days <- seq(1,ndays)
# import model parameter column names  
  am <- modelpar[modelpar$pars %in% pars$a,2]
  bm <- modelpar[modelpar$pars %in% pars$b,2]
  mum <- modelpar[modelpar$pars %in% pars$mu,2]
  
# create estimated model passage   
  Modelesc <- matrix(0,ndays,nyrs)  
  for (i in 1:ndays){
    for (j in 1:nyrs){
      #     Expected log normal run timing   
      Modelesc[i,j]<- exp(am[j])*exp(-0.5*(log(days[i]/mum[j])/bm[j])^2)
      #     Expected Extreme value normal run timing   
      #      Modelesc[i,j] <-am[j]*exp(-exp(-(x[i]-mum[j])/bm[j])-(x[i]-mum[j])/bm[j]+1)
      #     Expected log logistic run timing
      #     Modelesc[i,j] <- (am[j]*(bm[j]/mum[j])*((x[i]/mum[j])^(bm[j]-1))/(1+(x[i]/mum[j])^bm[j])^2)-1   
    }
  }

# Create a data.frame with observed escapement, estimated, lower CI, and upper CI

# plot graph
par(mfrow=c(2,2),mai=c(.6,.5,.1,.1))
pg1 <-  4*(pg)+1
pg2 <-  min(4*(pg)+4,nyrs)

 for(i in pg1:pg2){
# get annual daily data 
  pred <- predyd[[i]]  
# get annual data 
  y <- predy[i,]
# Get maximum daily passage estimate
  maxdat<-max(pred[,-1],na.rm=TRUE)
# plot observed passage
  plot(dates,pred$observed,col='blue', ylim = c(0,maxdat), xlab= 'Date', ylab='escapement')
  legend('topright',bty ='n', paste(years()[i],'missing counts',y$Median,'CI',y$LCI,'-',y$UCI))
    # plot modeled run timing
  lines(dates,Modelesc[,i],col='red')
    # plot 95% CI lines    
  arrows(dates,y0=pred$LCI,y1=pred$UCI,code=0)
    # plot median passage stimate    
   points(dates,pred$Median, pch=21, col='black',bg='white')
  }
})



})# End of Server 




# Create Shiny app ----
shinyApp(ui, server)
