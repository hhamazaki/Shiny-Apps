#initialize
library(shiny)
library(shinythemes)
library(datasets)
library(lmtest)
library(MCMCpack)
#=======================================================================    
#  UI:  
#=======================================================================
ui<-fluidPage(
  navbarPage(
    theme = shinytheme("cerulean"),  
    "Aging Error Simulaton",
#-----------------------------------------------------------------------
#  Panel 4  Management Strategy Evaluation 
#-----------------------------------------------------------------------
navbarMenu("Aging Error Simulation",
  tabPanel("Simulation Model",
    sidebarPanel(width = 3,        
      p(strong("Select Chinook salmon population")),
      selectInput(inputId="pop","Stock", 
      choices = c('Copper','Karluk','Kuskokwim')),
       numericInput(inputId="maxH", "Maximum Harvest", value=100000,min=0,step=10000),
       sliderInput(inputId="maxHr", "Maximum Surplus Harvest Rate", value=0.5,min=0,max=1,step=0.1),
       sliderInput(inputId="msigma", "Change Sigma effects", value=1,min=0,max=2,step=0.1),
       actionButton("SimRun","Simulate")
              ), #End sidebarPanel
    mainPanel(tabsetPanel(
           tabPanel("Simulation Run",
                    fluidRow(  
                      p(("")),
                      p((""))
                       ),
                    fluidRow( 
                      column(3,""),
                      column(3,"")
                        ),
                    fluidRow(  
                    plotOutput(height='600px',"simSR"),
#                    dataTableOutput("SRtable")
                    verbatimTextOutput("SRtable")
                            )
                                ),
           tabPanel("Sim Summary",
                    plotOutput(height='600px','agesim')
#                    verbatimTextOutput("altsim.sum")
                    ),
           tabPanel("Sim time series",
                   plotOutput(height='600px',"runsim"),
                   actionButton("SimClear","Clear Results"),
                   downloadButton("simdownload", "Download")
                 ),
           
        tabPanel("Aging Errors",
          fluidPage(
                fluidRow( 
                   column(6,
                      p(strong("Fresswater Aging Errors (+/- %)")),           
                      sliderInput(inputId="efw0", "Age 0: Upreading only", value=25,min=0,max=50,step=5),
                      sliderInput(inputId="efw1", "Age 1", min=-15,max=15,step=.5,value=c(-1,1)),
                      sliderInput(inputId="efw2", "Age 2: Downreading only", value=-25,min=-50,max=0,step=5)
                                                  ),
                    column(6,
                      p(strong("Saltwarter Aging Errors (+/- %)")),           
                      sliderInput(inputId="esw1", "Age 1", min=-25,max=25,step=0.5,value=c(-5,8)),
                      sliderInput(inputId="esw2", "Age 2", min=-25,max=25,step=0.5,value=c(-5,5)),
                      sliderInput(inputId="esw3", "Age 3",min=-25,max=25,step=0.5,value=c(-4,3)),
                      sliderInput(inputId="esw4", "Age 4", min=-25,max=25,step=0.5,value=c(-8,2)),
                      sliderInput(inputId="esw5", "Age 5: Downreading only", value=-25,min=-50,max=0,step=5)     
                        )
                      ) # End fluidRow
                    )# End fluidOPage
           ) # End tabPanel
      )#End tabsetPanel
    )#End mainPanel
  ),#End tabPanel           
           tabPanel("Model Parameters",
              fluidPage(
                title = 'Set MSE Simulaiton Initization Parameters',
                hr(),
              fluidRow( 
              column(3,
                  p(strong("Modeling Parameters")),  
                  sliderInput("EGlu", label = "Escament Goal Range", min = 0, max = 3, value = c(0.8, 1.6),step=0.1),
                  selectInput(inputId="EGm","Escapement Goal", choices = c('Smsy','Smax')),  
                  selectInput(inputId="cmode","Fishery Oepnng above Escapement Goal", choices = c('Lower','Middle','Upper')),              
                  numericInput(inputId="EGY", "Update Escapement Goal Years", value=6,min=1,step=1)
                ),
                 column(3,
              p(strong("Simulation Years")),           
              sliderInput(inputId="burnin", "Burnin", value=25,min=0,max=100,step=5,round=0),
              sliderInput(inputId="train", "Training", value=25,min=0,max=100,step=5,round=0),
              sliderInput(inputId="simy", "Management", value=50,min=0,max=100,step=5,round=0)
               ),
              column(3,
              p(strong("Errors")),       
              sliderInput(inputId="spred", "Preseason Run prediction %E", value=20,min=0,max=100,step=5),
              sliderInput(inputId="simpH", "Management Imprementation %E", value=10,min=0,max=100,step=5),
              sliderInput(inputId="sobsH", "Harvest Observation %E", value=0,min=0,max=100,step=5),
              sliderInput(inputId="sobsE", "Escapement Observation %E", value=0,min=0,max=100,step=5)
               ),
              column(3,
              p(strong("Population Errors")), 
              sliderInput(inputId="phi", "AR1 correlation", value=0.6,min=0,max=1,step=.1),
              sliderInput(inputId="D", "Drishelet D", value=50,min=0,max=200,step=10)
                 )
              ) # End fluidRow
               )# End fluidOPage
        ),#End tabPanel
    tabPanel("Model Description",
          tabsetPanel(
            tabPanel("Aging Errors",
              p(strong("Aging Errors can occur at borh sampling (i.e, sampled fish may not be representable), 
                and reading (i.e., scales are not read correctly). This simulation model only evaluate
                scale readin errors.")),
              h2("Scale Aging Errors"),
              p("Scale aging error occurs when a reader misidentify presnece of absence of age band. 
                  Miscounting of band are usually +/- one year (i.e. reaing saltwater age 2 to 1 or 3), 
                  but miscounting of more than 1 year can occur occasionally."),
              p("This simulation model assumes that aging occurs annually at the time of assessing run age composition.
                  Miscounting of aging band occurs +/- 1 year
                  and aging error occurs in both freshwater and saltwater independently."),
              h3("Scale Aging Error Example"),
              p("Scale of 1.3 can be misread to 0.2,0.3,0.4,1.2,1.4,2.2,and 2.4."),
              p("Simultaneously,scales of 0.2,0.3,0.4,1.2,1.4,2.2,and 2.4 can be misread to 1.3"),
              p("Thus, the number of 1.3 scales read by a reader is a sum of..."),
              p("number of 1.3 scales read correctly"),
              p("+ number of 0.2 scales read incorrectly to 1.3"),
              p("+ number of 0.3 scales read incorrectly to 1.3"),
              p("+ number of 0.4 scales read incorrectly to 1.3"),
              p("+ number of 1.2 scales read incorrectly to 1.3"),
              p("+ number of 1.4 scales read incorrectly to 1.3"),
              p("+ number of 2.2 scales read incorrectly to 1.3"),
              p("+ number of 2.4 scales read incorrectly to 1.3")
                ),
             tabPanel("Model Structure",
              h2("Model Population"),
              p("This model preloaded parameters of several Chinook salamon populations."),
              p("Preloaded Parameters are: Ricker Spawner Recruit Parameters (alpha, beta, sigma),
                 Mean brood return age composition (by freshwater, sltwater), and 
                 Mean recruit used as model starting population size"),
              p("Modeled age composition consists of 3 freshwater(0,1,2) and 5 salwtwater (0,1,2,3,4,5),
                  consiting of brood return ages of 1 to 8."),   
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
               ),
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
                        )
                 )# End TabsetPanel
           )#End tabPanel
        )#End navMenue Panel
     )#End nabVarPage
 )#End fluidPage


#=======================================================================    
#  Server:  
#=======================================================================
server<-shinyServer(function(input, output, session){
#-----------------------------------------------------------------------
#  Panel 1: Data upload and output 
#-----------------------------------------------------------------------
# Upload file 
sr.pars <- reactive({
  if(input$pop=='Kuskokwim'){
    lnalpha <- 1.76927190497737
    beta <- 0.0000097124192446185
    sigma <- 0.211211313595143
    f0 <- c(0,0,0,0,0,0)
    f1 <- c(0,0.002692464,0.19699346,0.383847805,0.388710847,0.027755423)
    f2 <- c(0,0,0,0,0,0)
    Rm <- 213000 
  } else if(input$pop=='Karluk'){
    lnalpha <- 0.300973311604996
    beta <- 0.000038261256433614
    sigma <- 0.379491231776822
    f0 <- c(0,0,0,0,0,0)
    f1 <- c(0,0.032606373,0.119694807,0.310275861,0.450716033,0.086706925)
    f2 <- c(0,0,0,0,0,0)
    Rm <- 8000
  } else if(input$pop=='Copper'){
    lnalpha <- 1.70257080981884
    beta <- 0.0000331771086629641
    sigma <- 0.212559238333044
    f0 <- c(0,0,0,0,0,0)
    f1 <- c(0,0.005731287,0.080795495,0.582928014,0.326540437,0.004004767)
    f2 <- c(0,0,0,0,0,0)
    Rm <- 58000
  }
  pars <- data.frame(lnalpha,beta,sigma,f0,f1,f2,Rm)
  return(pars)
})

#-------------------------------------------------------------------------------
#  Aging Error Control 
#-------------------------------------------------------------------------------
# Freshwaer Aging Error control 
efw <- reactive({
  efw0 <- c(0,input$efw0)/100 
  efw1 <- c(-input$efw1[1],-input$efw1[2])/100
  efw2 <- c(-input$efw2,0)/100
  efw <- rbind(efw0,efw1,efw2)
  return(efw)
})
# Saltwaer Aging Error control 
esw <- reactive({
  esw1 <- c(-input$esw1[1],input$esw1[2])/100
  esw2 <- c(-input$esw2[1],input$esw2[2])/100
  esw3 <- c(-input$esw3[1],input$esw3[2])/100
  esw4 <- c(-input$esw4[1],input$esw4[2])/100
  esw5 <- c(-input$esw5[1],0)/100
  esw <- rbind(esw1,esw2,esw3,esw4,esw5)
  return(esw)
})


#==============================================================================
#  5.0 Function Libararies 
#==============================================================================
#-------------------------------------------------------------------------------
#  5.1 mult: Unit conversion display 
#-------------------------------------------------------------------------------
mult <- reactive({
    u <- as.numeric(input$ui)
    mult <- ifelse(u==1000000,paste0('(x million)'),ifelse(u>1,paste0('(x',u,')'),''))  
    return(mult)
  })
#-------------------------------------------------------------------------------
#   AR 1 process 
#-------------------------------------------------------------------------------
ar1 <- function(n,cv,phi){
    ar1 <- numeric(n)
    ar1[1] <- 0
    for(i in 2:n){
      ar1[i] <- phi*ar1[i-1]+runif(1,-cv,cv)
    }
    return(ar1)
   } 
#-------------------------------------------------------------------------------
#   Aging Error  
#-------------------------------------------------------------------------------
Aging.error <- function(N.ta,efw,esw){
# Apply Freshwater Age readiing error
# Create an matrix by saltwater(row) x freshwater (column)
  foo.a <- cbind(N.ta[1:6],N.ta[7:12],N.ta[13:18]) 
# temporary matrix incorporating freswater reading error   
  foo.fa <- matrix(0,nrow=6,ncol=3)
# Apply freshwater age 0 reading error: 
# Observed age = 
#   P(true age)*(1-p(error of readig up + reading down) 
#   + P(true age+1)*p(error of reading down) + P(true age-1)*p(error of reading up) 
  foo.fa[,1] <-  foo.a[,1]*(1-sum(efw[1,]))+foo.a[,2]*efw[2,1]
# Apply freshwater age 1 reading error   
  foo.fa[,2] <-  foo.a[,2]*(1-sum(efw[2,]))+foo.a[,1]*efw[1,2]+foo.a[,3]*efw[3,1]
# Apply freshwater age 2 reading error   
  foo.fa[,3] <-  foo.a[,3]*(1-sum(efw[3,]))+foo.a[,2]*efw[2,2]
# Apply Saltwater Age readiing error   
# temporary matrix incorporating freswater reading error     
  foo.sa <- matrix(0,nrow=6,ncol=3)
# Apply saltwater age 0 reading error   
  foo.sa[1,]  <- foo.fa[1,]+foo.fa[2,]*esw[1,1]
# Apply saltwater age 1 reading error   
  foo.sa[2,]  <- foo.fa[2,]*(1-sum(esw[1,]))+foo.fa[3,]*esw[2,1]+foo.fa[2,]*esw[1,2]
# Apply saltwater age 2 reading error 
  foo.sa[3,]  <- foo.fa[3,]*(1-sum(esw[2,]))+foo.fa[4,]*esw[3,1]+foo.fa[3,]*esw[2,2] 
# Apply saltwater age 3 reading error   
  foo.sa[4,]  <- foo.fa[4,]*(1-sum(esw[3,]))+foo.fa[5,]*esw[4,1]+foo.fa[4,]*esw[3,2] 
# Apply saltwater age 4 reading error     
  foo.sa[5,]  <- foo.fa[5,]*(1-sum(esw[4,]))+foo.fa[6,]*esw[5,1]+foo.fa[5,]*esw[4,2] 
# Apply saltwater age 5 reading error     
  foo.sa[6,]  <- foo.fa[6,]*(1-sum(esw[5,]))+foo.fa[5,]*esw[5,2]  

# Reorganize data     
  N.ta.obs <- c(foo.sa[,1],foo.sa[,2],foo.sa[,3])
  return(N.ta.obs)
 }

#==============================================================================
# Function Libararies 
#==============================================================================

#=======================================================================
# Panel 1: SR Simulation 
#=======================================================================
#-----------------------------------------------------------------------
#  Initialize 
#-----------------------------------------------------------------------
MSE.int <- eventReactive(input$SimRun,{
#-----------------------------------------------------------------------
  sigma <- sqrt(sr.pars()$sigma[1])*input$msigma
  f0 <- sr.pars()$f0
  f1 <- sr.pars()$f1
  f2 <- sr.pars()$f2
# Create  mean age recruit proportion
  p.i <- c(f0,f1,f2)
# Set Drishelet random parameter   
  D <- input$D
# Set AR1 Correlation parameter   
  phi <- input$phi
# first age
  fage <- 1
# number of age groups  
  nages <- 8 
# last age 
  lage <- 8
# Year 
  years <- input$simy
  burnin <-input$burnin
  train <- input$train
# Total Simulation Years 
  nyrs <- burnin+train+years
# Create   
  e.Rec <- ar1(nyrs,sigma,phi)
  e.p <- rdirichlet(nyrs,alpha=p.i*D)
  # output data  
  e.pred <- exp(rnorm(nyrs,0,input$spred/100))
  e.obsH <- exp(rnorm(nyrs,0,input$sobsH/100))
  e.obsS <- exp(rnorm(nyrs,0,input$sobsE/100))
  e.imp <- exp(rnorm(nyrs,0,input$simpH/100))
  out <- list(e.pred = e.pred, e.obsH = e.obsH, e.obsS = e.obsS,e.imp = e.imp, e.Rec = e.Rec, e.p = e.p)
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
  e.pred <- as.vector(MSE.int()$e.pred)
  e.obsH <- as.vector(MSE.int()$e.obsH)
  e.obsS <- as.vector(MSE.int()$e.obsS)
  e.imp <- as.vector(MSE.int()$e.imp)
  e.Rec <- as.vector(MSE.int()$e.Rec)
  e.p <- as.matrix(MSE.int()$e.p)
  
  
# Initial Run size   
  R0 <- sr.pars()$Rm[1]
# first age
  fage <- 1
# last age 
  lage <- 8
# number of ages
  nages <- 8
  years <- input$simy
  burnin <- input$burnin
  train <- input$train
# Total Simulation Years 
  nyrs <- burnin+train+years
#-----------------------------------------------------------------------
#  Import SR and management parameters    
#-----------------------------------------------------------------------  
  lnalpha <- sr.pars()$lnalpha[1]
  beta <- sr.pars()$beta[1]
  Umsy <- lnalpha*(0.5-0.07*lnalpha)
  EGl <- input$EGlu[1]
  EGu <- input$EGlu[2]
#-----------------------------------------------------------------------
#  Create Empty vectors  
#-----------------------------------------------------------------------  
# Recruit
  R <- numeric(nyrs)
# Recruit with no aging error   
  R.obs.na <- numeric(nyrs)
# Recruit with aging error   
  R.obs.ae <- numeric(nyrs)
# Annual Run
  N <- numeric(nyrs)
# Annual Escapement
  S <- numeric(nyrs)
# Annual observed Escapement  
  S.obs <- numeric(nyrs)
# Annual Harvest 
  H <- numeric(nyrs)
# Annual observed Harvest 
  H.obs <- numeric(nyrs)
# Annuual Run by age 
# Modeled run by age  
  N.ta <- matrix(0,ncol=18, nrow=nyrs+lage+2)
# Observed run by age without aginge error
  N.ta.obs.na <- matrix(0,ncol=18, nrow=nyrs+lage+2)
# Observed run by age with aginge error
  N.ta.obs.ae <- matrix(0,ncol=18, nrow=nyrs+lage+2)
# Annual Escapement goals  
  Egoals <- matrix(0,ncol=2, nrow = nyrs+1)
# Annual observed SR parameters   
  SR.sim <- matrix(0,ncol=4, nrow = nyrs)

#---------------------------------------------------------------------------
#   Start simulation 
#---------------------------------------------------------------------------
 for (y in 1:nyrs){
# First generaion is constant   
  if(y<=lage) {N.ta[y,] <- R0*exp(e.Rec[y])*e.p[y,]}  
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
  H[y] <- min(H.target*e.imp[y],0.9*N[y])
# Actual Escapement 
  S[y] <- N[y] - H[y]
# Calculate Future Recruits based on SR 
  R[y] <- exp(lnalpha)*S[y]*exp(-beta*S[y]+e.Rec[y])
    
# Fill Future Return by age
 for (fw in 1:3) {
  for(sw in 1:6) {
      N.ta[y+fw+sw-1,6*(fw-1)+sw] <- R[y]*e.p[y,6*(fw-1)+sw]
      }
    }
# Read Aging Error Control 
    efw <- efw()
    esw <- esw()
# Apply Aging Error   
    N.ta.obs.ae[y,] <- Aging.error(N.ta[y,],efw,esw)
    
# Observed Escapement 
  S.obs[y] <- S[y]*e.obsS[y]
#Observed Harvest
  H.obs[y] <- H[y]*e.obsH[y]
#Observed Run by age without aging error 
  N.ta.obs.na[y,] <- N.ta[y,]*sum(S.obs[y],H.obs[y])/N[y]
#Observed Run by age with aging error 
  N.ta.obs.ae[y,] <- N.ta.obs.ae[y,]*sum(S.obs[y],H.obs[y])/N[y]

#-------------------------------------------------------------------------------
#   Observed Recruits 
#-------------------------------------------------------------------------------
# With Aging Error 
  R.foo.ae <- sum(
      N.ta.obs.ae[y-lage+1,1],
      N.ta.obs.ae[y-lage+2,c(2,7)],
      N.ta.obs.ae[y-lage+3,c(3,8,13)],
      N.ta.obs.ae[y-lage+4,c(4,9,14)],
      N.ta.obs.ae[y-lage+5,c(5,10,15)],
      N.ta.obs.ae[y-lage+6,c(6,11,16)],
      N.ta.obs.ae[y-lage+7,c(12,17)],
      N.ta.obs.ae[y-lage+8,18])
# Without Aging Error 
  R.foo.na <- sum(
      N.ta.obs.na[y-lage+1,1],
      N.ta.obs.na[y-lage+2,c(2,7)],
      N.ta.obs.na[y-lage+3,c(3,8,13)],
      N.ta.obs.na[y-lage+4,c(4,9,14)],
      N.ta.obs.na[y-lage+5,c(5,10,15)],
      N.ta.obs.na[y-lage+6,c(6,11,16)],
      N.ta.obs.na[y-lage+7,c(12,17)],
      N.ta.obs.na[y-lage+8,18])
    
# Create Recruitment data based on observed harvest and escapement
  if(y>lage){
      R.obs.ae[y-lage] <- R.foo.ae
      R.obs.na[y-lage] <- R.foo.na  
    }
    
#-------------------------------------------------------------------------------
#   Active harvest management:  Set Escapment Goal 
#-------------------------------------------------------------------------------
  if(y>=(burnin+train)) {
      # Start esimating SR model parameters
      # Assume data have been collected since train    
      R.est <- R.obs.ae[(burnin+1):(y-lage)]
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
# Simulation Data Output
#-------------------------------------------------------------------------------	 
  # Put NA on years Escaoement goals were not calculated  
  Egoals[1:(burnin+train),] <- NA  
  SR.sim[1:(burnin+train-1),] <- NA  
  # data output
  out <- list(N=N,S=S,H = H,R=R, R.obs.ae=R.obs.ae,R.obs.na=R.obs.na,SR.sim=SR.sim,Egoals=Egoals,
              N.ta.obs.na=N.ta.obs.na[1:nyrs,],N.ta.obs.ae=N.ta.obs.ae[1:nyrs,])
  return(out)  
})
#=================================================================================
#   End of Simulation 
#=================================================================================

#=================================================================================
#   Simulation Outputs 
#=================================================================================
SR.sim.data <- reactive({
  lage <- 8
  years <- input$simy
  burnin <- input$burnin
  train <- input$train
  # Total Simulation Years 
  nyrs <- burnin+train+years
  S <- sim()$S
  R <- sim()$R
  R.obs.ae <- sim()$R.obs.ae
  R.obs.na <- sim()$R.obs.na
  SRdata <- data.frame(S,R,R.obs.ae,R.obs.na)[(burnin+train+1):(nyrs-lage),]
  return(SRdata)  
})

#-----------------------------------------------------------------------
#  SR Model   
#-----------------------------------------------------------------------
# Without aging error 
SR.na <- reactive({
  x <- SR.sim.data()
  model <- lm(log(R.obs.na/S)~S,data=x)
  return(model)
})  
# With aging error 
SR.ae <- reactive({
  x <- SR.sim.data()
  model <- lm(log(R.obs.ae/S)~S,data=x)
  return(model)
})  
# True Recruit  
SR.t <- reactive({
  x <- SR.sim.data()
  model <- lm(log(R/S)~S,data=x)
  return(model)
})  

#-----------------------------------------------------------------------
#  2.0: SR Parameters
#-----------------------------------------------------------------------
SR.sim.out <- reactive({
# lnalpha
  ln.alpha <- c(sr.pars()$lnalpha[1],coef(SR.t())[1],coef(SR.na())[1],coef(SR.ae())[1])
# beta   
  beta <- -c(-sr.pars()$beta[1],coef(SR.t())[2],coef(SR.na())[2],coef(SR.ae())[2])
# Seq  
  Seq <- ln.alpha/beta
# Smsy   
  Smsy <- Seq*(0.5-0.07*ln.alpha)
# parameter output  
  out <- data.frame(ln.alpha,beta,Seq,Smsy)
  return(out)
})

#-----------------------------------------------------------------------
#  Sim SRmodel Predictions  
#-----------------------------------------------------------------------
SRp.sim <- reactive({
  pars <-SR.sim.out()
  maxs <- max(1.5*pars$Seq[1])
  # Prediction model s range  
  s <- seq(0,maxs,length.out =101)
  # Predicttion 
  pred.model <- sr.pars()$lnalpha[1]-sr.pars()$beta[1]*s
  pred.ae <- predict(SR.ae(), newdata=data.frame(S=s))
  pred.na <- predict(SR.na(), newdata=data.frame(S=s))
  pred.t <- predict(SR.t(), newdata=data.frame(S=s))
  # Predicted ln(R/S)
  pred.Rm <- exp(pred.model)*s
  pred.Rae <- exp(pred.ae)*s
  pred.Rna <- exp(pred.na)*s
  pred.Rt <- exp(pred.t)*s
  out <- data.frame(s,pred.Rm,pred.Rt,pred.Rna,pred.Rae)
  return(out)
})  

#--------------------------------------------------------------------------------
#  Simulation SR plots
#--------------------------------------------------------------------------------
output$simSR <- renderPlot({
  par(cex=1.3,xaxs='i',yaxs='i',bty='l')
  simdata <- SR.sim.data()
  SRp <- SRp.sim()
  Par <- SR.sim.out()
  plot(R~S,type='p',ylim=c(0,max(simdata$R)),xlim=c(0,max(SRp$s)), col=2,
       data = simdata,
       xlab=paste('Escapement'),
       ylab=paste('Recruit'))
  points(R.obs.na~S,data = simdata,col=3)
  points(R.obs.ae~S,data = simdata,col=4,pch=19)
  lines(pred.Rm~s,data=SRp,col=1)
  lines(pred.Rt~s,data=SRp,col=2)
  lines(pred.Rna~s,data=SRp,col=3)
  lines(pred.Rae~s,data=SRp,col=4)
  abline(v=Par$Smsy[1],col=1,lty=2)
  abline(v=Par$Smsy[2],col=2,lty=2)
  abline(v=Par$Smsy[3],col=3,lty=2)
  abline(v=Par$Smsy[4],col=4,lty=2)
  abline(0,1)
  legend('topleft',c('Model','Sim','No Age Error','Age Error'),lty=c(1,1,1,1),col=c(1,2,3,4),bty='n')  
})


output$SRtable <- renderPrint({
    out <- SR.sim.out()
    row.names(out) <- c('Modeled','Simulated','No Aging Error','With Aging Error')
    return(out)
  })

#--------------------------------------------------------------------------------
#  Simulation Annual Change 
#--------------------------------------------------------------------------------
output$runsim <- renderPlot({
  par(cex=1.3,xaxs='i',yaxs='i',bty='l')
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
  plot(nyrs,N,type='l',ylim=c(0,max(N)),col=1,
       xlab='Year',
       ylab=paste('Run / Escapement'))
  lines(nyrs,S,lty=2,col=2)
  lines(nyrs,H,lty=3,col=3)
  lines(nyrs,Egoals[nyrs,1],col=4)
  lines(nyrs,Egoals[nyrs,2],col=4)
  col <- ifelse(input$EGm =='Smsy',3,4)
  tn <- ifelse(input$EGm =='Smsy','Smsy','Smax')
    preS <- SR.sim.out()$Smsy[1]
    lines(nyrs,SR[nyrs,col],col=6)
    abline(h=preS,col=6,lty=2)
  legend('topleft',c('Run','Escapement','Harvest',tn),lty=c(1,2,3,1),col=c(1,2,3,6),bty='n')  
})

#--------------------------------------------------------------------------------
#  Simulation Aging Error Annual Change 
#--------------------------------------------------------------------------------
output$agesim <- renderPlot({
  par(cex=1.3,xaxs='i',yaxs='i',bty='l')
  years <- input$simy
  burnin <- input$burnin
  train <- input$train
  # Total Simulation Years 
  nyrs <- burnin+train+years
  N <- sim()$N
  N.ta.obs.na <- sim()$N.ta.obs.na
  N.ta.obs.ae <- sim()$N.ta.obs.ae
  p.N.age.na <- cbind(N.ta.obs.na[,1],rowSums(N.ta.obs.na[,c(2,7)]),rowSums(N.ta.obs.na[,c(3,8,13)]),
                      rowSums(N.ta.obs.na[,c(4,9,14)]), rowSums(N.ta.obs.na[,c(5,10,15)]),
                      rowSums(N.ta.obs.na[,c(6,11,16)]), rowSums(N.ta.obs.na[,c(12,17)]),N.ta.obs.na[,18])/N
  p.N.age.ae <- cbind(N.ta.obs.ae[,1],rowSums(N.ta.obs.ae[,c(2,7)]),rowSums(N.ta.obs.ae[,c(3,8,13)]),
                      rowSums(N.ta.obs.ae[,c(4,9,14)]), rowSums(N.ta.obs.ae[,c(5,10,15)]),
                      rowSums(N.ta.obs.ae[,c(6,11,16)]), rowSums(N.ta.obs.ae[,c(12,17)]),N.ta.obs.ae[,18])/N
  row.names(p.N.age.na) <- c(1:nyrs)
  barplot(t(p.N.age.na),ylim=c(0,1))
  df.bar <- barplot(t(p.N.age.na))
  points(df.bar,p.N.age.ae[,1],pch=19)
  points(df.bar,rowSums(p.N.age.ae[,1:2]),pch=19)
  points(df.bar,rowSums(p.N.age.ae[,1:3]),pch=19)
  points(df.bar,rowSums(p.N.age.ae[,1:4]),pch=19)
  points(df.bar,rowSums(p.N.age.ae[,1:5]),pch=19)
  points(df.bar,rowSums(p.N.age.ae[,1:6]),pch=19)
  points(df.bar,rowSums(p.N.age.ae[,1:7]),pch=19)
  points(df.bar,rowSums(p.N.age.ae[,1:8]),pch=19)

})



   
#=======================================================================
#  Plots and Tables Outputs  
#======================================================================= 

#=======================================================================
# Panel 1: SR Analyses SEction 
#=======================================================================
#-----------------------------------------------------------------------
#  Plot time serise
#-----------------------------------------------------------------------  
output$srt <- renderPlot({
  x <- sr.data()
  u <- as.numeric(input$ui)
  mult <- mult()
  plot(R/u~Yr,data=x,type='l', bty='l',ylim=c(0,with(x,max(R,S)/u)),
       main=input$caption,
       xlab='Brood Year',
       ylab=paste('Spawner / Recruit',mult))
  lines(S/u~Yr,data=x,lty=2)
  legend('topright',c('Spawner','Recruit'),lty=c(2,1),box.lty=0)  
})

output$runesc <- renderPlot({
  x <- data()[,c(1:3)]
  names(x) <-c('Yr','S','R')
  u <- as.numeric(input$ui)
  mult <- mult()
  plot(R/u~Yr,data=x,type='l', bty='l',ylim=c(0,with(x,max(R,S)/u)),
       main=input$caption,
       xlab='Year',
       ylab=paste('Run / Escapement',mult))
  lines(S/u~Yr,data=x,lty=2)
  legend('topright',c('Run','Escapement'),lty=c(1,2),box.lty=0)  
})

#--------------------------------------------------------------------------------
#  Simulation Summary
#--------------------------------------------------------------------------------
output$simsum <- renderPrint({
  options(scipen=999)
  years <- input$simy
  burnin <- input$burnin
  train <- input$train 
  nyrs <- burnin+train+years
  lage <- 8
  S <- sim()$S
  R <- sim()$R
  R.obs.ae <- sim()$R.obs.ae
  R.obs.na <- sim()$R.obs.na
  SRdata <- data.frame(S=S,R=R,R.obs.ae=R.obs.ae,R.obs.na=R.obs.na)[(burnin+train+1):(nyrs-lage),]
  print(summary(SRdata,digits=c(0,0,0,0)))
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

#================================================================================
#  Save multiple Simulation Results
#================================================================================
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

#================================================================================

#--------------------------------------------------------------------------------
#  Simulation Table Output
#--------------------------------------------------------------------------------

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
  plot(nyrs,N.alt[,1]/u,type='l', bty='l',ylim=c(0,max(N.alt))/u,col=1,
       xlab='Year', main=paste('Run',mult))
  for(i in 1:n.rep){
  lines(nyrs,N.alt[,i]/u, col = i)
  }
  legend('topleft',paste('Alt',seq(1,n.rep)),lty=1,col=seq(1,n.rep),bty='n',cex=0.8) 

  plot(nyrs,S.alt[,1]/u,type='l', bty='l',ylim=c(0,max(S.alt))/u,col=1,
       xlab='Year', main=paste('Escapement',mult))
  for(i in 1:n.rep){
    lines(nyrs,S.alt[,i]/u, col = i)
  }
  legend('topleft',paste('Alt',seq(1,n.rep)),lty=1,col=seq(1,n.rep),bty='n',cex=0.8) 

  plot(nyrs,H.alt[,1]/u,type='l', bty='l',ylim=c(0,max(H.alt))/u,col=1,
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
                               