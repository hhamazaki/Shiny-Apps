#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Shiny Bayes Modules  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#===============================================================================
#  BayesModelUI: 
#  Usage: UI section 
#  BayesModelUI("ns.name", "User data (.csv format)")
#  Usage: Server section
#  callModule(dataInput, "ns.name",stringsAsFactors = FALSE)
#===============================================================================
BayesInputUI <- function(id) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    p("Bayesian Model Setting"),
    fluidRow(
      column(6,
        numericInput(ns('n.burnin'),'Burn-in',value=1000,min=0,step = 1000),
        numericInput(ns('n.thin'),'Thinning',value=10,min=0,step = 1)
                     ),  
      column(6,
             numericInput(ns('n.iter'),'Simulation',value=10000,min=0,step=10000), 
             numericInput(ns('n.chain'),'Chains',value=1,min=1,step = 1)
             )
    ),    
    p("Start Bayesian Analyses"),
    actionButton(ns("RunBayes"),"Run")
    )
  }
#-------------------------------------------------------------------------------
BayesInputServer <- function(id,Bayesdata,Bayesmodel){
  moduleServer(id,
               function(input, output, session) {
   # The selected file, if any
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
    titer <- nburn+niter
    nthin <- input$n.thin
    nchain <- input$n.chain
    #  JAGS model selection 
    jagmodel <- Bayesmodel()$jagmodel
    pars <- Bayesmodel()$parameters
    # Run JAGS 
    output <- jags(data=datnew,parameters.to.save=pars, model.file= jagmodel,
                   n.chains=nchain, n.iter=titer,n.burnin=nburn,n.thin=nthin,DIC=TRUE)
    
    return(output)
  })
    } # End fundtion
  ) # End moduleServer
} # End BayesInputServer


#===============================================================================
#  Bayesmodel  Model Functions  
#===============================================================================
#---------------------------------------------------------------
#  Ricker 
#---------------------------------------------------------------
  jag.model.CR <- function(){
    for(y in 1:nyrs){
      s[y] <- S[y]/(10^d)
# v[i] is used for Kalman filter 
      fit[y] <- log(S[y]) + lnalpha - beta * s[y] 
      e[y] <- log(R[y]) - fit[y]
      w[y] ~dnorm(0,tauw)
     }
# ar1 = 0 and kf =0 in standard analysis   
# ar1 = 1 and kf =0 AR1 error moddel 
# ar1 = 0 and kf =1 time-varying alpha
    mu[1] <-  fit[1] + ar1*phi * e0;
    cw[1] <- w[1] #+ kf*v[y]
  for(y in 2:nyrs){	   
      cw[y] <- cw[y-1] + w[y] #+ v[y]
      mu[y] <- fit[y] + kf*cw[y] + ar1*phi*e[y-1]
    }
    #   Define Priors
    lnalpha ~ dunif(0,10)
    beta ~ dunif(0,10)
    sigma ~ dunif(0,10)  
    sigmaw ~ dunif(0,10)
#    sigmav ~ dunif(0,10)
    phi ~ dunif(-1,1)
    e0 ~ dnorm(0,0.001) 
    Tau <- 1/(sigma*sigma)
    tauw <- 1/(sigmaw*sigmaw)	
#    tauv <- 1/(sigmav*sigmav)
# Extract time-varyihg alapha
    for(y in 1:nyrs){
     lnalphai[y] <- lnalpha+cw[y] 
    }
# log nornmal Likelihood 
    for(y in 1:nyrs){     
      R[y] ~ dlnorm(mu[y],Tau)
    }  
  }
  
# SR model function for post processing ---------------------------------------
  SR.CR <- function(lnalpha,beta,S,d){
    s <- S/(10^d)
    lnR <- log(S) + lnalpha - beta*s
    R <- exp(lnR)
    return(R)
  }

# Estimate Biological Reference for post processing ------------------------------
  BR.CR <- function(lnalpha,beta,d){
    Seq <- (10^d)*lnalpha/beta
    Smsy <- Seq*(0.5-0.07*lnalpha)
    Umsy <- lnalpha*(0.5-0.07*lnalpha)
    Smax <- (10^d)/beta
    out <- data.frame(Seq,Smsy,Umsy,Smax)
    return(out)
  }    
  
  
#-------------------------------------------------------------------------------
#  Beverton Holt  
#-------------------------------------------------------------------------------
  jag.model.BH <- function(){
    for(y in 1:nyrs){
      s[y] <- S[y]/(10^d)
      fit[y] = lnalpha + log(S[y]) -log(1+beta*s[y])
      e[y] = log(R[y]) - fit[y]
      w[y] ~dnorm(0,tauw)
    }
# ar1 = 0 and kf =0 in standard analysis   
# ar1 = 1 and kf =0 AR1 error moddel 
# ar1 = 0 and kf =1 time-varying alpha
    mu[1] <-  fit[1] + ar1*phi * e0;
    cw[1] <- w[1] #+ v[y]
    for(y in 2:nyrs){	   
      cw[y] <- cw[y-1] + w[y] #+ v[y]
      mu[y] <- fit[y] + kf*cw[y] + ar1*phi*e[y-1]
    }
    # Define Priors
    lnalpha ~ dunif(0,10)
    beta ~ dunif(0,10)
    phi ~ dunif(-1,1)
    e0 ~ dnorm(0,0.001)     
    sigma ~ dunif(0,10)
    sigmaw ~ dunif(0,10)
#    sigmav ~ dunif(0,10)
    Tau <- 1/(sigma*sigma)
    tauw <- 1/(sigmaw*sigmaw)	
#    tauv <- 1/(sigmav*sigmav)	
# Extract time-varyihg alapha
    for(y in 1:nyrs){
      lnalphai[y] <- lnalpha+cw[y] 
    }
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
# Estimate Biological Reference for post processing ------------------------------
  BR.BH <- function(lnalpha,beta,d){
    alpha <- exp(lnalpha)
    Seq <- (10^d)*(alpha-1)/beta
    Smsy <- (10^d)*(sqrt(alpha)-1)/beta
    Umsy <- 1-sqrt(1/alpha)
    Smax <- NA
    out <- data.frame(Seq,Smsy,Umsy,Smax)
    return(out)
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

#-------------------------------------------------------------------------------
#  State-Space Model 
#-------------------------------------------------------------------------------
  jag.model.SS <- function(){
#-------------------------------------------------------------------------------
# fage: first age of fish returning, 
# lage: last age of fish returtning
# nage: number of ages returning: nage = lage-fage+1
#  Brood [b] vs. Calendar [y] Year 
# Brood:    1,  2,  3, ... lage, lage+1, lage+2, lage+3, .... , lage+nyr    
# Calendar: NA, NA, NA, ... NA,  1, 2,  3, ......., nyr        
#-------------------------------------------------------------------------------    
#-------------------------------------------------------------------------------
#  Generate Recruitment prior to calendar Year 
#-------------------------------------------------------------------------------
  for (b in 1:lage) { 
      S[b] <- exp(ln.S[b])
      s[b] <- S[b]/(10^d)
      fit[b] <- ln.S[b] + lnalpha - beta * s[b]
      e[b] ~ dnorm(0,tau)
      w[b] ~ dnorm(0,tauw)
       }	
    # ar1 = 0 and kf =0 in standard analysis   
    # ar1 = 1 and kf =0 AR1 error moddel 
    # ar1 = 0 and kf =1 time-varying alpha
    ar[1] <- e[1];
    cw[1] <- w[1] #+ kf*v[y]
    for(b in 2:lage){	   
# Random walk 
      cw[b] <- cw[b-1] + w[b] #+ kf*v[y]
# AR1 Error 
      ar[b] <- ar1*phi*e[y-1] + e[b]
# Expected Recruitment 
      mu[b] <- fit[b] + kf*cw[b] + ar1*phi*e[b-1]
      R[b] <- exp(mu[b])   
    }

#  calculate maturity functions ------------------------------------------------   
    for (b in 1:nyr){
      for (a in 1:nage){
        # cummulative brood age comp is a logistic function	
        ca <- fage+a-1  # age of fish 
        cum_brp[b,a] <- 1.0/(1.0+exp(mk_y[b]*(me_y[b]-ca)))
        cum_brp[b,nage]<- 1.0  # all fish mature 
      }
      # brp: brood year age proportion (i.e. maturity schedule). 	 
      brp[b,1]<- cum_brp[bg,1]	 
      for (a in 2:nage){
        brp[b,a] <- cum_brp[b,a] - cum_brp[b,a-1];	 
      }
    }
    
    for(y in 1:nyrs){
      s[y] <- S[y]/(10^d)
      # v[i] is used for Kalman filter 
      fit[y] <- log(S[y]) + lnalpha - beta * s[y] 
      e[y] <- log(R[y]) - fit[y]
      w[y] ~dnorm(0,tauw)
    }
    # ar1 = 0 and kf =0 in standard analysis   
    # ar1 = 1 and kf =0 AR1 error moddel 
    # ar1 = 0 and kf =1 time-varying alpha
    mu[1] <-  fit[1] + ar1*phi * e0;
    cw[1] <- w[1] #+ kf*v[y]
    for(y in 2:nyrs){	   
      cw[y] <- cw[y-1] + w[y] #+ kf*v[y]
      mu[y] <- fit[y] + kf*cw[y] + ar1*phi*e[y-1]
      v[y] ~ dnorm(mu[y],tauv)
    }
    
    #   Define Priors
    lnalpha ~ dunif(0,10)
    beta ~ dunif(0,10)
    sigma ~ dunif(0,10)  
    sigmaw ~ dunif(0,10)
    sigmav ~ dunif(0,10)
    phi ~ dunif(-1,1)
    e0 ~ dnorm(0,0.001) 
    Tau <- 1/(sigma*sigma)
    tauw <- 1/(sigmaw*sigmaw)	
    tauv <- 1/(sigmav*sigmav)	
    # Extract time-varyihg alapha
    for(y in 1:nyrs){
      lnalphai[y] <- lnalpha+cw[y] 
    }
    # log nornmal Likelihood 
    for(y in 1:nyrs){     
      R[y] ~ dlnorm(mu[y],Tau)
    }  
  }
  

  
#--- Function Model select -----------------------------------------------------
model_select <- function(smodel,add){
  if(smodel=='Ricker'){
    jagmodel <- jag.model.CR
    parameters <- c('lnalpha','beta','sigma') 
    if(add=='ar1'){ ar1.p <- c('phi','e0')} else {ar1.p <- NULL}
    if(add=='kf'){kf.p <- c('lnalphai','sigmaw')} else {kf.p <- NULL}
    parameters <- c(parameters,ar1.p,kf.p)
    model <- SR.CR
    model.br <- BR.CR
  }
  if(smodel=='Beverton-Holt'){
    jagmodel <- jag.model.BH
    parameters <- c('lnalpha','beta','sigma') 
    if(add=='ar1'){ ar1.p <- c('phi','e0')} else {ar1.p <- NULL}
    if(add=='kf'){kf.p <- c('lnalphai','sigmaw')} else {kf.p <- NULL}
    parameters <- c(parameters,ar1.p,kf.p)
    model <- SR.BH
    model.br <- BR.BH
  } 
  if(smodel=='Deriso-Shunute'){
    jagmodel <- jag.model.DS
    parameters <- c('lnalpha','beta','c','sigma')
    if(add=='ar1'){ ar1.p <- c('phi','e0')} else {ar1.p <- NULL}
    if(add=='kf'){kf.p <- c('lnalphai','sigmaw','sigmav')} else {kf.p <- NULL}
    parameters <- c(parameters,ar1.p,kf.p)      
    model <- SR.DS
  }
  out <- list(jagmodel=jagmodel,parameters=parameters,model=model,model.br = model.br)
  return(out)
}
