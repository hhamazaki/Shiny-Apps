#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Shiny Boootstrap Modules  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#===============================================================================
#  BootrepUI Module: Produce Boootstrap replicated parameters 
#  Usage: 
#  UI section 
#  SmsyoutUI("ns.name")
#  Server section
#  callModule(Bootres, "ns.name",SR, srdata)
#  SR: SR model output 
#  srdata: SR data used for the analyses 
#  Output: Bootstrap replicted: lnalpha, beta, phi, sigma
#===============================================================================
BootrepUI <- function(id){
  ns <- NS(id)
  tagList(
  numericInput(ns("bn"), "Number bootstrap replicates", value=10000,min=1000,step=1000)
  )
}


Bootrep <-function(input, output,session,SR,srdata){
#-------------------------------------------------------------------------------
#  1.0: Create Bootstrap Data and output boot SR parameters
#------------------------------------------------------------------------------
boot.SR <- reactive({
# The number of bootstrap replicates
    boot.n <- input$bn
#-----------------------------------------------------------------------  
    progress <- Progress$new()
    on.exit(progress$close())
    progress$set(message = paste(boot.n,'Bootstrap Calculation in progress'),
                 detail = 'This will take a while. Be patient please....')
#-----------------------------------------------------------------------   
    D <- floor(mean(log10(srdata()$S))) 
    s <- srdata()$S/(10^D)
    # Calculate presdicted ln(R/S)    
    SRP <-predict(SR())
    # Calculate resicuals     
    SRR <-residuals(SR())
    # Extract AR1 phi
    phi <- coef(SR()$modelStruct$corStruct,unconstrained=FALSE)
    # If model is standard Ricker, phi is 0     
    phi <- ifelse(is.null(phi),0,phi)
    # The number of sample size     
    n <- length(SRR)
#----------------------------------------------------------------------- 
# Create bootstrap residuals    
#----------------------------------------------------------------------- 
    # Step 1: Calculate AR1 residuals: v is iid  
    # when phi = 0,v = residual     
    v <- SRR[-1] - phi*SRR[-n]
    # Step 2: Assign fist v be AR1[1] residual
    v <- c(SRR[1],v)
    # Step 3: Create empty vecotr for AR1
    eb <- numeric(n)
    # Step 5: resample v (iid) with replacement.  Ths creates a list file with 
    # boot.n replicates of boots trap samples 
    resamples <- lapply(1:boot.n, function(x) sample(v, replace = TRUE))
    # Step 6: create matrix of residuals wtih row boot.n, colums n
    vb <- matrix(unlist(resamples), ncol = n, byrow = TRUE)
    # Step 7: Add model predicted:  output1 is bootstrap replicates of y for phi = 0.  
    Y.b <- t(t(vb)+SRP)
    # Bootstrap with AR1 logic 
    # e[1] = v[1],  e[2] = phi*v[1]+v[2], e[3] = phi^2*v[1]+phi*v[2]+v[3],...
    # e[n] = phi^n*v[1]+phi^(n-1)*v[2]+phiphi^(n-2)v[3] +....+v[n]
    
    boot <- matrix(0,nrow=boot.n,ncol=4)
    # Add column name 
    colnames(boot) <- c('lnalpha','beta','phi','sigma') 
# Case 1: standard Ricker Model 
if(phi==0){    
    for (i in 1:boot.n) {
        bootR <- Y.b[i,]
        SRi <- gls(bootR~s,method='ML')
      # Extract, Ricker lnalpha, beta, phi,sigma
      boot[i,1] <- SRi$coefficients[1]
      boot[i,2] <- -SRi$coefficients[2]
      boot[i,3] <- 0
      boot[i,4] <-  sigma(SRi)
      progress$set(value = i/boot.n)
      # Increment the progress bar, and update the detail text.
      progress$inc(1/boot.n, detail = paste("Completed", round(100*i/boot.n,0),"%"))
    }
    }
# Case 2: AR1 Ricker Model 
if(phi!=0){  
      # Step 1 calculate vector 1,phi,phi^2,phi^3,.....phi^(n-1)    
      vph <- phi^(0:(n-1))
      # Step 2: set matrixt 
      e.b <- matrix(0,nrow=boot.n,ncol=n)
      # first column is v[1]
      e.b[,1] <- vb[,1]
      # first column is v[1]
      for (j in 2:n){ e.b[,j] <- colSums(t(vb[,1:j])*rev(vph[1:j])) }
      # e[2] = phi*v[1]+v[2]= sum(c(V1,V2)*c(phi,1))  
      # e[3] = phi^2*v[1]+phi*v[2]+v[3]= sum(c(V1,V2,v3)*c(phi^2,phi,1))
      # e[n] = phi^n*v[1]+phi^(n-1)*v[2]+phiphi^(n-2)v[3] +....+v[n] 
      #  = sum(C(V1,V2,..Vn)*c(phi^(n-1),phi^(n-2), .... ,1)) 
      Y.ar.b <- t(t(e.b)+SRP)
    
    for (i in 1:boot.n) {
      bootR <- Y.ar.b[i,]
      SRi <- gls(bootR~s,correlation=corAR1(form=~1),method='ML')
  # Extract, Ricker lnalpha, beta, phi,sigma
      boot[i,1] <- SRi$coefficients[1]
      boot[i,2] <- -SRi$coefficients[2]
      # Esxtract phi 
      boot[i,3] <- coef(SRi$modelStruct$corStruct,unconstrained=FALSE)
      boot[i,4] <-  sigma(SRi)
      progress$set(value = i/boot.n)
      # Increment the progress bar, and update the detail text.
      progress$inc(1/boot.n, detail = paste("Completed", round(100*i/boot.n,0),"%"))
      }
   }
    return(boot)
  })
 return(boot.SR)
}
  

#-------------------------------------------------------------------------------
# Ricker SR model 
#-------------------------------------------------------------------------------
# SR model function for poost processing ---------------------------------------
SR.CR <- function(lnalpha,beta,S,d){
  s <- S/(10^d)
  lnR <- log(S) + lnalpha - beta*s
  R <- exp(lnR)
  return(R)
}

#------------------------------------------------------------------------
# bootSR: Create bootstrap parameters
#-------------------------------------------------------------------------------
bootSR <- function(boot.n,s,phi,SRP,SRR){
# boot.n: the number of bootstrap replicates 
# s: sapwners 
# D: spawner multiplier
# SRP: model predicted ln(R/s)
# SRR: model residuals.  
#----------------------------------------------------------------------- 
# Create bootstrap data  
#----------------------------------------------------------------------- 
# Get the number of sample size     
  n <- length(SRR)
# Step 1: Calculate AR1 residuals: v is iid  
# when phi = 0,v = residual     
  v <- SRR[-1] - phi*SRR[-n]
# Step 2: Assign fist v be AR1[1] residual
  v <- c(SRR[1],v)
# Step 3: Create empty vecotr for AR1
  eb <- numeric(n)
# Step 5: resample v (iid) with replacement.  Ths creates a list file with 
# boot.n replicates of boots trap samples 
  resamples <- lapply(1:boot.n, function(x) sample(v, replace = TRUE))
# Step 6: create matrix of residuals wtih row boot.n, colums n
  vb <- matrix(unlist(resamples), ncol = n, byrow = TRUE)
# Step 7: Add model predicted:  output1 is bootstrap replicates of y for phi = 0.  
  Y.b <- t(t(vb)+SRP)
#----------------------------------------------------------------------- 
# Create bootstrap AR1 residuals    
#----------------------------------------------------------------------- 
if(phi!=0){  
# Step 1 calculate vector 1,phi,phi^2,phi^3,.....phi^(n-1)    
    vph <- phi^(0:(n-1))
# Step 2: set matrixt 
    e.b <- matrix(0,nrow=boot.n,ncol=n)
# first column is v[1]
    e.b[,1] <- vb[,1]
# first column is v[1]
    for (j in 2:n){ e.b[,j] <- colSums(t(vb[,1:j])*rev(vph[1:j])) }
# e[2] = phi*v[1]+v[2]= sum(c(V1,V2)*c(phi,1))  
# e[3] = phi^2*v[1]+phi*v[2]+v[3]= sum(c(V1,V2,v3)*c(phi*2,phi,1))
# e[n] = phi^n*v[1]+phi^(n-1)*v[2]+phiphi^(n-2)v[3] +....+v[n] 
#  = sum(C(V1,V2,..Vn)*c(phi^(n-1),phi^(n-2), .... ,1)) 
    Y.ar.b <- t(t(e.b)+SRP)
  }
  
#----------------------------------------------------------------------- 
#  Genereate Bootstrap SR parameters    
#----------------------------------------------------------------------- 
boot.R <- matrix(0,nrow=boot.n,ncol=4)
# Add column name 
  colnames(boot.R) <- c('lnalpha','beta','phi','sigma') 
  for (i in 1:boot.n){
    # Create bottstrap random ln(R/S) 
    if(phi==0){
      bootR <- Y.b[i,]
      SRi <- gls(bootR~s,method='ML')
    } else {
      #  With AR1 error 
      bootR <- Y.ar.b[i,]
      SRi <- gls(bootR~s,correlation=corAR1(form=~1),method='ML')
    }  
    # Extract, Ricker lnalpha, beta, phi,sigma
    boot.R[i,1] <- SRi$coefficients[1]
    boot.R[i,2] <- -SRi$coefficients[2]
    # Esxtract phi 
    phi.b <- coef(SRi$modelStruct$corStruct,unconstrained=FALSE)
    # if does not exist, put 0     
    boot.R[i,3] <- ifelse(is.null(phi.b),0,phi.b)
    boot.R[i,4] <-  sigma(SRi)
  }
  return(boot.R)
}
  

