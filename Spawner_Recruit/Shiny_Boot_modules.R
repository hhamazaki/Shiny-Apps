#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  Shiny Boootstrap Modules  
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#-------------------------------------------------------------------------------
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
# Step 3: Create empty vector for AR1
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
#  Generate Bootstrap SR parameters    
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
  


