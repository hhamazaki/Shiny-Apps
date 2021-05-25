#===============================================================================
# Shiny_SR_function.R 
# Collections of functions used for Shiny SR model apps
#===============================================================================
#===============================================================================
#  1.0  Input data manipulation
#===============================================================================
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  1.1  maake.brood:   Read run data and create brood table and SR data
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
make.brood <- function(data,fage){
  # fage is the first return age
  fage <- as.numeric(fage)
  # nage is the number of reutn ages 
  # Specifying that first age starts from 4th column and the last column is the 
  # last age, number of ages are the number of colums from 4th col to the last col
  nages <- dim(data)[2]-3
  # lage is the lastreutn ages  
  lage <- fage+nages-1
  # Standardize the run age proporiton, so that sum of proportion is exactly 1    
  p <- data[,-c(1:3)]/rowSums(data[,-c(1:3)])
  # Calculate maximum brood year range: 
  # Minimum year is first year return 
  yr <- seq(min(data[,1])-lage,max(data[,1]))
  # Set up brood year matrix    
  brood <- matrix(0,ncol=nages+2,nrow = length(yr))
  # First column is year 
  brood[,1] <- yr
  # Second column is Escapment by year   
  brood[,2] <- c(rep(NA,lage),data[,2])
  # 3rd to the last columns are brood year return by age    
  for(i in 1:nages){
    brood[,i+2] <- c(rep(NA,lage-fage+1-i),p[,i]*data[,3],rep(NA,fage+i-1))
  }
  # Change to data.frame 
  brood <- data.frame(brood)
  # Name all columns 
  names(brood) <- c('b.Year','Spawner',paste0('b.Age',seq(fage,lage)))
  # Recruit is sum of brood year return by age 
  brood$Recruit <- rowSums(brood[,-c(1:2)])
  # Create SR data 
  SR <- brood[complete.cases(brood),c('b.Year','Spawner','Recruit')]
  out <- list(brood=brood,SR=SR)
  # Output data is a list data    
  return(out)
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  1.2  cut.data: Cut data based on specified years
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 cut.data <- function(data, years){
   x <- data[data$Yr>=years[1] & data$Yr<=years[2],]
   return(x)
 }

 
#===============================================================================
#  2.0  Percentile Method 
#===============================================================================
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  2.1  Tier definition
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
tier_def <- function(tier){
  out <- HTML(
    if(tier =="Tier 1"){
      paste(paste("Escapement goal criteria"),
            "High contrast (> 8)",
            "High measurement error (aerial or foot sruveys)",
            "Low to moderate average harvest rates (<40%)",
            "Goal Range: 20th - 60th percentile",sep = '<br/>')
    } else if(tier == "Tier 2")  {
      paste(paste("Escapement goal criteria"),
            "High contrast (> 8)","Low measurement error (weir or tower sruveys)",
            "Low to moderate average harvest rates (<40%)",
            "Goal Range: 15th - 65th percentile",sep = '<br/>')
    } else {
      paste(paste("Escapement goal criteria"),
            "Low contrast (< 8)",
            "Lowe to moderate average harvest rates (<40%)",
            "Goal Range: 5th - 65th percentile",sep = '<br/>')
    } 
   )
  return(out)
  }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  2.2  Tier goals 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
tier_goals <- function(S){
  # Percentile Analyses in 3 Tiers 
  e.g.1 <- (quantile(S,c(0.2,0.6)))   #Tier 1
  e.g.2 <- (quantile(S,c(0.15,0.65))) #Tier 2
  e.g.3 <- (quantile(S,c(0.05,0.65))) #Tier 3
  e.g <- data.frame(rbind(e.g.1,e.g.2,e.g.3))
  names(e.g) <- c('EGL','EGU')
  return(e.g)
  }

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  2.3  Tier EG 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
tier_EG <- function(tier,S,EG){
contrast <- round(max(S)/min(S),1)
 if(tier == "Tier 1") { e.g <- EG[1,]
} else if(tier == "Tier 2") { e.g <- EG[2,]     
} else if(tier == "Tier 3") { e.g <- EG[3,]       
}
out <- HTML(paste(paste(tier,"Escapement goal range"),
           paste("Escapement Contrast:",contrast),
           paste0(round(e.g[1],0)," - ",round(e.g[2],0)),sep = '<br/>'))
 return(out)
}

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#  2.4  Plot_prcnt 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
Plot_prcnt <- function(tier,e.data,EG,u){
  if(tier == "Tier 1") { e.g <- EG[1,]
  } else if(tier == "Tier 2") { e.g <- EG[2,]     
  } else if(tier == "Tier 3") { e.g <- EG[3,]       
  }
  # Graphics     
  par(yaxs='i',bty='l')
  plot(S/u~Yr,data=e.data,type='l',ylim=c(0,max(e.data$S)/u),
       main = 'Escapement', xlab='Year',ylab=paste('Escapement',mult(u)))
  # Add Escapement Goal range  
  # Alternative: 
  abline(h=EG[1,]/u,col = 3, lty=2)
  abline(h=EG[2,]/u,col = 4, lty=2)
  abline(h=EG[3,]/u,col = 5, lty=2)
  polygon(with(e.data,c(min(Yr),max(Yr),max(Yr),min(Yr))),c(e.g[1],e.g[1],e.g[2],e.g[2]),col=tcol(2,50),border=NA)
  # EG      
  abline(h=e.g/u,col=2,lwd=2)
  txt <- c('Tier 1','Tier 2','Tier 3')
  legend('topright',legend=txt,col=c(3,4,5), lty=2, bty ='n')  
 }



#===============================================================================
# 2.0  sim.out: Read model specification and simulation data, 
#      calculate simulated SEQ, Smsy, Umsy, Smax
#      Clean up simulation results
#===============================================================================
sim.out <- function(sim,d,add,model,model.br){
  #D is multiplier.   
  D <- as.numeric(d)
  # sim should include followings: 'lnalpha','beta', lnalphai
  post <- as.data.frame(sim)
  if(add=='kf'){
    post$lnalpha <- apply(post[,grep(pattern='lnalphai',names(post),value=TRUE)],1, FUN=median, na.rm=TRUE)
    }
  post$alpha <- exp(post$lnalpha)
# Calculate Seq, Smsy, Umsy, Smax    
#  post <- data.frame(post,model.br(post$lnalpha,post$beta,D)) 
  
#-------------------------------------------------------------------------------
# Ricker model 
#-------------------------------------------------------------------------------

   if(model =='Ricker')
  {
    post$Seq <- with(post,lnalpha/beta)*(10^D)
    post$Smsy <- with(post,Seq*(0.5-0.07*lnalpha))
    post$Umsy <- with(post,lnalpha*(0.5-0.07*lnalpha))
    post$Smax <- with(post,1/beta)*(10^D)
  }
#-------------------------------------------------------------------------------
# Beverton Holt model 
#-------------------------------------------------------------------------------
  if(model =='Beverton-Holt')
  {
    post$Seq <- with(post,(alpha-1)/beta)*(10^D)
    post$Smsy <- with(post,(sqrt(alpha)-1)/beta)*(10^D)
    post$Umsy <- with(post, 1-sqrt(1/alpha))
    post$Smax <- NA
  }
#-------------------------------------------------------------------------------
# Outlier removing function  
#-------------------------------------------------------------------------------
  remove.out <- function(x){
    out <- boxplot(x, plot=FALSE)$out
    r <- -which(x %in% out)
    return(r)
  }
#-------------------------------------------------------------------------------
# Remove outliers  
#-------------------------------------------------------------------------------
  # Remove obvious outlier data   
  post <- post[post$beta>0,]
  post <- post[post$Umsy>0,]
  if(length(remove.out(post$beta))>0) post <- post[remove.out(post$beta),] 
  if(length(remove.out(post$alpha))>0) post <- post[remove.out(post$alpha),] 
  if(length(remove.out(post$Seq))>0) post <- post[remove.out(post$Seq),] 
  if(length(remove.out(post$Smax))>0) post <- post[remove.out(post$Smax),] 
  return(post)
}

#===============================================================================
# 3.0  plot_denisty: Read simulation data and plot SR Density plots
# Plot MCMC density plots of alpha, beta, phi, Seq, Smsy, UmsY, Smax 
#===============================================================================
plot_density <- function(sim,D,ar1,model='Ricker'){
  par(mfrow=c(2,4),mar = c(1.75,1.5,1.5,1.75),xaxs='i',yaxs='i',bty='l')
  plot(density(sim$alpha),main='alpha',xlab='',ylab='')
  plot(density(sim$beta),main=paste0('beta',' x 10^(',-D,')'),xlab='',ylab='')
  if(ar1==TRUE){
    plot(density(sim$phi),main='Phi',xlab='',ylab='')
  }
  plot(density(sim$Seq), main='SEQ',xlab='',ylab='')
  plot(density(sim$Smsy),main='Smsy',xlab='',ylab='')
  plot(density(sim$Umsy), main='Umsy',xlab='',ylab='')
  # Smax esists only Ricker SR model.   
  if(model=='Ricker'){
    plot(density(sim$Smax), main='Smax',xlab='',ylab='')
  }
}

#===============================================================================
# 4.0  SR.pred.sim:  Read simulation data, and create  
#   Create predicted Recruit and Yied (CI,PI) at given S
#===============================================================================
SR.pred.sim <-function(SRpar,D,max.s,srmodel){
#---------- Extract MCMC SR Model Parameters ---------------------------------
  lnalpha <-SRpar$lnalpha
  beta <- SRpar$beta
  sigma <- SRpar$sigma
  # This makes largest numbers into integer (e.g. 100000)
  maxb <- ceiling(max.s/(10^D))*(10^D)
  # Cut into 201 segments (can be increased) 
  S <- seq(0,maxb, length.out=201) 
  nrow <- length(lnalpha)  
  # Create Expected mean and observed Recruit MCMC matrix    
  mc.R <- matrix(NA,nrow=nrow,ncol=201)   # Model expected recruit -------------
  mc.R.p <- matrix(NA,nrow=nrow,ncol=201) # Model expected observed recruit-----
#---------- Calculate expecte rueturns from each MCMC ------------------------ 
  for(i in 1:nrow){
    # Calculated expected Returns form each MCMC SR model parameters   
    mc.R[i,] <- srmodel(lnalpha[i],beta[i],S,D)
    # mc.R.p adds observation error (sigma)  
    mc.R.p[i,] <- exp(rnorm(201,log(mc.R[i,]),sigma[i]))
  }  
# Create expected mean and observed Yield matric
  mc.Y <-  t(t(mc.R)-S) 
  mc.Y.p <-  t(t(mc.R.p)-S) 
#------  Create Output list file -----------------------------------------------  
  out <- list()
  out$S <- S
  out$R <- mc.R
  out$R.p <- mc.R.p
  out$Y <- mc.Y
  out$Y.p <- mc.Y.p
  return(out)
}


#===============================================================================
# 4.1  Output Predicted Run and CI-PI   
#  Create CI and PI range of Recruit and Yield
#===============================================================================
pred_CI<- function(SR.pred, CI) {
#--- Import predicted data -----------------------------------------------------
  Pred <-SR.pred
#--- User defined % interval range ---------------------------------------------  
  pci <- (100-CI)/200
  # Model used S
  S <- Pred$S
  # Median RS 
  RS.md <- apply(Pred$R,2,median)
  # Mean RS
  RS.me <- apply(Pred$R,2,mean)
  # Calculate Lower and upper CI-PI
  # Lower CI 
  Rl <- apply(Pred$R,2,function(x) quantile(x, pci))
  # Upper CI   
  Ru <-  apply(Pred$R,2,function(x) quantile(x, 1-pci))
  # Lower PI   
  Rl.p <- apply(Pred$R.p,2,function(x) quantile(x, pci))
  # Upper PI    
  Ru.p <- apply(Pred$R.p,2,function(x) quantile(x, 1-pci))
  # Create dataframe   
  out <- data.frame(cbind(S,RS.md,RS.me,Rl,Ru,Rl.p,Ru.p))
  # Name 
  names(out) <- c('S','RS.md','RS.me','Rl','Ru','Rl.p','Ru.p')
  return(out)
}  

#===============================================================================
# 4.2  Cut out simulation output by specific range 
#===============================================================================
SR.cut <-function(SR.pred,Srange){
  #--- Import predicted data ---------------------------------------------------
  Pred <-SR.pred
  # Model used S
  S <- Pred$S  
  # Get column   
  SS <-  which(S >= min(Srange) & S <= max(Srange))
  #------  Create Output list file ---------------------------------------------  
  out <- list()
  out$R <- Pred$R[,SS]
  out$R.p <- Pred$R.p[,SS]
  out$Y <- Pred$Y[,SS]  
  out$Y.p <- Pred$Y.p[,SS]
  return(out)
}


#===============================================================================
#  Figure editing functions 
#===============================================================================
#----- Show multiplier axis ---------------------------------------------------- 
mult <- function(u){
  mult <- ifelse(u==1000000,paste0('(x million)'),ifelse(u>1,paste0('(x',u,')'),''))  
  return(mult)  
}
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


#------ Show two density plots in one fig --------------------------------------
mult.den.plt <- function(dat.a,dat.m,main.t,xlab.t){
  d1 <- density(dat.a)
  d2 <- density(dat.m)
  plot(d2,xlim =c(min(min(d1$x),0),max(d1$x)),axes = FALSE,main='',xlab='',ylab='')
  par(new = TRUE)
  # Mean estimate   
  # Bootstrap estimate  
  plot(d1, main=main.t,xlab=xlab.t, lty=2,ylab='')
  legend('topright',c('Annual','Mean'), lty=c(1,2),bty='n')
}

#===============================================================================
#  Profile Analyses functions 
#===============================================================================
#-------------------------------------------------------------------------------    
# profile --- Profile Calculation Function  
# This function calculated probability of meeting specific target at S range 
# Calculate lower and upper end of S that meeting the target achievement criteria 
#-------------------------------------------------------------------------------    
profile <- function(S, M.rep, mp,tp) {
  #	S: Spawner 
  #	M.rep: Yield, Run, simulaitn matrix
  # M.rep: nrow:  number of simulation replicated 
  # M.rep: ncol:  number of S  nocl = length(S)
  # mp: Target criteria: mp < 1, Percentage of max, mp > 1, specific target 
  # tp: Target percentage  (0-1)
  # Determine the dimention of temporal matrix  
  nrows <- dim(M.rep)[1]
  ncols <- dim(M.rep)[2]
  # Create an empty matrix  
  temp <-  matrix(0,nrow = nrows,ncol=ncols) 
  # For each simulation, assign 1 if value of M.rep is > minimum % M.rep
  # Assign 0  0 if not
  if(mp > 1){
    for(j in 1:nrows){ temp[j,] <- ifelse(M.rep[j,] > mp,1,0) } 
  } else {
    for(j in 1:nrows){temp[j,] <- ifelse(M.rep[j,] > mp*max(M.rep[j,]),1,0)}
  }
  # Mean of temp matrix is a  profile probability  
  M.Rep.prof <- colMeans(temp)
  # Find range of S that intersect with target probabilty  
  S.prof <- S[M.Rep.prof >= tp]
  # Extract min and max S: Profile determined S range
  S.range <- c(NA,NA)
  if(sum(S.prof) > 0){S.range <- c(min(S.prof),max(S.prof))}
  # Output  
  out <- list(S = S, M.prof = M.Rep.prof, S.range = S.range)
  return(out)
}

# plot_profile  --- Profile plotting  Function  ---------------------------------   
plot_profile <- function(TN,prof,prof.st,S,mip,tp,u){
  # TN: Profile Target name 
  # prof: user defined Profile 
  # profst: standard Profile
  # S: Spawner 
  # mip: user defined min p 
  # tp: user defined target p 
  # u: user defined multiplier output  
  mult <- mult(u)
  S <- S/u
  #---------------------------------------------------------------------------
  par(xaxs='i',yaxs='i',bty='l')
  #  Standard profile plots 
  plot(S,prof.st[1,],type='l',col=1, ylim=c(0,1),ylab = 'Probability',
       xlab=paste('Escapement',mult),main=paste(TN,'Profile')) 
  lines(S,prof.st[2,],lty = 2,col=1)
  lines(S,prof.st[3,],lty = 4,col=1)
#  abline(h = stp,lwd=1,col=1)
  #  User deflined profile 
  lines(S,prof,lty = 1,lwd=2,col=4)
  # User defined target  
  abline(h = tp,lwd=2,col=2)
}


#===============================================================================
# Management Strategy Evaluation simulation function 
# MSE.sim  
# This function run MSE : Input data
# srmodel: SR model used to estimate SR parameters
# nyrs: number of simulation (management) years: default 50
# lnalpha.i:  SR lnalpha, beta: SR beta from model
# S0: Actual recent observed escapement that were not used for SR analyses
# e.Rec: Annual recruitment deviation   
# First years of recruits and run are generated by actual observed escapement 
#===============================================================================
#-------------------------------------------------------------------------------
# Fishery opening trigger 
#-------------------------------------------------------------------------------  
# Low: Fishery opens when expected run exceeds lower goal
# Middle: Fishery opens when expected run exceeds Mid goal points 
# Upper:  Fishery opens when expected run exceeds Upper goal 
FTA <-function(cmode,LEG,UEG){
  FT <- ifelse(
  cmode =='Middle',mean(c(LEG,UEG)), # Fishery target: mid EG range 
  ifelse(cmode =='Upper',UEG, # Fishery target: Upper EG range 
  LEG # Fishery target lower EG range}       
  ))
  return(FT)
  }

MSE.sim2 <- function(srmodel,lnalpha.i,beta,S0,D,e.Rec,e.p,e.pred,e.imp,LEG,UEG,cmode){
#-------------------------------------------------------------------------------
#  Create Ages 
#-------------------------------------------------------------------------------  
# lage: last age, nages: number of adult return age, fage: first adult return age
  lage <- length(S0)
  nages <- dim(e.p)[2]
  fage <- lage - nages + 1
  nyrs <- length(e.pred)
#-------------------------------------------------------------------------------
#  Create Empty vector for simulation and outputs  
#-------------------------------------------------------------------------------  
# Recruit: R0: straight from model, R: R0 with error 
  R0 <- numeric(nyrs+lage)
  R <- numeric(nyrs+lage)
# Annual Run (N), Escapement (S), Harvest (H)
  N <- numeric(nyrs)
  S <- numeric(nyrs)
  H <- numeric(nyrs)
# EG: Met lower escapement goal?   
  EG <- numeric(nyrs)
# Annual Run by age 
  N.ta <- matrix(0,ncol=nages, nrow=nyrs+lage*2)
  
#-------------------------------------------------------------------------------
#  Population Dynamics: Initialization
#-------------------------------------------------------------------------------
  for (b.y in 1:lage){
# Calculate expected Recruit from SR model parameter   
  R0[b.y] <- srmodel(lnalpha.i[b.y],beta,S0[b.y],D)
# With error   
  R[b.y] <- R0[b.y]*exp(e.Rec[b.y])
# Distribute recruitment to each year
    for (a in 1:nages){N.ta[b.y+fage+a-1,a] <- R[b.y]*e.p[b.y,a]}
  }  # Expected return        
#-------------------------------------------------------------------------------
#  Population Dynamics: Start Management Strategies  
#-------------------------------------------------------------------------------
  for (y in 1:nyrs){
# Brood year b.y is y + lage
    b.y <-y+lage
# Annual Run is sum of all ages
    N[y] <- sum(N.ta[b.y,]) 
# Predicted Run with error 
    N.pred <- N[y]*e.pred[y]
# Management based on Escapement goal 
    H.target <- ifelse(
# If projected run is less than FT, Fishery is closed       
      N.pred < FT,0,  
# If projected run is greater than FT, 
# Fishery target is max harvest rate of surplus, or max harvest capacity    
      min(input$maxH,(N.pred-FT)*input$maxHr))
# Actual Harvest: Harvest will not exceed N
    H[y] <- min(H.target*e.imp[y],0.99*N[y])
    S[y] <- N[y] - H[y]
    # Escapement goal achievement 
    EG[y] <- ifelse(S[y]>input$LEG,1,0)
    R0[b.y] <- srmodel(lnalpha.i[b.y],beta,S[y],D)
    # Expected return        
    R[b.y] <- R0[b.y]*exp(e.Rec[b.y])
    # Fill ages       
    for (a in 1:nages){N.ta[b.y+fage+a-1,a] <- R[b.y]*e.p[b.y,a]}
  }

  out <- data.frame(cbind(N,S,H,EG))
  #  out <- data.frame(N.ta)
  return(out)
}


#===============================================================================
#  Stars function:  
#===============================================================================
# Modified from Sergei Rodionov's VBA and Andrew Gardner's Matlab codes to produce an R version of STARS. 
# Go to http://www.climatelogic.com/overview for more information, or see the Rodionov (2004; 2006) references in the paper.
# Red noise correction included, but some other functions from VBA code (version 3.2) 
# still missing (e.g. tests in shifts in variance).
# Alistair Seddon, October 2013. 
# alistair.seddon@gmail.com

stars<- function(y=c(rnorm(50), rnorm(50,2,1)), L=20, p=0.05, h=1,  AR1red="none", prewhitening = F) {
  if(AR1red=="none" && prewhitening== T) stop("Impossible to perform prewhitening if AR1 coefficient is not estimated") 		
  m= round((L+1)/3)	
  # formula to estimate subsample size for calculating alpha (Rodionov 2006 + http://www.climatelogic.com/documentation/red-noise-estimation)    	  
  # library("MASS") needed if you want to use Huber correction parameter built in to MASS  
  # ------------------------------------------------------------------------------
  #    hWeightedAverage(xwin)   
  #     Calculates the mean estimate for a given range using Huber's weights.
  # ------------------------------------------------------------------------------      	
  hWeightedAverage<-function(xwin, h){
    # simple estimate of the regime mean for the windowed clip
    dblEstAve <- mean(xwin);
    
    for(jjj in 1:2){
      sumWeights = 0
      sumAve = 0
      
      # Estimate normalised deviation
      xDev = (xwin-dblEstAve)/sqrt(sigL)
      
      # Estimate weights or normalised deviation
      xDev[xDev==0] = 1
      wDev = pmin(rep(1, length(xwin)), h/abs(xDev), na.rm=T)
      
      #sum weights and weighed values
      sumWeights = sum(wDev)
      sumAve = sum(xDev*wDev)
      
      sumAve = sumAve/sumWeights
      sumAve = sumAve*sqrt(sigL) + dblEstAve
      dblEstAve = sumAve
    }		
    dblWeightedAve = dblEstAve
    # hestimate<- huber(xwin, h)
    # dblWeightedAve = hestimate$mu
  }
  
  #-------------------------------------------------------------------
  # estimateSigma
  # Estimate the long-term, L-pt variance (assume homoskedastic signals).
  #-------------------------------------------------------------------
  estimateSigma<-function(x, L){
    # Estimate the long-term length-L variance. If the signal >> length of the analysis window, sample to estimate the variance.	
    nx<-length(x)
    if(nx/L>300) ix <- as.integer(runif(100)*(nx-2*L)+L) else ix<-seq(L,nx,1)
    s<-0
    for(i in 1:length(ix)){
      xwin <- x[(ix[i]-L+1):ix[i]]
      s <- s + var(xwin, na.rm=T)
    }
    sigL1 = s / length(ix)
    sigL1
  }
  
  
  # ------------------------------------------------------------------
  #   getThreshold()
  #   Calculate the critical threshold of deviation that signals regime changes.  
  #   This does not change over the signal.
  # ---------------------------------------------------------------
  getThreshold<-function(L, p, sigL){
    if(prewhitening == T){
      dof <- 2*L-2						# number degrees freedom
    } else {
      dof <- EqN((2*L-2), alpha)
    }
    t <- abs(qt(p/2, dof));              # crit 2-sided t-value
    thresh1 = t*sqrt(2*sigL/L);          # crit deviation
    thresh1
  }
  
  
  # ------------------------------------------------------------------------------
  # OLS estimate of AR1 coefficient from Orcutt and Winokur, 1969, 
  #      Econometrica, 37:1,1-14
  # ------------------------------------------------------------------------------
  OLScalc<-function(x){
    Nobs = length(x)
    ave1 = mean(x[2:Nobs])
    ave2 = mean(x[1:(Nobs-1)])
    sumNom=0
    sumDenom=0
    for(i in 2:Nobs){
      sumNom = sumNom + (x[i] - ave1) * (x[i - 1] - ave2)
      sumDenom = sumDenom + (x[i - 1] - ave2) * (x[i - 1] - ave2)
    }
    if(sumDenom > 0) OLSAR1 = sumNom / sumDenom else OLSAR1 = 0
    OLSAR1
  }
  
  # -------------------------------------------------------------------
  # AR1 correlation estimate (alpha)
  # ------------------------------------------------------------------- 
  AR1cor<-function (m, y){
    m= m #define this in big function above
    ny=length(y)
    iy=seq(1,ny-m+1,1)
    OLS=rep(NA, length(iy))
    # Calculate OLS for sequential samples of length m
    for(i in 1:length(iy)){        
      xwin = y[(iy[i]):(iy[i]+m-1)]
      if(length(xwin[is.na(xwin)]) == 0)   OLS[i] <- OLScalc(xwin)
    }
    
    est<-median(OLS, na.rm=T)
    
    # Calculate IP4	
    IP4= est + 1/m
    for(j in 1:3) IP4=IP4 + abs(IP4)/m
    
    # Calculate MPK
    if (m>4) MPK=((m-1)*est+1)/(m-4) else MPK= est
    
    alphaEst<-c(est, MPK, IP4) 	
  } 	
  
  # -------------------------------------------------------------------
  # Function EqP: calculates t-test using equivalent sample size as in 
  #   von Storch and Zwiers (1999, p.115)
  # ------------------------------------------------------------------- 
  EqP= function(rng1, rng2){   
    # Set standard no-result for if command at end
    EqP = 0
    # Calculate means and variances
    ave1 = mean(rng1, na.rm =T)
    ave2 = mean(rng2, na.rm =T)   
    
    var1 = sd(rng1, na.rm = T)
    var2 = sd(rng2, na.rm = T)   
    
    # Calculate effective sample sizes   
    Ns1 = length(na.omit(rng1))
    if(Ns1 < 2){
      EqP = -1
    } 
    eN1 = EqN(Ns1, alpha)
    
    Ns2 = length(na.omit(rng2))
    if(Ns2 < 2){
      EqP = -1
    } 
    eN2 = EqN(Ns2, alpha)
    
    if(EqP == -1){
      EqP
    } else{
      # Calculate t-statistics
      T_stat = sqrt(var1/eN1 + var2/ eN2)
      T_stat = abs(ave1 - ave2)/ T_stat
      
      EqP = (1-pt(T_stat, eN1 + eN2 -2))*2
      EqP
    }
  }
  
  # -------------------------------------------------------------------
  # EqN: Calculates equivalent sample size as in von Storch and Zwiers (1999, p.115)
  # -------------------------------------------------------------------
  EqN = function(Ns, alpha){
    
    sumEqN = rep(NA, Ns-1)
    for(i in 1: (Ns-1)){
      sumEqN[i] = (1-i/Ns)*alpha^i
    }
    EqN = Ns / (1 + sum(c(sumEqN)))
    
    # just in case
    if( EqN <=2) EqN = 2
    if(EqN > Ns) EqN =Ns
    EqN
  }
  
  # ------------------------------------------------------------------
  #   cusumUp()
  #       Compute the L-pt cusum for positive level changes.  For a positive regime change to be accepted, we require the L-pt lookahead samples	to produce a cusum sequence which does not go negative.
  # -------------------------------------------------------------------
  
  cusumUp<-function(k){
    # LL sets the look ahead length: L, or the number of points until the end of the signal. k is the sample point running through the iteration
    LL <- min(L, N-k+1)
    
    # dblXdev is the length-LL vector of normalized deviations of x outside of the range lvl +/- thresh
    dblXdev = ((x[k:(k+LL-1)]) - (lvl+thresh)) / sqrt(sigL)
    
    #  these are Huber weight values, so large deviations are deemphasized
    dblXdev[dblXdev==0] = 1
    dblXweight = pmin(rep(1, length(dblXdev)), h/abs(dblXdev), na.rm=T)
    
    # % the cusum is the integral of the weighted deviations; we normalize
    # % here, too, by dividing by the sum of the weights
    
    cs<- cumsum(dblXweight*dblXdev)/sum(dblXweight)
    
    # cs<-cumsum(dblXdev) #simple non weighted version
    
    # we check for cusum values below zero, which would indicate a failed
    # regime change; otherwise, we have a positive shift
    if (length(which(cs < 0) > 0)) cs = 0 else  cs = cs[LL]
    cs
  }    
  
  # ------------------------------------------------------------------
  #   cusumDown()   
  #       Compute the L-pt cusum for positive level changes.  For a positive regime change to be accepted, we require the L-pt lookahead samples	to produce a cusum sequence which does not go negative.
  # -------------------------------------------------------------------
  cusumDown<-function(k){
    # LL sets the look ahead length: L, or the number of points until the end of the signal. k is the sample point running through the iteration
    LL <- min(L, N-k+1)
    
    # dblXdev is the length-LL vector of normalized deviations of x outside of the range lvl +/- thresh
    dblXdev = ((x[k:(k+LL-1)]) - (lvl-thresh)) / sqrt(sigL)
    
    #  these are Huber weight values, so large deviations are deemphasized
    dblXdev[dblXdev==0] = 1
    dblXweight = pmin(rep(1, length(dblXdev)), h/abs(dblXdev), na.rm=T)
    
    # % the cusum is the integral of the weighted deviations; we normalize
    # % here, too, by dividing by the sum of the weights
    
    cs<- cumsum(dblXweight*dblXdev)/sum(dblXweight)
    
    # cs<-cumsum(dblXdev) # simple non-weighted version
    
    # we check for cusum values above zero, which would indicate a failed
    # regime change; otherwise, we have a positive shift
    if (length(which(cs > 0) > 0)) cs = 0 else  cs = cs[LL]
    cs
    
  }    
  
  #  -------------------------------------------------------------------------
  #      rsi(k)   
  #      Compute the rsi for a given sample index, regime mean, and critical
  #      threshold.
  #    -------------------------------------------------------------------------
  rsi<-function(k){
    if(x[k] > (lvl + thresh)){
      r = cusumUp(k)
    } else if(x[k] < (lvl - thresh)){
      r = cusumDown(k)
    } else {
      r = 0
    }
    r
  }  
  
  #  -------------------------------------------------------------------------
  # Red noise filtering of timeseries.	
  #  -------------------------------------------------------------------------			
  alpha<-AR1cor(m,y) # calculate alpha estimates
  if(AR1red=="est"){
    alpha = alpha[1]
  }else  if(AR1red=="MPK"){
    alpha = alpha[2]	
  }else if(AR1red=="IP4"){
    alpha = alpha[3]
  }else if(AR1red=="none"){
    alpha= 0
  }
  
  if(alpha<0) alpha <- 0 ; if(alpha>1) alpha <- 1
  
  # Filter time series if selected and select as x for main procedure, otherwise use timeseries
  
  if(prewhitening == T){ 	
    Zt=rep(NA, length(y))
    for(j in 2:length(y)) Zt[j]<-y[j]-(y[j-1]*alpha)
    
    if(alpha>0) x=Zt[-1] else x=y[-1]
    names(x) <- names(y)[-1]
    
  } else x=y[-1]
  
  x <- na.omit(x)
  
  #  -------------------------------------------------------------------------
  # initialization 
  #  -------------------------------------------------------------------------
  sigL = estimateSigma(x, L);           			# sample L-pt variance
  thresh = getThreshold(L, p, sigL);          # critical threshold
  lvl = hWeightedAverage(x[1:L], h);             # initial mean level
  R = rep(0, length(x));                      # rsi values
  RpVal<-rep(0, length(x))
  cp = 1;                                     # current change-point index
  N = length(x)                              # number of samples
  
  if(length(names(y))==0) {
    stop("Stopped: No ages supplied with timeseries")	
  } else ages = names(y) 
  
  
  # Main routine.
  for (k in 2:N){
    R[k] = rsi(k)
    
    #   too few samples to confirm last regime change?
    if (abs(R[k]) > 0 && k > (N-L+1)) break           
    
    #   test for regime shifts and update current regime mean (unless we are within L-pts of most recent change-point)
    if(R[k] == 0){
      if(k >= (cp + L)) lvl = hWeightedAverage(x[cp:k], h)    # same regime, far enough 
      
    } else{
      cp = k                              # regime change
      lvl = hWeightedAverage(x[k:(k+L-1)], h); # same regime, far enough from cp
    }
  }
  
  #  Calculation of new regime sample means and associated pvalues of shifts)
  if(R[length(R)] != 0) R[length(R)]<- 0
  cps<-which(abs(R)>0)
  
  rID<-rep(1, length(x))
  rLabel<-seq(2, length(cps)+1,1)
  Rmean<-rep(0, length(cps)+1)
  
  for(j in 1:length(cps)) rID[cps[j]:N]<-rLabel[j]
  for(j in 1:length(Rmean)) Rmean[j]<- hWeightedAverage(x[rID==j], h)
  # for(j in 1:length(Rmean)) Rmean[j]<- mean(x[rID==j])
  xNames= names(x)
  
  rID1 = rID
  for(j in 1:length(Rmean)) rID[rID==j]<-Rmean[j]
  
  xNA=rep(NA, length(y))
  xNA[match(xNames, names(y))] <- x
  
  RNA=rep(NA, length(y))
  RNA[match(xNames, names(y))] <- c(R)
  
  rIDNA=rep(NA, length(y)) 
  rIDNA[match(xNames, names(y))] <- c(rID)
  starsResult<-cbind(y,xNA, RNA , rIDNA) 
  
  colnames(starsResult) = c("ts", "AR1.cor", "rsi", "mean"); rownames(starsResult) = ages
  
  # Estimate pValues of shifts on either white-noise filtered series, or by using the AR1 correction parameter
  pVal = rep(0, length(cps))
  
  for(j in 1:length(cps)) {
    
    rs1 = x[rID1==j]
    rs2 = x[rID1==(j+1)]
    
    if(length(rs2)==1) {
      next
    } else {
      ifelse(prewhitening ==T, pVal[j] <- t.test(rs1, rs2)$p.value, pVal[j] <- EqP(rs1, rs2))
    }
  }
  
  if(length(which(pVal == -1)) >0) warning("pValue calculation of -1 due regime containing only one sample")		
  
  starsOUT=list(starsResult, alpha, pVal)
  names(starsOUT)=c("starsResult", "alpha", "pVal") 
  starsOUT
  
}


