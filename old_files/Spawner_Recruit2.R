
#===========================================================================
#   1.0 Set Modeling Environment 
#===========================================================================
# Clear data 
rm(list=ls(all=TRUE))
# load packages
library(ggplot2)
library(ggthemes)
library(lmtest)
library(rstan)
#source('C:/Projects/Statistics_Worksheets/R_functions/ggplot_themes.r')
#---------------------------------------------------------------------------
#   1.1 Set Data input output file and directory 
#---------------------------------------------------------------------------
# Set directory where data file is located
data_dir <- 'c:/Projects/Bristol_Bay/'
# set data name 
file_name <- 'Egigik.csv'
dat1 <- read.csv(paste(data_dir,file_name, sep=''), na.strings ='', header = TRUE)
nyrs <- length(dat1$Year)
R <- dat1$Recruit
S <- dat1$Escapement
################################################################
#  2.0: Create STAN Model code					               #
################################################################
SRmodel_1 <- '
data{
	int nyrs;  // number of years
	vector[nyrs] S; // Observed Spawner
	vector[nyrs] R; // Observed Recruit	
	}
transformed data{
    vector[nyrs] lnS;
	vector[nyrs] lnR;
	real d;
	lnS = log(S);
	lnR = log(R);	
    d = floor(log10(mean(S)));
  }
parameters{
	real<lower=0> lnalpha;
	real<lower=0,upper =10> beta;
	real<lower=0> sigma;	
	real<lower=-1,upper=1> phi;
	real<lower=-2,upper=2> lnresid_0;
	} // End Parameters
transformed parameters {
	vector[nyrs] lnRm_1;
	vector[nyrs] lnResid;
	vector[nyrs] lnRm;
    lnRm_1 = lnS + lnalpha - beta * S/(10^d);
    lnResid = lnR - lnRm_1;
	lnRm[1] = lnRm_1[1] + phi * lnresid_0;
    lnRm[2:nyrs] = lnRm_1[2:nyrs] + phi * lnResid[1:(nyrs-1)];
	}
  
model{
//	vector[nyrs] lnRm_1;
//	vector[nyrs] lnResid;
//	vector[nyrs] lnRm;
//    lnRm_1 = lnS + lnalpha - beta * S/(10^d);
//    lnResid = lnR - lnRm_1;
//	lnRm[1] = lnRm_1[1] + phi * lnresid_0;
//   lnRm[2:nyrs] = lnRm_1[2:nyrs] + phi * lnResid[1:(nyrs-1)];
 //  Define Priors
   lnalpha ~ uniform(0,10);
   beta ~ uniform(0,10);
   sigma ~ uniform(0,10);
   phi ~ uniform(-1,1); 
   lnresid_0 ~ normal(0,20);  
 // Likelihood      
     lnR[2:nyrs] ~ normal(lnRm[2:nyrs],sigma);
   } 
//End model	   
'
SRmodel_2 <- '
data{
	int nyrs;  // number of years
	vector[nyrs] S; // Observed Spawner
	vector[nyrs] R; // Observed Recruit	
	}
transformed data{
    vector[nyrs] lnS;
	vector[nyrs] lnR;
	real d;
	lnS = log(S);
	lnR = log(R);	
    d = floor(log10(mean(S)));
  }
parameters{
	real<lower=0> lnalpha;
	real<lower=0,upper =10> beta;
	real<lower=0> sigma;	
	} // End Parameters
transformed parameters {
//	vector[nyrs] lnRm_1;
//    lnRm_1 = lnS + lnalpha - beta * S/(10^d);
	}	
model{
	vector[nyrs] lnRm_1;
    lnRm_1 = lnS + lnalpha - beta * S/(10^d);
//  Define Priors
    lnalpha ~ uniform(0,10);
    beta ~ uniform(0,10);
    sigma ~ uniform(0,100);
 // Likelihood      
    lnR ~ normal(lnRm_1,sigma);
	} 
//End model	   
'
datnew<-list(nyrs=nyrs, R=R, S=S)
starttime=Sys.time()
sim <- stan(model_code=SRmodel_2,data=datnew, iter=10000,thin=10)
Sys.time()-starttime

list_of_posterior <- extract(sim)
pars <- summary(sim,pars=c('lnalpha','beta','sigma'))$summary
lnRm <- summary(sim,pars=c('lnRm'))$summary
lnRm1 <- summary(sim,pars=c('lnRm_1'))$summary
lnalpha <- list_of_posterior$lnalpha
beta <- list_of_posterior$beta*10^(-6)
Seq <- lnalpha/beta
Smsy <- Seq*(0.5-0.07*lnalpha)



plot(dat1$Year,dat1$R,type='l')
lines(dat1$Year,exp(lnRm[,1]),col=2)
lines(dat1$Year,exp(lnRm1[,1]),col=3)

SR <- lm(log(Recruit/Spawner)~Spawner,data=dat1)
s <- seq(0,max(dat1$Spawner),by=5000)
pred <- predict(SR, newdata=data.frame(Spawner=s), se.fit=TRUE, interval="confidence",
               level = 0.95)
foop <- predict(SR, newdata=data.frame(Spawner=s), se.fit=TRUE, interval="prediction",
               level = 0.95)
  rg <- 200000
  yg <- 100000
  lRS <- data.frame(pred$fit)
  ER <- exp(lRS)*s
  se <- pred$se.fit
  res <- pred$residual.scale
  pse <- sqrt(se^2+res^2)
  dft <- pred$df
  erg <- log(rg/s)
  eyg <- log(yg/s + 1)
  prof.Rci <- 1-pt((erg-lRS$fit)/se,dft)
  prof.Rpi <- 1-pt((erg-lRS$fit)/pse,dft)
  prof.Yci <- 1-pt((eyg-lRS$fit)/se,dft)
  prof.Ypi <- 1-pt((eyg-lRS$fit)/pse,dft)
  out <- data.frame(cbind(s,ER,prof.Rci,prof.Rpi,prof.Yci,prof.Ypi ))
  names(out) <- c('s','fit','lwr','upr','prof.Rci','prof.Rpi','prof.Yci','prof.Ypi')



dwtest(log(Recruit/Spawner)~Spawner,data=dat1)


m <- data.frame(foop$fit)
fit <- m$fit
upr <- log(200000/s)


se <- sqrt(foop$se.fit^2+foop$residual.scale^2)
dft <- foop$df
plot(1-pt((upr-fit)/se,dft))


summary(SR)

ln.alpha <- SR$coefficients[1]
alpha <- exp(ln.alpha)
beta <- -SR$coefficients[2]
ln.alpha.c <- ln.alpha+0.5*(sigma(SR))^2
Seq <- ln.alpha.c/beta
Smsy <- Seq*(0.5-0.07*ln.alpha.c)
RSmsy <- Smsy*exp(ln.alpha.c-beta*Smsy)
MSY <- RSmsy-Smsy
Rmax <- exp(ln.alpha.c-1)/beta


windows(width=20,height=20, record = TRUE) 
par(mfrow=c(1,1),mar = c(1.75,1.5,1.5,1.75),oma = c(3,3,3,3)) 

#---------------------------------------------------------------------------
#  Spawner - Recruit 
#---------------------------------------------------------------------------
k <- 1000
s <- seq(0,max(dat1$Spawner),by=5000)
plot(Recruit~Spawner,data=dat1/k,pch=19,col=1,xlim=c(0,max(dat1$Spawner)/k),ylim=c(0,max(dat1$Recruit)/k))
abline(0,1,col=2)
df1 <- data.frame(s=s,y=ER)/k
lines(y.fit~s,data=df1,col=1,lw=2)
lines(y.upr~s,data=df1,col=1,lw=2,lty=2)
lines(y.lwr~s,data=df1,col=1,lw=2,lty=2)
abline(v=Smsy/k,col=1,lty=2)
text(Recruit~Spawner, data=dat1/k,labels=Year*k, cex= 0.8, pos=3)
tex <- c(paste('alpha',round(alpha,1)),paste('beta',round(beta,5)),
    paste('SEQ',round(Seq/k,0)),paste('Smsy',round(Smsy/k,0)))
legend('toprigh',tex,box.lty=0,xjust=0)
########  Add Texts  ###########################################################
mult <- ifelse(k>1,paste0('(x',k,')'),'')
mtext("SR ", side = 3, line = 0, outer = TRUE)
mtext(paste('Recruit',mult), side = 2, line = 1, outer = TRUE)
mtext(paste("Escapemeknt",mult), side = 1, line = 1, outer = TRUE)

#---------------------------------------------------------------------------
#  Spawner - Recruit ggplot
#---------------------------------------------------------------------------
s <- seq(0,max(dat1$Spawner),by=5000)
df1 <- data.frame(x=s,y=ER)
p <- ggplot(dat1, aes(x=Spawner,y=Recruit))
p1 <-p+theme_cowplot()+geom_point()
p2 <- p1+ coord_cartesian(xlim=c(0,max(dat1$Spawner)),ylim=c(0,max(dat1$Recruit)))+
  geom_abline(intercept=0,slope=1,color='red',size=1)+
  scale_x_continuous( labels = function(x){paste0(x/1000)})+ 
  scale_y_continuous(labels = function(x){paste0(x/1000)})
y <-1
if(y==1){p2 <- p2+geom_line(data=df1,aes(x,y.fit))+
  geom_line(data=df1,aes(x,y.lwr),linetype=2)+
  geom_line(data=df1,aes(x,y.upr),linetype=2)+
  geom_vline(xintercept=Smsy,linetype=2)}
p2
  
p4 <- p3 + geom_text(data=dat1,aes(x=Spawner,y=Recruit,label=Year),vjust = -0.8)+
   labs(x='Spawner',y='Recruit')




#---------------------------------------------------------------------------
#  Annual S R  
#---------------------------------------------------------------------------
plot(Recruit~Year,data=dat1,type='l',ylim=c(0,max(dat1$Recruit)))
lines(Spawner~Year,data=dat1,lty=2)
legend('topright',c('Spawner','Recruit'),lty=c(2,1), box.lty=0,xjust=0)
########  Add Texts  ###########################################################
mtext("Annual SR ", side = 3, line = 0, outer = TRUE)
mtext('Recruit/Spawner', side = 2, line = 1, outer = TRUE)
mtext("Year", side = 1, line = 1, outer = TRUE)

	
SRP <- predict(SR)
SRR <- residuals(SR)

#---------------------------------------------------------------------------
#  Annual S R  
#---------------------------------------------------------------------------
plot(SRR~dat1$Year)


lines(Spawner~Year,data=dat1,lty=2)
legend('topright',c('Spawner','Recruit'),lty=c(2,1), box.lty=0,xjust=0)
########  Add Texts  ###########################################################
mtext("Annual SR ", side = 3, line = 0, outer = TRUE)
mtext('Recruit/Spawner', side = 2, line = 1, outer = TRUE)
mtext("Year", side = 1, line = 1, outer = TRUE)

boot.n <- 1000
boot.R <- matrix(0,nrow=boot.n,ncol=3)
colnames(boot.R) <- c('ln.alpha','beta','sigma')
for (i in 1:boot.n)
{
bootR <- sample(SRR,length(SRR),replace = TRUE) + SRP
SRi <- lm(bootR~dat1$Spawner)
boot.R[i,1] <- SRi$coefficients[1]
boot.R[i,2] <- -SRi$coefficients[2]
boot.R[i,3] <- sigma(SRi)
}
boot.R <- as.data.frame(boot.R)
boot.R$alpha <- exp(boot.R$ln.alpha)
boot.R$ln.alpha.c <- with(boot.R,ln.alpha+0.5*sigma^2)
boot.R$Seq <- with(boot.R,ln.alpha.c/beta)
boot.R$Smsy <- with(boot.R,Seq*(0.5-0.07*ln.alpha.c))
boot.R$RSmsy <- with(boot.R,Smsy*exp(ln.alpha.c-beta*Smsy))
boot.R$MSY <- with(boot.R,RSmsy-Smsy)
boot.R$Smax <- with(boot.R,1/beta)
boot.R$Rmax <- with(boot.R,exp(ln.alpha.c-1)/beta)


windows(width=20,height=20, record = TRUE) 
par(mfrow=c(3,3),mar = c(1.75,1.5,1.5,1.75),oma = c(3,3,3,3)) 
hist(boot.R$alpha)
hist(boot.R$beta)
hist(boot.R$Smsy)
hist(boot.R$Seq)
hist(boot.R$RSmsy)
hist(boot.R$MSY)
hist(boot.R$Rmax)
# Create Profile 
Smsy.l <- round(0.1*Smsy,-3)
Smsy.u <- min(round(2*Smsy,-3),1.2*round(Seq,-3))
inc <- round((Smsy.u-Smsy.l)/100,-3)
boot.s <- seq(Smsy.l,Smsy.u,inc)
boot.Smsy <- matrix(0,nrow=boot.n,ncol=length(boot.s))
boot.Yield <- matrix(0,nrow=boot.n,ncol=length(boot.s))
pred.Y <- t(boot.s*t(exp(boot.R[,'ln.alpha']-boot.R[,'beta']%o%boot.s))-boot.s)


for (i in 1:boot.n)
{
boot.Ri <- boot.R[i,]
pred.Y <- with(boot.Ri,boot.s*exp(ln.alpha-beta*boot.s)-boot.s)
boot.Smsy[i,] <- ifelse(pred.Y > 0.9*boot.Ri$MSY,1,0)
boot.Yield[i,] <- pred.Y
}
Y.prob <- colSums(boot.Smsy)/boot.n

#---------------------------------------------------------------------------
#  Smsy Profile 
#---------------------------------------------------------------------------
par(mfrow=c(1,1),mar = c(1.75,1.5,1.5,1.75),oma = c(3,3,3,3))
plot(boot.s,Y.prob,type='l') 
abline(h = 0.9)
BEG.1 <- c(min(boot.s[which(round(Y.prob-0.9)==0)]),max(boot.s[which(round(Y.prob-0.9)==0)]))
abline(h = 0.8)
BEG.2 <- c(min(boot.s[which(round(Y.prob-0.8)==0)]),max(boot.s[which(round(Y.prob-0.8)==0)]))
abline(h = 0.7)
BEG.3 <- c(min(boot.s[which(round(Y.prob-0.7)==0)]),max(boot.s[which(round(Y.prob-0.7)==0)]))
tex <- c(paste('90%',BEG.1[1],'-',BEG.1[2]),paste('80%',BEG.2[1],'-',BEG.2[2]),paste('70%',BEG.3[1],'-',BEG.3[2]))
legend('topright',tex,box.lty=0,xjust=0)

#---------------------------------------------------------------------------
#  Yield Profile 
#---------------------------------------------------------------------------
Yield.m <- colMeans(boot.Yield)
Yield.l <- apply(boot.Yield,2,function(x) quantile(x,0.025))
Yield.u <- apply(boot.Yield,2,function(x) quantile(x,0.975))
plot(boot.s,Yield.m,type='l',ylim=c(0,max(Yield.u)))
lines(boot.s,Yield.l,lty=2)
lines(boot.s,Yield.u,lty=2) 
legend('topright',c('Meann','95% Range'),lty=c(1,2),bty='n')
########  Add Texts  ###########################################################
mtext("Yield Plot", side = 3, line = 0, outer = TRUE)
mtext('Yield', side = 2, line = 1, outer = TRUE)
mtext("Escapement", side = 1, line = 1, outer = TRUE)

BEG.2 <- c(min(boot.s[which(round((Yield.l-100000)/100000,1)==0)]),max(boot.s[which(round((Yield.l-100000)/100000,1)==0)]))

round((Yield.l-100000)/100000,1)








