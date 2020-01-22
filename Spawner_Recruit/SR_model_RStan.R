#initialize
library(shiny)
library(shinythemes)
library(datasets)
library(lmtest)
library(rstan)
#=======================================================================    
#  UI:  
#=======================================================================
ui<-fluidPage(
  navbarPage(
    theme = shinytheme("cerulean"),  
    "Escapement Goal Analyses",
#-----------------------------------------------------------------------
#  Panel 1:  Data Input and Submit 
#-----------------------------------------------------------------------
  tabPanel("Data Input",
   sidebarPanel(width = 3,
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
    p("Select Column name from data file"),
    # Input: Select what to display
    selectInput(inputId="Yr","Brood Year:", choices = ""),
    selectInput(inputId="S","Spawner:", choices = ""),
    selectInput(inputId="R","Recruit:", choices = ""),
    numericInput("bn", "Number bootstrap replicates", value=10000,min=1000,step=1000),
    p("Submit Data for Analyses"),
    actionButton("go", "Submit"),
    # Horizontal line ----
    tags$hr()
   ), # End SidePanel
  # output
  mainPanel(
    h3(textOutput("caption")),
    tabsetPanel(
      tabPanel("Table",dataTableOutput("table")),
      tabPanel("Summary",
               verbatimTextOutput('summary'),
               plotOutput('hist'))
        )  # End tabsetPanel
      )  # End mainPanel
    ), # End tabanle
#-----------------------------------------------------------------------
#  Panel 2
#-----------------------------------------------------------------------
  tabPanel("SR Analyses",
    sidebarPanel(width = 3,
      textInput(inputId='caption',label='Figue Caption Title',value=''),           
      selectInput(inputId="ui","Axis Dislpay Unit", choices = c(1,10,100,1000,10000,100000,1000000)),  
        checkboxInput(inputId="show.points", "show Years", TRUE), 
        checkboxInput(inputId="show.smsy", "show Smsy", TRUE),
        checkboxInput(inputId="show.smax", "show Smax", TRUE),
        checkboxInput(inputId="show.int", "show Interval", TRUE),
        numericInput(inputId="p.i", "% Interval", value=90,min=0,max=100,step=5),
        selectInput(inputId="Li","Interval Type", choices = c('confidence','prediction'))         
           ),
           mainPanel(tabsetPanel(
             tabPanel("SR Plot",plotOutput(height='500px',"p"),
                      p(strong("Anova Table")), 
                      verbatimTextOutput('anova'),
                      p(strong("SR Parameters")), 
                      verbatimTextOutput('RS.out')                      
                      ),
             tabPanel("Yield Plot",
                      plotOutput(height='500px','py')
                      ),
             tabPanel("Time Series",
                      plotOutput("srt")
                      ),
             tabPanel("Residuals", 
                      plotOutput("Resid"),
                      p(strong("Durbin-Watson Serial Correlation Analyses")), 
                      verbatimTextOutput('dwtest')),
             tabPanel("Bootstrap",
                      verbatimTextOutput('bsummary'),
                      verbatimTextOutput('bquantile'),
                      plotOutput("bhist"))
               )#End tabsetPanel
             )#End mainPanel
           ),#End tabPanel
#-----------------------------------------------------------------------
#  Panel 3 
#-----------------------------------------------------------------------
   navbarMenu("Escapement Goal Analyses",
       tabPanel("Smsy Goal Analyses",      
         sidebarPanel(width = 3,
         p(strong("Smsy Analyses")),  
         numericInput("p1", "Min % of MSY 1", value=90,min=0,max=100,step=5),
         numericInput("p2", "Min % of MSY 2", value=70,min=0,max=100,step=5),
         numericInput("p3", "% Meeting MSY Target", value=90,min=0,max=100,step=5)
         ),
         mainPanel(
             tabsetPanel(
              tabPanel("Smsy Profile",
                   plotOutput(height='500px','bsmsy'),
                   verbatimTextOutput("bsmsyt")),
              tabPanel("Yield  & Recruit Profile",
                       splitLayout(cellWidths = c("50%", "50%"),
                         plotOutput(height='500px','bsmsy.y'),
                         plotOutput(height='500px','bsmsy.r'))
                    )
               )#End tabsetPanel
             )#End mainPanel
          ),#End tabPanel
      tabPanel("Smax Goal Analyses",
            sidebarPanel(width = 3,
            p(strong("Smax Analyses")),            
               numericInput("sp", "Min % of Rmax", value=90,min=0,max=100,step=5),
               numericInput("sp1", "% Meeting Rmax Target", value=90,min=0,max=100,step=5)
                  ), 
                mainPanel(
                  tabsetPanel(
                    tabPanel("Smax Profile",
                             plotOutput(height='500px','bsmax1'),
                             verbatimTextOutput('bsmaxt1')),
                  tabPanel("Run Profile",
                   splitLayout(cellWidths = c("50%", "50%"),                 
                          plotOutput("bsmax.r"),
                          plotOutput("bsmax"))
                          )
                         )#End tabsetPanel
                      )#End mainPanel
                   ),#End tabPanel
       tabPanel("Yield & Recruit Goal Analyses",
         sidebarPanel(width = 3,
         p(strong("Yield GoalAnalyses")),            
         numericInput("y1", "Min Mean Yied", value=100000,min=0, step=10000),
         numericInput("y1p", "Min % Achieve", value=90,min=0, max=100,step=5),
         p(strong("Recruit Goal Analyses")), 
         numericInput("r1", "Min Mean Recruit", value=100000,min=0, step=10000),
         numericInput("r1p", "Min % Achieve", value=90,min=0, max=100,step=5)
          ), 
         mainPanel(
           tabsetPanel(
             tabPanel("Yield Goal Analyses",
                      splitLayout(cellWidths = c("50%", "50%"),
                      plotOutput(height='500px','byield'),
                      plotOutput(height='500px','byp')),
                      splitLayout(cellWidths = c("50%", "50%"),
                      verbatimTextOutput("byt"),
                      textOutput("bypt"))),
             tabPanel("Recruit Goal Analyses",
                      splitLayout(cellWidths = c("50%", "50%"),
                      plotOutput(height='500px','breturn'),
                      plotOutput(height='500px','brp')),
                      splitLayout(cellWidths = c("50%", "50%"),
                      verbatimTextOutput("brt"),
                      textOutput("brpt")))
             )#End tabsetPanel
            )#End maiPanel
          ),#End tabPanel
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
                               ),
          mainPanel(
            tabsetPanel(
              tabPanel("Expected Mean Recruit & Yields",
                            plotOutput("bGAf"),
                            splitLayout(cellWidths = c("50%", "50%"),
                            p(strong("Recruit and Yields Summary")),
                            p(strong("Probability of Meeting Target"))),
                            splitLayout(cellWidths = c("50%", "50%"),
                            verbatimTextOutput("bGAs"),
                            verbatimTextOutput("bGAt"))
                            ),
              tabPanel("Expected Annual Recruit & Yields",
                       plotOutput("bGASR"),
                       splitLayout(cellWidths = c("50%", "50%"),
                                   p(strong("Recruit and Yields Summary")),
                                   p(strong("Probability of Meeting Target"))),
                       splitLayout(cellWidths = c("50%", "50%"),
                                   verbatimTextOutput("bGASRs"),
                                   verbatimTextOutput("bGASRt"))
                      )
            )#End tabsetPanel
          )#End maiPanel 
       )#End tabPanel
   )#End nabVarMenu
  )#End nabVarPage
 )#End fluidPage


#=======================================================================    
#  Server:  
#=======================================================================
server<-shinyServer(function(input, output, session){
#-----------------------------------------------------------------------
#  Panel 1: Data upload and output 
#-----------------------------------------------------------------------
# Uplaed file 
data <- reactive({
    req(input$file1)
    inFile <- input$file1
    #could also store in a reactiveValues
    df <- read.csv(inFile$datapath,
             header = input$header,
             sep = input$sep)
    return(df)
  })  
# Observe Year, Spawner, Recruit Column name
observe({
    var.opts<-names(data())
    updateSelectInput(session, inputId="Yr", choices = var.opts)
    updateSelectInput(session, inputId="S", choices = var.opts)
    updateSelectInput(session, inputId="R", choices = var.opts)
  })

# Create SR data  
sr.data <- reactive({
   x <- data()[,c(input$Yr,input$S,input$R)]
   names(x) <- c('Yr','S','R')
   return(x)   
  }) 
  
# Data output display
output$table <- renderDataTable(
    {
        data()
    })
# Data summary output disply
output$summary <- renderPrint({
      summary(data())
    })
# SR Histogrom 
output$hist <- renderPlot({
  par(mfrow=c(1,2))
  x <- sr.data()
  hist(x$S,main='',xlab='Spawnter')
  hist(x$R,,main='',xlab='Recruit')
 })

#-----------------------------------------------------------------------
#  Panel 2: SR Data Analyses and Output  
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#  1.0: RSTAN SR Model  
#-----------------------------------------------------------------------

SR <- eventReactive(input$go,{
#-----------------------------------------------------------------------  
  progress <- Progress$new(session, min=1, max=50)
  on.exit(progress$close())
  progress$set(message = 'Bayes MCMC in progress',
               detail = 'This may take a while...')
  for (i in 1:50) {
    progress$set(value = i)
    Sys.sleep(0.5)}
#-----------------------------------------------------------------------   
    x <- sr.data()
    R <- x$R
    S <- x$S
    nyrs <- length(R)
    datnew<-list(nyrs=nyrs, R=R, S=S)
    SRmodel <- '
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
real<lower=0,upper =10> lnalpha;
real<lower=0,upper =10> beta;
real<lower=0> sigma;	
} // End Parameters

model{
vector[nyrs] lnRm;
lnRm = lnS + lnalpha - beta * S/(10^d);
//  Define Priors
lnalpha ~ uniform(0,10);
beta ~ uniform(0,10);
sigma ~ uniform(0,100);
// Likelihood      
lnR ~ normal(lnRm,sigma);
} 
//End model	   
'
    model <- stan(model_code=SRmodel,data=datnew, iter=10000,thin=10)
    return(model)
  })  

#-----------------------------------------------------------------------
#  2.0: SR Parameters
#-----------------------------------------------------------------------
SR.out <- reactive({
  pars <- extract(SR())
  d <- floor(log10(mean(SR.data()$S)))
  ln.alpha <- pars$lnalpha
  alpha <-  ln.alpha
  beta <- pars$beta*10^(-d)
  sigma <- pars$sigma
  ln.alpha.c <- ln.alpha+0.5*(sigma)^2
  Seq <- ln.alpha.c/beta
  Smsy <- Seq*(0.5-0.07*ln.alpha.c)
  RSmsy <- Smsy*exp(ln.alpha.c-beta*Smsy)
  MSY <- RSmsy-Smsy
  Smax <- 1/beta
  Rmax <- exp(ln.alpha.c-1)/beta
  out <- data.frame(t(c(ln.alpha,alpha, beta, sigma, ln.alpha.c,Seq,Smsy,RSmsy,MSY,Smax,Rmax)))
  names(out) <- c('ln.alpha','alpha', 'beta', 'sigma','ln.alpha.c','Seq','Smsy','RSmsy','MSY','Smax','Rmax')
  return(out)
})

#-----------------------------------------------------------------------
#  3.0: SR Precictions 
#-----------------------------------------------------------------------
SRp <- reactive({
  par <-SR.out()
  mp <- input$p.i/100
  rg <- input$r1
  yg <- input$y1
# Prediction model s range  
  s <- seq(0,max(1.2*par$Seq,data()$S), length.out =100)
# Predicttion 
#  pred <- predict(SR(), newdata=data.frame(S=s), se.fit = TRUE, interval=input$Li,level = mp)
# Predicted ln(R/S)
#  lRS <- data.frame(pred$fit)
# Predicted R)
#  ER <- exp(lRS)*s
# Model SE
  se <- pred$se.fit
# Model Residual
  res <- pred$residual.scale
# Model df  
  dft <- pred$df
# Calculatge SE for Prediction interval
  pse <- sqrt(se^2+res^2)
# ln(R/S) at given Run target
  erg <- log(rg/s)
# ln(R/S) at given Yield target
  eyg <- log(yg/s + 1)
# Probability of achiving target Rercuits: CI and PI  
  prof.Rci <- 1-pt((erg-lRS$fit)/se,dft)
  prof.Rpi <- 1-pt((erg-lRS$fit)/pse,dft)
# Probability of achiving target Yield: CI and PI  
  prof.Yci <- 1-pt((eyg-lRS$fit)/se,dft)
  prof.Ypi <- 1-pt((eyg-lRS$fit)/pse,dft)
  out <- data.frame(cbind(s,ER,prof.Rci,prof.Rpi,prof.Yci,prof.Ypi ))
  names(out) <- c('s','fit','lwr','upr','prof.Rci','prof.Rpi','prof.Yci','prof.Ypi')
  return(out)
})  

SRp.G <- eventReactive(input$Run,{
  #-----------------------------------------------------------------------  
  progress <- Progress$new(session, min=1, max=15)
  on.exit(progress$close())
  progress$set(message = 'Calculation in progress',
               detail = 'This may take a while...')
  for (i in 1:15) {
    progress$set(value = i)
    Sys.sleep(0.5)}
#-----------------------------------------------------------------------   
  lg <- input$lg
  ug <- input$ug
  s <- seq(lg,ug,length.out=100)
  # Predicttion 
  pred <- predict(SR(), newdata=data.frame(S=s), se.fit = TRUE, interval='none')
  # Predicted ln(R/S)
  lRS <- pred$fit
  # Model SE
  se <- pred$se.fit
  # Model Residual
  res <- pred$residual.scale
  # Model df  
  dft <- pred$df
  # Calculatge SE for Prediction interval
  pse <- sqrt(se^2+res^2)
  boot.t <- matrix(0,nrow=100,ncol=1000)
  for (i in 1:100){
    boot.t[i,] <- rt(1000,dft)
  }
  bRci <- as.vector(exp(boot.t*se+lRS)*s)
  bYci <- as.vector(exp(boot.t*se+lRS)*s - s)
  bRpi <- as.vector(exp(boot.t*pse+lRS)*s)
  bYpi <- as.vector(exp(boot.t*pse+lRS)*s - s)
  out <- data.frame(cbind(bRci,bYci,bRpi,bYpi))
  names(out) <- c('bRci','bYci','bRpi','bYpi')
  return(out) 
})

#-----------------------------------------------------------------------
#  4.0: SR Analyses Outputs  
#-----------------------------------------------------------------------
# Model Parameters 
  output$RS.out <- renderPrint({
    print(SR.out(),digits=c(3,3,10,3,3,0,0,0,0,0,0))
  })  
# ANOVA
  output$anova <- renderPrint({
    anova(SR())
  })
# Durbin-Watson auto-correlaiton  
  output$dwtest <- renderPrint({
    dwtest(SR())
  })

#-----------------------------------------------------------------------
#  5.0 Helper Unit functions  
#-----------------------------------------------------------------------
  mult <- reactive({
    u <- as.numeric(input$ui)
    mult <- ifelse(u==1000000,paste0('(x million)'),ifelse(u>1,paste0('(x',u,')'),''))  
    return(mult)
  })
  
#-----------------------------------------------------------------------
#  6.0 Base SR plot 
#-----------------------------------------------------------------------
base.p <- reactive({
    #  dev.control("enable")
    u <- as.numeric(input$ui)
    mult <- mult()
    x <- sr.data()
    par <-SR.out()
    xp <- x/u
    SRp <- SRp()[,1:4]/u
    plot(R~S,data=xp,pch=19,col=1, 
         main= input$caption,
         xlab=paste("Escapement",mult),ylab=paste('Recruit',mult), bty='l',
         xlim=c(0,max(SRp$s)),ylim=c(0,1.2*max(xp$R)))
    abline(0,1,col=2)
    # Add Predicted   
    lines(fit~s,data=SRp,col=1,lw=2)
    out <-recordPlot()
    #  dev.off()
    return(out)
  })
  
#-----------------------------------------------------------------------
#  7.0 Base Yield plot 
#-----------------------------------------------------------------------
base.py <- reactive({
    #  dev.control("enable")
    u <- as.numeric(input$ui)
    mult <- mult()
    x <- sr.data()
    par <-SR.out()
    SRp <- SRp()[,1:4]/u
    xp <- x/u
    # Plot Basic Yield plot  
    plot((R-S)~S,data=xp,pch=19,col=1, 
         main= input$caption,
         xlab=paste("Escapement",mult),ylab=paste('Yield',mult), bty='l',
         xlim=c(0,max(SRp$s)),ylim=c(min(SRp$lwr-SRp$s),1.2*max(xp$R-xp$S)))
    lines((fit-s)~s,data=SRp,col=1,lw=2)
    abline(h=0,col=2)
    out <-recordPlot()
    #  dev.off()
    return(out)
  })
  
#=======================================================================
#  Bootstrap Analyses 
#=======================================================================
#-----------------------------------------------------------------------
#  1.0: Create Bootstrap Data
#-----------------------------------------------------------------------
 boot <- eventReactive(input$go,{
#-----------------------------------------------------------------------  
   progress <- Progress$new(session, min=1, max=15)
   on.exit(progress$close())
   progress$set(message = 'Bootstrap Calculation in progress',
                detail = 'This may take a while...')
   for (i in 1:15) {
     progress$set(value = i)
     Sys.sleep(0.5)}
#-----------------------------------------------------------------------   
    s <- sr.data()$S
    SRP <-predict(SR())
    SRR <-residuals(SR())
# The number of bootstrap replicates
    boot.n <- isolate(input$bn)
# Create  bootstrap replicates matrix
    boot.R <- matrix(0,nrow=boot.n,ncol=3)
# Add column name 
    colnames(boot.R) <- c('ln.alpha','beta','sigma')

  for (i in 1:boot.n)
    {
# Create bottstrap random ln(R/S)   
      bootR <- sample(SRR,length(SRR),replace = TRUE) + SRP
# Calculate SR model   
      SRi <- lm(bootR~s)
# Extract, Ricker lnalpha, beta, sigma
      boot.R[i,1] <- SRi$coefficients[1]
      boot.R[i,2] <- -SRi$coefficients[2]
      boot.R[i,3] <- sigma(SRi)
      }
# Change to data.frame    
    boot.R <- data.frame(boot.R)
    boot.R$alpha <- exp(boot.R$ln.alpha)
    boot.R$ln.alpha.c <- with(boot.R,ln.alpha+0.5*sigma^2)
    boot.R$Seq <- with(boot.R,ln.alpha/beta)
    boot.R$Smsy <- with(boot.R,Seq*(0.5-0.07*ln.alpha))
    boot.R$RSmsy <- with(boot.R,Smsy*exp(ln.alpha-beta*Smsy))
    boot.R$MSY <- with(boot.R,RSmsy-Smsy)
    boot.R$Smax <- with(boot.R,1/beta)
    boot.R$Rmax <- with(boot.R,exp(ln.alpha-1)/beta)
# Remove bad data (i.e. beta is neagative)     
    boot.R[boot.R[,2]< 0,] <- NA  
    out <- boot.R[complete.cases(boot.R),]
    return(out)
  })
#-----------------------------------------------------------------------
#  2.0 Bootstrap Recruit and Yields Data Out
#-----------------------------------------------------------------------
Y.boot <- eventReactive(input$go,{
#-----------------------------------------------------------------------  
  progress <- Progress$new(session, min=1, max=15)
  on.exit(progress$close())
  progress$set(message = 'Profile Calculation in progress',
               detail = 'This may take a while...')
  for (i in 1:15) {
    progress$set(value = i)
    Sys.sleep(0.5)}
#-----------------------------------------------------------------------  
  boot.n <-dim(boot())[1]  
  par <-SR.out()
  Seq <- par$Seq
  boot.s <- seq(0,Seq,length.out=100)
  boot.R <- boot()
  boot.Rec <- t(boot.s*t(exp(boot.R[,'ln.alpha']-boot.R[,'beta']%o%boot.s)))
  boot.Yield <- t(t(boot.Rec)-boot.s)
  out <- list(Y.boot = boot.Yield, R.boot = boot.Rec, boot.s = boot.s)
  return(out) 
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

#-----------------------------------------------------------------------
#  4.0 Target Yield and Recruit based Escapement Goal
#-----------------------------------------------------------------------  
b.YAg <- reactive({
  yg <- input$y1
  boot.s <- Y.boot()$boot.s 
  boot.Y <- as.matrix(Y.boot()$Y.boot)
  boot.Yp <- apply(boot.Y,2,function(x) ifelse(x >yg,1,0))
  boot.Yp <- colMeans(boot.Yp)
  boot.Ym <- b.YA()$boot.Ym
  boot.Yu <- b.YA()$boot.Yu
  boot.Yl <- b.YA()$boot.Yl  
  # Find Intersections 
  b.l <- boot.s[sign(boot.Yl-yg)==1]
  b.m <- boot.s[sign(boot.Ym-yg)==1]
  b.u <- boot.s[sign(boot.Yu-yg)==1]
  BEG.l <- c(min(b.l),max(b.l))
  BEG.m <- c(min(b.m),max(b.m))
  BEG.u <- c(min(b.u),max(b.u))
  out <- list(boot.Yp=boot.Yp, BEG.l = BEG.l, BEG.m = BEG.m, BEG.u=BEG.u)
  return(out)
  })

# Find Yield Target Intersection 
b.YApg <- reactive({
  ypg <- input$y1p/100
  boot.s <- Y.boot()$boot.s 
  boot.Yp <- b.YAg()$boot.Yp
  # Find Intersections 
  b.p <- boot.s[sign(boot.Yp-ypg)==1]
  BEG.p <- c(min(b.p),max(b.p))
  out <- BEG.p
  return(out)
})

# Find Recruit Target Intersection 
b.RAg <- reactive({
  rg <- input$r1
  boot.s <- Y.boot()$boot.s 
  boot.R <- as.matrix(Y.boot()$R.boot)
  boot.Rp <- apply(boot.R,2,function(x) ifelse(x >rg,1,0))
  boot.Rp <- colMeans(boot.Rp)
  boot.Rm <- b.YA()$boot.Rm
  boot.Ru <- b.YA()$boot.Ru
  boot.Rl <- b.YA()$boot.Rl  
  # Find Intersections 
  b.l <- boot.s[sign(boot.Rl-rg)==1]
  b.m <- boot.s[sign(boot.Rm-rg)==1]
  b.u <- boot.s[sign(boot.Ru-rg)==1]
  BEG.l <- c(min(b.l),max(b.l))
  BEG.m <- c(min(b.m),max(b.m))
  BEG.u <- c(min(b.u),max(b.u))
  out <- list(boot.Rp=boot.Rp, BEG.l = BEG.l, BEG.m = BEG.m, BEG.u=BEG.u)
  return(out)
})

b.RApg <- reactive({
  rpg <- input$r1p/100
  boot.s <- Y.boot()$boot.s 
  boot.Rp <- b.RAg()$boot.Rp
  # Find Intersections 
  b.p <- boot.s[sign(boot.Rp-rpg)==1]
  BEG.p <- c(min(b.p),max(b.p))
  out <- BEG.p
  return(out)
})

#-----------------------------------------------------------------------
#  Panel 3: SR Smsy based Escapement Goal  
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#  5.0 Bootstrap Smsy Anaylses Results Out    
#-----------------------------------------------------------------------
#  Calculate Smsy Optimum profile and escapement goal 
 b.SA <- reactive({
   mp.1 <- input$p1/100
   mp.2 <- input$p2/100
   ap <- input$p3/100
   bn <- isolate(input$bn)
   boot.s <- Y.boot()$boot.s 
   boot.Y <- as.matrix(Y.boot()$Y.boot)
   bMSY <- boot()$MSY
   boot.Smsy.1 <- ifelse(boot.Y > mp.1*bMSY,1,0)
   boot.Smsy.2 <- ifelse(boot.Y > mp.2*bMSY,1,0)
   Y.prob.1 <- colSums(boot.Smsy.1)/bn
   Y.prob.2 <- colSums(boot.Smsy.2)/bn
   # Find Smsy Profile Intersections 
   b.1 <- boot.s[sign(Y.prob.1-ap)==1]
   b.2 <- boot.s[sign(Y.prob.2-ap)==1]
   BEG.1 <- c(min(b.1),max(b.1))
   BEG.2 <- c(min(b.2),max(b.2))
   out <- list(boot.s = boot.s, Y.prob.1 = Y.prob.1, Y.prob.2 = Y.prob.2,
               BEG.1 = BEG.1, BEG.2 = BEG.2)
   return(out)   
 })

# Smsy Goal BEG Out
 SA.BEG <- reactive({
   mp.1 <- input$p1/100
   mp.2 <- input$p2/100
   u <- as.numeric(input$ui)
   BEG.1 <- u*round(b.SA()$BEG.1/u)
   BEG.2 <- u*round(b.SA()$BEG.2/u)
   t.BEG.1 <- paste(paste0(input$p1,'% MSY'),BEG.1[1],'-',BEG.1[2])
   t.BEG.2 <- paste(paste0(input$p2,'% MSY'),BEG.2[1],'-',BEG.2[2])
   out <- list(t1=t.BEG.1,t2=t.BEG.2)
   return(out)
 })
 
#-----------------------------------------------------------------------
#  Panel 3.2: SR Smax based Escapement Goal  
#-----------------------------------------------------------------------
#  Calculate Smax Profile and escapement goal 
b.SMX <- reactive({
   mp <- input$sp/100
   ap <- input$sp1/100
   bn <- isolate(input$bn)
   boot.s <- Y.boot()$boot.s 
   boot.R <- as.matrix(Y.boot()$R.boot)
   bRmax <- boot()$Rmax
   boot.Smax <- ifelse(boot.R > mp*bRmax,1,0)
   R.prob <- colSums(boot.Smax)/bn
   # Find Smsy Profile Intersections 
   b <- boot.s[sign(R.prob-ap)==1]
   BEG <- c(min(b),max(b))
   out <- list(boot.s = boot.s, R.prob = R.prob,
               BEG = BEG)
   return(out)   
 })
 
 # Smsy Goal BEG Out
SM.BEG <- reactive({
   u <- as.numeric(input$ui)
   BEG <- u*round(b.SMX()$BEG/u)
   t.BEG <- paste(paste0('Smax Goal Range'),BEG[1],'-',BEG[2])
   out <- list(t1=t.BEG)
   return(out)
 })
 
 
b.SX <- reactive({
# Read % range    
   mp <- input$sp/100
   lp <- (1 - mp)/2
   up <- 1 - lp
# Read bootstrap Smax 
   bsmax <- boot()$Smax
# Determine Smax Goal Range    
   lg <- quantile(bsmax,lp)
   ug <- quantile(bsmax,up)
   rsmax <- c(lg,ug)
# Read bootstrap input data    
   boot.s <- Y.boot()$boot.s 
# Set Smax range column  
   grange <- as.numeric((sign(boot.s-lg)==1)&(sign(ug-boot.s)==1))
# Read Rboot  
   boot.R <- as.matrix(Y.boot()$R.boot)
# Select Columns meeting range     
   boot.Rs <- boot.R[,grange==1] 
# Vectoize    
   bR <-  c(boot.Rs)
   out <- list(bR = bR, rsmax = rsmax)
   return(out)   
 })
 
# Smsy Goal BEG Out
Sx.BEG <- reactive({
   u <- as.numeric(input$ui)
   BEG.1 <- u*round(b.SX()$rsmax/u)
   out <- paste('Smax Range',BEG.1[1],'-',BEG.1[2])
   return(out)
 })
 
#-----------------------------------------------------------------------
#  7.0 Bootstrap Escapement Goal based Analneyses   
#-----------------------------------------------------------------------
b.GA <- eventReactive(input$Run,{
#-----------------------------------------------------------------------  
  progress <- Progress$new(session, min=1, max=15)
  on.exit(progress$close())
  progress$set(message = 'Calculation in progress',
               detail = 'This may take a while...')
  for (i in 1:15) {
    progress$set(value = i)
    Sys.sleep(0.5)}
#-----------------------------------------------------------------------   
   lg <- input$lg
   ug <- input$ug
   boot.n <- dim(boot())[1]   
   boot.s <- seq(lg,ug,length.out=100)
   boot.R <- boot()
   boot.Recruit <- t(boot.s*t(exp(boot.R[,'ln.alpha']-boot.R[,'beta']%o%boot.s)))
   boot.Yield <- t(t(boot.Recruit)-boot.s)
   out <- list(S = boot.s, R = boot.Recruit, Y = boot.Yield)
   return(out) 
   })
 
b.GA.t <- reactive({
  Return <- c(b.GA()$R)
  Yields <- c(b.GA()$Y)
  out <- as.data.frame(cbind(Return,Yields))
  }) 


#---------------------------------------------------------------------------
#  bootstrap Base Yield  Plot
#---------------------------------------------------------------------------
boot.Yldp <- reactive({
  mp <- input$p.i
  u <- as.numeric(input$ui)
  mult <- mult()
  boot.s <- b.YA()$boot.s/u
  boot.Yu <- b.YA()$boot.Yu/u
  boot.Yl <- b.YA()$boot.Yl/u
  boot.Ym <- b.YA()$boot.Ym/u
  plot(boot.s,boot.Ym,type='l', bty='l',ylim=c(0,max(boot.Yu)),
       ylab=paste('Expected Mean Yield',mult),xlab=paste("Escapement",mult)) 
  lines(boot.s,boot.Yu,lty=2)
  lines(boot.s,boot.Yl,lty=2)
  legend('topright',paste(mp,'% CI'),lty=2,box.lty=0)    
  out <-recordPlot()
  return(out)
})

#---------------------------------------------------------------------------
#  bootstrap Base Run Plot
#---------------------------------------------------------------------------
boot.Recp <- reactive({
  u <- as.numeric(input$ui)
  mp <- input$p.i
  mult <- mult()
  boot.s <- b.YA()$boot.s/u
  boot.Ru <- b.YA()$boot.Ru/u
  boot.Rl <- b.YA()$boot.Rl/u
  boot.Rm <- b.YA()$boot.Rm/u
  plot(boot.s,boot.Rm,type='l', bty='l',ylim=c(0,max(boot.Ru)),
       xlab=paste("Escapement",mult),ylab=paste('Expected Mean Recruit',mult)) 
  lines(boot.s,boot.Ru,lty=2)
  lines(boot.s,boot.Rl,lty=2)
  legend('topright',paste(mp,'% CI'),lty=2,box.lty=0)   
  abline(0,1,col=2)
  out <-recordPlot()
  return(out)
})

#=======================================================================
#  Plots and Tables Outputs  
#======================================================================= 

#=======================================================================
# Panel 1: SR Analyses SEction 
#=======================================================================
#-----------------------------------------------------------------------
#  SR plot 
#-----------------------------------------------------------------------
output$p <- renderPlot({
  u <- as.numeric(input$ui)
  x <- sr.data()
  par <-SR.out()
  xp <- x/u
  SRp <- SRp()[,1:4]/u
# Draw Base SR Plot
  replayPlot(base.p())
# Add CI    
  if(input$show.int==TRUE){
    lines(upr~s,data=SRp,col=1,lw=1,lty=4)
    lines(lwr~s,data=SRp,col=1,lw=1,lty=4)
  }
# Add Years
  if(input$show.points==TRUE) {
    text(R~S,data=xp, labels=x$Yr, cex= 1, pos=3,col=4)}

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
  legend('topright',c(t1,t2),lty=c(l1,l2),box.lty=0)    
})

#-----------------------------------------------------------------------
#  SR Yield plot 
#-----------------------------------------------------------------------
output$py <- renderPlot({
  u <- as.numeric(input$ui)
  x <- sr.data()
  par <-SR.out()
  SRp <- SRp()[,1:4]/u
  xp <- x/u
# Plot base Yiled plot
  replayPlot(base.py())
# Add CI    
  if(input$show.int==TRUE){
    lines((upr-s)~s,data=SRp,col=1,lw=1,lty=4)
    lines((lwr-s)~s,data=SRp,col=1,lw=1,lty=4)
   }
# Add Years
  if(input$show.points==TRUE) {
    text((R-S)~S,data=xp, labels=x$Yr, cex= 1, pos=3,col=4)}
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

#-----------------------------------------------------------------------
#  Plot time serise
#-----------------------------------------------------------------------  
output$srt <- renderPlot({
  x <- sr.data()
  u <- as.numeric(input$ui)
  mult <- mult()
  plot(R/u~Yr,data=x,type='l', bty='l',ylim=c(0,with(x,max(R,S)/u)),
       main=input$caption,
       xlab='Year',
       ylab=paste('Spawner / Recruit',mult))
  lines(S/u~Yr,data=x,lty=2)
  legend('topright',c('Spawner','Recruit'),lty=c(2,1),box.lty=0)  
})

#-----------------------------------------------------------------------
#  Plot Residual Plot
#-----------------------------------------------------------------------  
output$Resid <- renderPlot({
    year <- data()[,input$Yr]
    resid <-residuals(SR())
    plot(resid~year, bty='l',xlab='Year',ylab='Residuals')
    abline(h=0)
    lines(smooth.spline(resid~year),col=4)
})
 
#-----------------------------------------------------------------------
#  SR Bootstrap Parameters Distribution   
#-----------------------------------------------------------------------
# Bootstrap summary 
output$bsummary <- renderPrint({
  print(summary(boot()[,c(4,2,1,3,5:10)]),digits=c(3,10,3,0,0,0,0,0,0))
})

output$bquantile <- renderPrint({
  print(apply(boot()[,c(4,2,1,3,5:10)],2,function(x) quantile(x,probs=c(0.025,0.975),na.rm=TRUE)),digits=c(3,10,3,3,0,0,0,0,0,0))
})

output$bhist <- renderPlot({
  par(mfrow=c(3,3),mar = c(1.75,1.5,1.5,1.75))
  plot(density(boot()$alpha), bty='l',main='Ricker alpha',xlab='',ylab='')
  plot(density(boot()$beta),bty='l',main='Ricker beta',xlab='',ylab='')
  plot(density(boot()$Seq), bty='l',main='SEQ',xlab='',ylab='')
  plot(density(boot()$Smsy),bty='l',main='Smsy',xlab='',ylab='')
  plot(density(boot()$RSmsy), bty='l',main='R Smsy',xlab='',ylab='')
  plot(density(boot()$MSY), bty='l', main='MSY',xlab='',ylab='')
  plot(density(boot()$Smax), bty='l', main='Smax',xlab='',ylab='')
  plot(density(boot()$Rmax), bty='l', main='Rmax',xlab='',ylab='')
  })

#=======================================================================
# Panel 2: Smsy Analyses SEction 
#=======================================================================
#-----------------------------------------------------------------------
#  Smsy Optimum Profile Plot  
#-----------------------------------------------------------------------
output$bsmsy <- renderPlot({
  mp.1 <- input$p1/100
  mp.2 <- input$p2/100
  ap <- input$p3/100
  u <- as.numeric(input$ui)
  mult <- mult()
  bn <- isolate(input$bn)
  boot.s <- b.SA()$boot.s/u
  Y.prob.1 <- b.SA()$Y.prob.1
  Y.prob.2 <- b.SA()$Y.prob.2
#---------------------------------------------------------------------------
  plot(boot.s,Y.prob.1,type='l', bty='l', col=3, ylim=c(0,1),ylab = 'Probability',
       xlab=paste('Escapement',mult),main=paste0('MSY Yield probability curve')) 
  lines(boot.s,Y.prob.2,lty = 4,col=4)
    abline(h = ap,lwd=2,col=2)
  legend('topright',c(paste(input$p1,'% MSY'),paste(input$p2,'% MSY')),lty=c(1,4),
         col=c(3,4),box.lty=0)  
})  

#------------------------------------------------------------------------
#  Escapement goal table output 
#------------------------------------------------------------------------
output$bsmsyt <-renderText({
    paste(SA.BEG()$t1,SA.BEG()$t2,sep='\n')
})

#---------------------------------------------------------------------------
#  Smsy Yield and Recruit Plot
#---------------------------------------------------------------------------
output$bsmsy.y <- renderPlot({
  u <- as.numeric(input$ui)
  BEG.1 <- b.SA()$BEG.1/u
  BEG.2 <- b.SA()$BEG.2/u
# Plot base Yield Plot
  replayPlot(boot.Yldp())
# Plot escapement goal range 
  abline(v=BEG.1,lty=1,col=3)
  abline(v=BEG.2,lty=4,col=4)
 })

output$bsmsy.r <- renderPlot({
  u <- as.numeric(input$ui)
  BEG.1 <- b.SA()$BEG.1/u
  BEG.2 <- b.SA()$BEG.2/u
# Plot base recruit Plot  
  replayPlot(boot.Recp())
# Plot escapement goal range 
  abline(v=BEG.1,lty=1,col=3)
  abline(v=BEG.2,lty=4,col=4)
})


#=======================================================================
#  Panel 2: Yield Goal Analyses 
#=======================================================================
# Yield Plot 
output$byield <- renderPlot({
   u <- as.numeric(input$ui)
   yg <- input$y1/u
   replayPlot(boot.Yldp())
   abline(h=yg,lwd=2,col=2)
 }) 

# Print Escapement Goal Range
output$byt <-renderText({
     mp <- input$p.i
     u <- as.numeric(input$ui)
     BEG.l <- u*round(b.YAg()$BEG.l/u)
     BEG.m <- u*round(b.YAg()$BEG.m/u)
     BEG.u <- u*round(b.YAg()$BEG.u/u)
     t.BEG.l <- paste('Lower',mp,'% Limit',BEG.l[1],'-',BEG.l[2])
     t.BEG.m <- paste('Mean              ',BEG.m[1],'-',BEG.m[2])
     t.BEG.u <- paste('Upper',mp,'% Limit',BEG.u[1],'-',BEG.u[2])
     paste(t.BEG.l,t.BEG.m,t.BEG.u,sep='\n')
   })
# Optimum Yield Proflie Plot 
output$byp <- renderPlot({
   u <- as.numeric(input$ui)
   mult <- mult()
   yg <- input$y1
   ypg <- input$y1p/100
   boot.s <- b.YA()$boot.s/u
   boot.Yp <- b.YAg()$boot.Yp
   s <- SRp()$s/u
   Yci <- SRp()$prof.Yci
   plot(boot.s,boot.Yp,type='l', bty='l',ylim=c(0,1),ylab = 'Probability',xlab=paste("Escapement",mult),
        main=paste('Minimum',yg,'Yield probability plot')) 
   lines(s,Yci,col='grey')
   abline(h = ypg,lwd=2,col=2)
  })  
# Print Optimum Yield Proflie Goal Range  
output$bypt <-renderText({
   u <- as.numeric(input$ui)
   BEG.p <- u*round(b.YApg()/u)
   paste('Escapement Goal Range:',BEG.p[1],'-',BEG.p[2])
 })
 
# Recruit Plot 
output$breturn <- renderPlot({
   u <- as.numeric(input$ui)
   rg <- input$r1/u
# Plot Base Recruit Plot
   replayPlot(boot.Recp())
   abline(h=rg,lwd=2,col=2)
 }) 

# Print Base Recruit Goal  
output$brt <-renderText({
   u <- as.numeric(input$ui)
   mp <- input$p.i
   BEG.l <- u*round(b.RAg()$BEG.l/u)
   BEG.m <- u*round(b.RAg()$BEG.m/u)
   BEG.u <- u*round(b.RAg()$BEG.u/u)
   t.BEG.l <- paste('Lower',mp,'% Limit',BEG.l[1],'-',BEG.l[2])
   t.BEG.m <- paste('Mean              ',BEG.m[1],'-',BEG.m[2])
   t.BEG.u <- paste('Upper',mp,'% Limit',BEG.u[1],'-',BEG.u[2])
   paste(t.BEG.l,t.BEG.m,t.BEG.u,sep='\n')
 })

#  Minimum Recruit Proflie Plot 
output$brp <- renderPlot({
   rg <- input$r1
   rpg <- input$r1p/100
   u <- as.numeric(input$ui)
   mult <- mult()
   boot.s <- b.YA()$boot.s/u
   boot.Yp <- b.RAg()$boot.Rp
   plot(boot.s,boot.Yp,type='l', bty='l',ylim=c(0,1),ylab = 'Probability',xlab=paste("Escapement",mult),
        main=paste('Minimum',rg,'Recruit probability Plot')) 
   abline(h = rpg,lwd=2,col=2)
 })  

# Optimum Recruit Proflie Escapement Goal 
output$brpt <-renderText({
   u <- as.numeric(input$ui)
   BEG.p <- u*round(b.RApg()/u)
   paste('Escapement Goal Range:',BEG.p[1],'-',BEG.p[2])
 })
 
#=======================================================================
#  Panel 3: Smax Goal Analyses      
#=======================================================================
#-----------------------------------------------------------------------
#  Smax Profile Plot  
#-----------------------------------------------------------------------
output$bsmax1 <- renderPlot({
  mp <- input$sp/100
  ap <- input$sp1/100
  u <- as.numeric(input$ui)
  mult <- mult()
  bn <- isolate(input$bn)
  boot.s <- b.SMX()$boot.s/u
  R.prob <- b.SMX()$R.prob
  #---------------------------------------------------------------------------
  plot(boot.s,R.prob,type='l', bty='l', col=4, ylim=c(0,1),ylab = 'Probability',
       xlab=paste('Escapement',mult),main=paste0('Rmax probability curve')) 
  abline(h = ap,lwd=2,col=2)
  legend('topright',c(paste(input$sp,'% Rmax')),lty=c(1),
         col=c(4),box.lty=0)  
})  

#------------------------------------------------------------------------
#  Escapement goal table output 
#------------------------------------------------------------------------
output$bsmaxt1 <-renderText({
  paste(SM.BEG()$t,sep='\n')
})

#-----------------------------------------------------------------------
#  Recruit plot with Smax Goal Range
#-----------------------------------------------------------------------
output$bsmax.r <- renderPlot({
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
output$bsmax <- renderPlot({
  u <- as.numeric(input$ui)
  BEG.1 <- b.SMX()$BEG/u
  # Plot base Yield Plot
  replayPlot(boot.Yldp())
  # Plot escapement goal range 
  abline(v=BEG.1,col=4)}) 



#=======================================================================
#  Panel 4: User Defined Escapement Goal Range Analyses      
#=======================================================================
#-----------------------------------------------------------------------
#  Plot distribution of Recruit and Yield at Given Escapement Range
#-----------------------------------------------------------------------
output$bGAf <- renderPlot({
  par(mfrow=c(1,2))
  u <- as.numeric(input$ui)
  mult <- mult()
  rg <- input$rg/u
  yg <- input$yg/u
  plot(density(b.GA.t()$R/u),  bty='l',main='Expected Mean Recruit',xlab=paste("Recruit",mult),ylab='')
     lines(density(SRp.G()$bRci/u), col = 'grey')
     abline(v=rg,lty=2,col=2)
  plot(density(b.GA.t()$Y/u),  bty='l',main='Expected Mean Yields',xlab=paste("Yield",mult),ylab='')
     abline(v=yg,lty=2,col=2)
     lines(density(SRp.G()$bYci/u), col = 'grey')
   }) 

output$bGAs <- renderPrint({
   print(summary(b.GA.t()),digits=0)
 })

# Calculate Probability meeting target  
output$bGAt <- renderText({
     rg <- input$rg
     yg <- input$yg
     prg <- sum(ifelse(b.GA.t()$Return>rg,1,0))/length(b.GA.t()$Return)
     pyg <- sum(ifelse(b.GA.t()$Yields>yg,1,0))/length(b.GA.t()$Yields)
     t.prg <- paste('Meeting Target Recruit:',round(100*prg,0),'%')
     t.pyg <- paste('Meeting Target Yields:',round(100*pyg,0),'%')
     paste(t.prg,t.pyg,sep='\n')
 })
 

output$bGAfp <- renderPlot({
   par(mfrow=c(1,2))
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

#-----------------------------------------------------------------------
#  Plot distribution of Recruit and Yield at Given Escapement Range
#  SR model based CI and PI
#-----------------------------------------------------------------------
output$bGASR <- renderPlot({
  par(mfrow=c(1,2))
  u <- as.numeric(input$ui)
  mult <- mult()
  rg <- input$rg/u
  yg <- input$yg/u
  plot(density(SRp.G()$bRpi/u), bty='l',main='Expected Annual Recruit',xlab=paste("Recruit",mult),ylab='')
  abline(v=rg,lty=2,col=2)
  plot(density(SRp.G()$bYpi/u),bty='l',main='Expected Annual Yields',xlab=paste("Yield",mult),ylab='')
  abline(v=yg,lty=2,col=2)
}) 
  
output$bGASRs <- renderPrint({
  dat <- SRp.G()
  dat <- dat[,c(3,4)]
  names(dat) <- c('Recruit','Yields')
  print(summary(dat),digits=0)
})

# Calculate Probability meeting target  
output$bGASRt <- renderText({
  rg <- input$rg
  yg <- input$yg
  prg <- sum(ifelse(SRp.G()$bRci>rg,1,0))/length(SRp.G()$bRci)
  pyg <- sum(ifelse(SRp.G()$bYci>yg,1,0))/length(SRp.G()$bYci)
  t.prg <- paste('Meeting Target Recruit CI:',round(100*prg,0),'%')
  t.pyg <- paste('Meeting Target Yields CI:',round(100*pyg,0),'%')
  prg1 <- sum(ifelse(SRp.G()$bRpi>rg,1,0))/length(SRp.G()$bRpi)
  pyg1 <- sum(ifelse(SRp.G()$bYpi>yg,1,0))/length(SRp.G()$bYpi)
  t.prg1 <- paste('Meeting Target Recruit PI:',round(100*prg1,0),'%')
  t.pyg1 <- paste('Meeting Target Yields PI:',round(100*pyg1,0),'%')
  paste(t.prg,t.pyg,t.prg1,t.pyg1,sep='\n')
})

# Optimum Recruit Proflie Plot 
output$SRrp <- renderPlot({
  par(mfrow=c(1,2))
  rg <- input$rg
  yg <- input$yg
  u <- as.numeric(input$ui)
  mult <- mult()
  s <- SRp()$s/u
  Rci <- SRp()$prof.Rci
  Rpi <- SRp()$prof.Rpi
  Yci <- SRp()$prof.Yci
  Ypi <- SRp()$prof.Ypi
  plot(s,Rci,type='l', bty='l',ylim=c(0,1),ylab = 'Probability',xlab=paste("Escapement",mult),
       main=paste('Minimum',rg,'Recruit probability Plot')) 
  lines(s,Rpi,lty=2)
  plot(s,Yci,type='l', bty='l',ylim=c(0,1),ylab = 'Probability',xlab=paste("Escapement",mult),
       main=paste('Minimum',yg,'Yield probability Plot')) 
  lines(s,Ypi,lty=2)
})  


})  #End Server 


# Create Shiny app ----
shinyApp(ui, server)
