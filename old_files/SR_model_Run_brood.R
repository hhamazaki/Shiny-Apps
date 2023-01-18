#initialize
library(shiny)
library(shinythemes)
library(datasets)
library(lmtest)
library(reshape2)
library(mgcv)
library(MCMCpack)
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
  fileInput("file1", "Choose csv / tab File",
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
    p("Set First age of run"),
# Input: Select what to display
    numericInput("fage", "First Age", value=4,min=1,max=20,step=1),
    # Button
    p("Download Brood table"),
    downloadButton("downloadData", "Download"),
    # Horizontal line ----
    tags$hr()
   ), # End SidePanel
  # output
  mainPanel(
    h3(textOutput("caption")),
    tabsetPanel(
      tabPanel("Run Table",dataTableOutput("table")),
      tabPanel("Brood Table",dataTableOutput("btable")),
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
        p("Escapement Goal Range"),
        numericInput(inputId='egl','Lower Goal',value=0,min=0), 
        numericInput(inputId='egu','Upper Goal',value=0,min=0), 
        checkboxInput(inputId="show.eg", "Show Escapement Goal", TRUE),
        checkboxInput(inputId="show.points", "show Years", TRUE), 
        checkboxInput(inputId="show.smsy", "show Smsy", TRUE),
        checkboxInput(inputId="show.smax", "show Smax", TRUE),
        checkboxInput(inputId="show.int", "show Interval", TRUE),
        numericInput(inputId="p.i", "% Interval", value=90,min=0,max=100,step=5),
        selectInput(inputId="Li","Interval Type", choices = c('confidence','prediction')),
        numericInput("bn", "Number bootstrap replicates", value=10000,min=1000,step=1000)
           ),  # End sidebarPanel
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
              plotOutput("srt"),
              plotOutput('runesc')
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
                               ), #End Sidepar Panel
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
                            ), #End tab Panel
              tabPanel("Expected Annual Recruit & Yields",
                       plotOutput("bGASR"),
                       splitLayout(cellWidths = c("50%", "50%"),
                                   p(strong("Recruit and Yields Summary")),
                                   p(strong("Probability of Meeting Target"))),
                       splitLayout(cellWidths = c("50%", "50%"),
                                   verbatimTextOutput("bGASRs"),
                                   verbatimTextOutput("bGASRt"))
                      ) #End tab Panel
            )#End tabsetPanel
          )#End main Panel 
        )#End tabPanel
      ),#End nabVarMenu
#-----------------------------------------------------------------------
#  Panel 4  Management Strategy Evaluation 
#-----------------------------------------------------------------------
navbarMenu("MSE Analyses",
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
    mainPanel(tabsetPanel(
           tabPanel("Simulation Run",
                    fluidRow(  
                      p(("To compare outcomes of the same startegy, reapeat Simulation and Simuulate")),
                      p(("To compare outcomes of different startegy, change strategis and click Simulate")),
                      p(("To Clear Results, click Clear"))
                       ),
                    fluidRow( 
                      column(3, actionButton("InitRun","Initialize")),
                      column(3, actionButton("SimRun","Simulate")),
                      column(3, actionButton("SimClear","Clear Results"))
                        ),
                    fluidRow(  
                    plotOutput(height='500px',"runsim"),
                    verbatimTextOutput("simsum"),
                    plotOutput("simhist")
                            )
                                ),
           tabPanel("Sim Summary",
                    plotOutput('altsim.H'),
                    verbatimTextOutput("altsim.sum")
                    ),
           tabPanel("Sim time series",
                   plotOutput(height='600px',"altsim.N"), 
                   downloadButton("simdownload", "Download")
                 ),
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
              sliderInput(inputId="sobsE", "Escapement Observation %E", value=30,min=0,max=100,step=5)
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
        ),#End tabPanel
    tabPanel("Model Description",
          tabsetPanel(
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
data <- reactive({
    req(input$file1)
    inFile <- input$file1
    #could also store in a reactiveValues
    df <- read.csv(inFile$datapath,
             header = input$header,
             sep = input$sep)
    return(df)
  })  
# Create brood table 
brood.table <- reactive({
    x <- data()
    fage <- input$fage
    nages <- dim(x)[2]-3
    lage <- fage+nages-1
    yr <- c(min(x[,1])-seq(lage,1),x[,1])
    brood <- matrix(0,ncol=nages+2,nrow = length(yr))
    brood[,1] <- yr
    brood[,2] <- c(rep(NA,lage),x[,2])
    for(i in 1:nages){
        brood[,i+2] <- c(rep(NA,lage-fage+1-i),x[,3+i]*x[,3],rep(NA,fage+i-1))
      }
    brood.c <- data.frame(brood)
    names(brood.c) <- c('b.Year','Spawner',paste0('b.Age',seq(fage,lage)))
    brood.c$Recruit <- rowSums(brood.c[,-c(1:2)])
    return(brood.c)
  })

# Download data  ----
output$downloadData <- downloadHandler(
    filename = function() {"broodtable.csv"},
    content = function(file) {
    write.csv(brood.table(), file, row.names = FALSE)
  })

# Create SR data  
sr.data <- reactive({
   x <- brood.table()
   x <- x[complete.cases(x),c(1,2,dim(x)[2])]
   names(x) <- c('Yr','S','R')
   return(x)   
  }) 
  
# Data output display
output$table <- renderDataTable(
    {
        data()
    })

# Brood table display
output$btable <- renderDataTable(
  {
    brood.table()
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
  hist(x$R,,main='',xlab='Recruit')
 })

#-----------------------------------------------------------------------
#  Panel 2: SR Data Analyses and Output  
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#  1.0: SR Model   
#-----------------------------------------------------------------------
SR <- reactive({
    x <- sr.data()
    model <- lm(log(R/S)~S,data=x)
    return(model)
  })  

#-----------------------------------------------------------------------
#  2.0: SR Parameters
#-----------------------------------------------------------------------
SR.out <- reactive({
  ln.alpha <- coef(SR())[1]
  alpha <- exp(ln.alpha)
  beta <- -coef(SR())[2]
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
  pred <- predict(SR(), newdata=data.frame(S=s), se.fit = TRUE, interval=input$Li,level = mp)
# Predicted ln(R/S)
  lRS <- data.frame(pred$fit)
# Predicted R)
  ER <- exp(lRS)*s
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
  s <- seq(lg,ug,length.out=101)
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
  boot.t <- matrix(0,nrow=101,ncol=1000)
  for (i in 1:101){
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
# Coloer Scheme   
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
    ## Save the color
    # invisible(t.col)
    return(t.col)
  }
  
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
    par(xaxs='i',yaxs='i',bty='l')
    plot(R~S,data=xp,pch=19,col=1, 
         main= input$caption,
         xlab=paste("Escapement",mult),ylab=paste('Recruit',mult),
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
    par(xaxs='i',yaxs='i',bty='l')
    plot((R-S)~S,data=xp,pch=19,col=1, 
         main= input$caption,
         xlab=paste("Escapement",mult),ylab=paste('Yield',mult),
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
 boot <- reactive({
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
    boot.R$Rmsy <- with(boot.R,Smsy*exp(ln.alpha-beta*Smsy))
    boot.R$MSY <- with(boot.R,Rmsy-Smsy)
    boot.R$Smax <- with(boot.R,1/beta)
    boot.R$Rmax <- with(boot.R,exp(ln.alpha-1)/beta)
# Remove bad data (i.e. beta is neagative)     
    boot.R[boot.R[,2]<=0,] <- NA  
    out <- boot.R[complete.cases(boot.R),]
    return(out)
  })
#-----------------------------------------------------------------------
#  2.0 Bootstrap Recruit and Yields Data Out
#-----------------------------------------------------------------------
Y.boot <- reactive({
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
  boot.s <- seq(0,Seq,length.out=101)
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
   boot.s <- seq(lg,ug,length.out=101)
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
  par(xaxs='i',yaxs='i',bty='l')
  plot(boot.s,boot.Ym,type='l',ylim=c(0,max(boot.Yu)),
       ylab=paste('Expected Mean Yield',mult),xlab=paste("Escapement",mult)) 
  polygon(c(boot.s,rev(boot.s)),c(boot.Yu,rev(boot.Yl)),col=tcol('grey',50),border=NA)
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
  par(xaxs='i',yaxs='i',bty='l')
  plot(boot.s,boot.Rm,type='l',ylim=c(0,max(boot.Ru)),
       xlab=paste("Escapement",mult),ylab=paste('Expected Mean Recruit',mult)) 
  polygon(c(boot.s,rev(boot.s)),c(boot.Ru,rev(boot.Rl)),col=tcol('grey',50),border=NA)
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
  with(SRp,polygon(c(s,rev(s)),c(upr,rev(lwr)),col=tcol('grey',50),border=NA))
  }
# Add Years
  if(input$show.points==TRUE) {
    text(R~S,data=xp, labels=x$Yr, cex= 1, pos=3,col=4)}
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
  with(SRp,polygon(c(s,rev(s)),c(upr-s,rev(lwr-s)),col=tcol('grey',50),border=NA))
   }
# Add Years
  if(input$show.points==TRUE) {
    text((R-S)~S,data=xp, labels=x$Yr, cex= 1, pos=3,col=4)}
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

#-----------------------------------------------------------------------
#  Plot time serise
#-----------------------------------------------------------------------  
output$srt <- renderPlot({
  x <- sr.data()
  u <- as.numeric(input$ui)
  mult <- mult()
  par(xaxs='i',yaxs='i',bty='l')
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
})

output$runesc <- renderPlot({
  x <- data()[,c(1:3)]
  names(x) <-c('Yr','S','R')
  u <- as.numeric(input$ui)
  mult <- mult()
  par(xaxs='i',yaxs='i',bty='l')
  plot(R/u~Yr,data=x,type='l',ylim=c(0,with(x,max(R,S)/u)),
       main=input$caption,
       xlab='Year',
       ylab=paste('Run / Escapement',mult))
  lines(S/u~Yr,data=x,lty=2)
  # Add Escapement Goal range  
  if(input$show.eg==TRUE) {
    with(x,polygon(c(Yr,rev(Yr)),c(rep(input$egl/u,length(Yr)),rev(rep(input$egu/u,length(Yr)))),col=tcol('grey',50),border=NA))
  }
  legend('topright',c('Run','Escapement'),lty=c(1,2),box.lty=0)  
})


#-----------------------------------------------------------------------
#  Plot Residual Plot
#-----------------------------------------------------------------------  
output$Resid <- renderPlot({
    year <- sr.data()$Yr
    resid <-residuals(SR())
    par(xaxs='i',yaxs='i',bty='l')
    plot(resid~year,xlab='Year',ylab='Residuals')
    abline(h=0)
    model <- gam(resid~s(year),family=gaussian, fit =TRUE)
    pred.year <- data.frame(year, predict.gam(model,se = TRUE))    
    lines(pred.year$year, pred.year$fit,lwd=2,col=4)
    pred.year$ciu <- pred.year$fit + 2*pred.year$se.fit
    pred.year$cil <- pred.year$fit - 2*pred.year$se.fit
    with(pred.year,polygon(c(year,rev(year)),c(ciu,rev(cil)),col=tcol('grey',50),border=NA))
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
  par(mfrow=c(3,3),mar = c(1.75,1.5,1.5,1.75),xaxs='i',yaxs='i',bty='l')
  plot(density(boot()$alpha),main='Ricker alpha',xlab='',ylab='')
  plot(density(boot()$beta),main='Ricker beta',xlab='',ylab='')
  plot(density(boot()$Seq), main='SEQ',xlab='',ylab='',xlim=c(0,quantile(boot()$Seq,0.99)))
  plot(density(boot()$Smsy),main='Smsy',xlab='',ylab='',xlim=c(0,quantile(boot()$Smsy,0.99)))
  plot(density(boot()$RSmsy), main='R Smsy',xlab='',ylab='',xlim=c(0,quantile(boot()$RSmsy,0.99)))
  plot(density(boot()$MSY), main='MSY',xlab='',ylab='',xlim=c(0,quantile(boot()$MSY,0.99)))
  plot(density(boot()$Smax), main='Smax',xlab='',ylab='',xlim=c(0,quantile(boot()$Smax,0.99)))
  plot(density(boot()$Rmax), main='Rmax',xlab='',ylab='',xlim=c(0,quantile(boot()$Rmax,0.99)))
  
  })

#=======================================================================
# Panel 2: Smsy Analyses Section 
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
  par(xaxs='i',yaxs='i',bty='l')
  plot(boot.s,Y.prob.1,type='l',col=3, ylim=c(0,1),ylab = 'Probability',
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
#  Panel 3: Tab 2: Yield Goal Analyses 
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
   par(xaxs='i',yaxs='i',bty='l')
   plot(boot.s,boot.Yp,type='l',ylim=c(0,1),ylab = 'Probability',xlab=paste("Escapement",mult),
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
   par(xaxs='i',yaxs='i',bty='l')
   plot(boot.s,boot.Yp,type='l',ylim=c(0,1),ylab = 'Probability',xlab=paste("Escapement",mult),
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
#  Panel 3  Tab 3: Smax Goal Analyses      
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
  par(xaxs='i',yaxs='i',bty='l')
  plot(boot.s,R.prob,type='l', col=4, ylim=c(0,1),ylab = 'Probability',
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
# Panel 3: Tab 4: User Defined Escapement Goal Range Analyses      
#=======================================================================
#-----------------------------------------------------------------------
#  Plot distribution of Recruit and Yield at Given Escapement Range
#-----------------------------------------------------------------------
output$bGAf <- renderPlot({
  par(mfrow=c(1,2),xaxs='i',yaxs='i',bty='l')
  u <- as.numeric(input$ui)
  mult <- mult()
  rg <- input$rg/u
  yg <- input$yg/u
  
  plot(density(b.GA.t()$R/u),main='Expected Mean Recruit',xlab=paste("Recruit",mult),ylab='')
     lines(density(SRp.G()$bRci/u), col = 'grey')
     abline(v=rg,lty=2,col=2)
  plot(density(b.GA.t()$Y/u),main='Expected Mean Yields',xlab=paste("Yield",mult),ylab='')
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
  par(mfrow=c(1,2),xaxs='i',yaxs='i',bty='l', cex=1.2)
  u <- as.numeric(input$ui)
  mult <- mult()
  rg <- input$rg/u
  yg <- input$yg/u
  plot(density(SRp.G()$bRpi/u), main='Expected Annual Recruit',xlab=paste("Recruit",mult),ylab='')
  abline(v=rg,lty=2,col=2)
  plot(density(SRp.G()$bYpi/u), main='Expected Annual Yields',xlab=paste("Yield",mult),ylab='')
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
  par(mfrow=c(1,2),xaxs='i',yaxs='i',bty='l')
  rg <- input$rg
  yg <- input$yg
  u <- as.numeric(input$ui)
  mult <- mult()
  s <- SRp()$s/u
  Rci <- SRp()$prof.Rci
  Rpi <- SRp()$prof.Rpi
  Yci <- SRp()$prof.Yci
  Ypi <- SRp()$prof.Ypi
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
#Observed Run by age (Assume age comp est is accurate)
  N.ta.obs[y,] <- sum(S.obs[y],H.obs[y])*(N.ta[y,]/N[y])
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
       main=input$caption,
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

observe({if(input$SimClear == 0) 
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
#output$prtxval <- renderDataTable({(xvals())})

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
                               