#initialize
library(shiny)
library(shinythemes)
library(datasets)
library(lmtest)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(cowplot)
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
      tabPanel("Summary",verbatimTextOutput('summary'))
        )  # End tabsetPanel
      )  # End mainPanel
    ), # End tabanle
#-----------------------------------------------------------------------
#  Panel 2
#-----------------------------------------------------------------------
  tabPanel("SR Analyses",
    sidebarPanel(width = 3,
      textInput(inputId='caption',label='Figue Caption Title',value=''),           
      selectInput(inputId="ui","Dislpay Unit", choices = c(1,100,1000,10000,100000,1000000)),  
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
             tabPanel("Time Series",
                      plotOutput("srt")
                      ),
             tabPanel("Residuals", 
                      plotOutput("Resid"),
                      p(strong("Durbin-Watson Serial Correlation Analyses")), 
                      verbatimTextOutput('dwtest')),
             tabPanel("Bootstrap",
                      verbatimTextOutput('bsummary'),
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
         numericInput("p3", "Min % of Meeting the MSY Goal", value=90,min=0,max=100,step=5)
         ),
         mainPanel(
             tabsetPanel(
              tabPanel("Optimum Profile",
                   plotOutput(height='500px','bsmsy'),
                   verbatimTextOutput("bsmsyt")),
              tabPanel("Yield  & Return Profile",
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
               numericInput("sp", "Select Smax % range", value=80,min=0, step=5)
                  ), 
                mainPanel(
                  splitLayout(cellWidths = c("50%", "50%"),                 
                          plotOutput("bsmax.r"),
                          plotOutput("bsmax")),
                          textOutput("bsmaxt"),
                          p(strong("Return Summary")), 
                          verbatimTextOutput("bsum")
                          )#End mainPanel
          ),#End tabPanel
       tabPanel("Yield & Return Analyses",
         sidebarPanel(width = 3,
         p(strong("Yield Analyses")),            
         numericInput("y1", "Select Yied Goal", value=100000,min=0, step=10000),
         numericInput("y1p", "Select % Goal Achieve", value=90,min=0, max=100,step=5),
         p(strong("Return Analyses")), 
         numericInput("r1", "Select Return Goal", value=100000,min=0, step=10000),
         numericInput("r1p", "Select % Goal Achieve", value=90,min=0, max=100,step=5)
          ), 
         mainPanel(
           tabsetPanel(
             tabPanel("Yield Analyses",
                      splitLayout(cellWidths = c("50%", "50%"),
                      plotOutput(height='500px','byield'),
                      plotOutput(height='500px','byp')),
                      splitLayout(cellWidths = c("50%", "50%"),
                      verbatimTextOutput("byt"),
                      textOutput("bypt"))),
             tabPanel("Return Analyses",
                      splitLayout(cellWidths = c("50%", "50%"),
                      plotOutput(height='500px','breturn'),
                      plotOutput(height='500px','brp')),
                      verbatimTextOutput("brt"),
                      textOutput("brpt"))
             )#End tabsetPanel
            )#EndmaiPanel
          ),#End tabPanel
       tabPanel("Escapment Goal Evaluation",
                  sidebarPanel(width = 3,
                  p(strong("Select Lower and Upper Escapement Goal")),  
                  numericInput("lg", "Lower Goal", value=50000,min=0, step=1000),  
                  numericInput("ug", "Upper Goal", value=100000,min=0, step=1000),
                  actionButton("Run","Run"),
                  # Horizontal line ----
                  tags$hr(),
                  numericInput("rg", "Target Return", value=200000,min=0, step=1000),
                  numericInput("yg", "Target Yield", value=100000,min=0, step=1000)
                               ),
                  mainPanel("Expected Return & Yields",
                            plotOutput("bGAf"),
                            p(strong("Return and Yields Summary")), 
                            verbatimTextOutput("bGAs"),
                            p(strong("Probability of Meeeting Target")), 
                            verbatimTextOutput("bGAt")
                            )
       )#End tabPanel
   )#End nabVarMenu
  )#End nabVarPage
 )#End fluidPage


#=======================================================================    
#  Server:  
#=======================================================================
server<-shinyServer(function(input, output, session){
#-----------------------------------------------------------------------
#  Data upload and output 
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
observe({
    var.opts<-names(data())
    updateSelectInput(session, inputId="Yr", choices = var.opts)
    updateSelectInput(session, inputId="S", choices = var.opts)
    updateSelectInput(session, inputId="R", choices = var.opts)
  })
  
sr.data <- reactive({
   x <- data()[,c(input$Yr,input$S,input$R)]
   names(x) <- c('Yr','S','R')
   return(x)   
  }) 
  
output$table <- renderDataTable(
    {
        data()
    })

output$summary <- renderPrint({
      summary(data())
    })

#=======================================================================
#  SR Data Analyses 
#=======================================================================
  SR <- eventReactive(input$go,{
    x <- sr.data()
    model <- lm(log(R/S)~S,data=x)
    return(model)
  })  
#-----------------------------------------------------------------------
#  SR Parameters output
#-----------------------------------------------------------------------
   SR.out <- reactive({
   ln.alpha <- coef(SR())[1]
   alpha <- exp(ln.alpha)
   beta <- -coef(SR())[2]
   sigma <- sigma(SR())
   ln.alpha.c <- ln.alpha+0.5*(sigma(SR()))^2
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
#  SR Analyses Outputs  
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
  
#=======================================================================
#  Bootstrap Analyses 
#=======================================================================
#-----------------------------------------------------------------------
#  Create Bootstrap Data
#-----------------------------------------------------------------------
 boot <- eventReactive(input$go,{
#-----------------------------------------------------------------------  
    progress <- Progress$new(session, min=1, max=15)
   on.exit(progress$close())
   progress$set(message = 'Calculation in progress',
                detail = 'This may take a while...')
   for (i in 1:15) {
     progress$set(value = i)
     Sys.sleep(0.5)}
#-----------------------------------------------------------------------   
    s <- sr.data()$S
    SRP <-predict(SR())
    SRR <-residuals(SR())
    boot.n <- isolate(input$bn)
    boot.R <- matrix(0,nrow=boot.n,ncol=3)
    colnames(boot.R) <- c('ln.alpha','beta','sigma')
    for (i in 1:boot.n)
    {
      bootR <- sample(SRR,length(SRR),replace = TRUE) + SRP
      SRi <- lm(bootR~s)
      boot.R[i,1] <- SRi$coefficients[1]
      boot.R[i,2] <- -SRi$coefficients[2]
      boot.R[i,3] <- sigma(SRi)
    }
    boot.R <- as.data.frame(boot.R)
    boot.R$alpha <- exp(boot.R$ln.alpha)
    boot.R$Seq <- with(boot.R,ln.alpha/beta)
    boot.R$Smsy <- with(boot.R,Seq*(0.5-0.07*ln.alpha))
    boot.R$RSmsy <- with(boot.R,Smsy*exp(ln.alpha-beta*Smsy))
    boot.R$MSY <- with(boot.R,RSmsy-Smsy)
    boot.R$Smax <- with(boot.R,1/beta)
    boot.R$Rmax <- with(boot.R,exp(ln.alpha-1)/beta)
    boot.R[boot.R[,2]<0,] <- NA  #Remove bad data
    out <- boot.R[complete.cases(boot.R),]
    return(out)
  })
#-----------------------------------------------------------------------
#  Bootstrap Yields Data Out
#-----------------------------------------------------------------------
Y.boot <- eventReactive(input$go,{
  
  progress <- Progress$new(session, min=1, max=15)
  on.exit(progress$close())
  progress$set(message = 'Calculation in progress',
               detail = 'This may take a while...')
  for (i in 1:15) {
    progress$set(value = i)
    Sys.sleep(0.5)}
  
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
#  Bootstrap Return & Yields Anaylses Results Out
#-----------------------------------------------------------------------  
b.YA <- reactive({
# Yield goal
  boot.s <- Y.boot()$boot.s 
  boot.Y <- as.matrix(Y.boot()$Y.boot)
  boot.Ym <- colMeans(boot.Y)
  boot.Yu <- apply(boot.Y,2,function(x) quantile(x,0.975))
  boot.Yl <- apply(boot.Y,2,function(x) quantile(x,0.025))
  boot.Y <- as.matrix(Y.boot()$Y.boot)
# Recruit goal  
  boot.R <- as.matrix(Y.boot()$R.boot)
  boot.Rm <- colMeans(boot.R)
  boot.Ru <- apply(boot.R,2,function(x) quantile(x,0.975))
  boot.Rl <- apply(boot.R,2,function(x) quantile(x,0.025))
  out <- list(boot.s = boot.s, boot.Ym = boot.Ym, boot.Yu = boot.Yu, boot.Yl = boot.Yl,
              boot.Rm = boot.Rm, boot.Ru = boot.Ru, boot.Rl = boot.Rl)
  return(out)
  })
 
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
#  Bootstrap Smsy Anaylses Results Out    
#-----------------------------------------------------------------------
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
#  Bootstrap Smax Anaylses Results Out    
#-----------------------------------------------------------------------
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
#  Bootstrap Escapement Goal based Analneyses   
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
# Yield plot 
#---------------------------------------------------------------------------
byield.p <- reactive({
  u <- as.numeric(input$ui)
  mult <- ifelse(u>1,paste0('(x',u,')'),'')
  boot.s <- b.YA()$boot.s
  boot.Yu <- b.YA()$boot.Yu
  boot.Yl <- b.YA()$boot.Yl
  boot.Ym <- b.YA()$boot.Ym
  BEG.1 <- b.SA()$BEG.1
  BEG.2 <- b.SA()$BEG.2
  df1 <- data.frame(boot.s,boot.Yu,boot.Yl,boot.Ym)
  names(df1) <- c('s','Yu','Yl','Ym')
  p1 <- ggplot(df1,aes(x=s,y=Ym))+geom_line()+ 
    coord_cartesian(ylim=c(0,with(df1,max(Yu))))+
    scale_y_continuous(labels = function(x){paste0(x/u)})+  
    scale_x_continuous(labels = function(x){paste0(x/u)})+  
    labs(title = input$caption, x=paste('Escapement',mult),y=paste('Expected Yield',mult))+ 
    geom_line(data=df1,aes(x=s,y=Yl),linetype=2)+
    geom_line(data=df1,aes(x=s,y=Yu),linetype=2)
  return(p1)
 })
brun.p <- reactive({
  u <- as.numeric(input$ui)
  mult <- ifelse(u>1,paste0('(x',u,')'),'')
  boot.s <- b.YA()$boot.s
  boot.Ru <- b.YA()$boot.Ru
  boot.Rl <- b.YA()$boot.Rl
  boot.Rm <- b.YA()$boot.Rm
  BEG.1 <- b.SA()$BEG.1
  BEG.2 <- b.SA()$BEG.2
  df1 <- data.frame(boot.s,boot.Ru,boot.Rl,boot.Rm)
  names(df1) <- c('s','Ru','Rl','Rm')
  p1 <- ggplot(df1,aes(x=s,y=Rm))+geom_line()+ 
    coord_cartesian(ylim=c(0,with(df1,max(Ru))))+
    scale_y_continuous(labels = function(x){paste0(x/u)})+  
    scale_x_continuous(labels = function(x){paste0(x/u)})+  
    labs(title = input$caption, x=paste('Escapement',mult),y=paste('Expected Return',mult))+ 
    geom_line(data=df1,aes(x=s,y=Rl),linetype=2)+
    geom_line(data=df1,aes(x=s,y=Ru),linetype=2)
  return(p1)
 })


 
#=======================================================================
#  Plots Tables Outputs  
#======================================================================= 
#-----------------------------------------------------------------------
#  SR plot 
#-----------------------------------------------------------------------
output$p <- renderPlot({
  mp <- input$p.i/100
  u <- as.numeric(input$ui)
  mult <- ifelse(u>1,paste0('(x',u,')'),'')
  df1 <- sr.data()
  par <-SR.out()
  s <- seq(0,1.2*max(par$Seq,df1$S), length.out =100)
  pred.sr <- predict(SR(), newdata=data.frame(S=s), interval=input$Li,level = mp)
  ER <- exp(pred.sr)*s
  df2 <- data.frame(s=s,y=ER)
  p1 <- ggplot(df1,aes(x=S,y=R))+geom_point()+ 
  coord_cartesian(xlim=c(0,s),ylim=c(0,with(df1,1.2*max(R,S))))+
  scale_y_continuous(labels = function(x){paste0(x/u)})+  
  scale_x_continuous(labels = function(x){paste0(x/u)})+  
  labs(title = input$caption, x=paste('Spawner',mult),y=paste('Recruit',mult))+ 
  geom_abline(intercept=0,slope=1,color='red',size=1)+
  geom_line(data=df2,aes(s,y.fit))
  if(input$show.int==TRUE){
    p1 <- p1 +
    geom_line(data=df2,aes(s,y.lwr),linetype=2)+
    geom_line(data=df2,aes(s,y.upr),linetype=2)   
    } 
  if(input$show.points==TRUE) {
    p1 <- p1+geom_text(data=df1,aes(x=S,y=R,label=Yr),vjust = -0.8,col=4)
    } 
  if(input$show.smsy==TRUE) {
    p1 <- p1+ geom_vline(xintercept=par$Smsy,linetype=2)
     } 
  if(input$show.smax==TRUE) {
    p1 <- p1+geom_vline(xintercept=par$Smax,linetype=3)
    }
  print(p1)
})

#-----------------------------------------------------------------------
#  Residual Plot
#-----------------------------------------------------------------------  
output$Resid <- renderPlot({
    year <- data()[,input$Yr]
    resid <-residuals(SR())
    df <- data.frame(cbind(year,resid))
    names(df) <- c('year','resid')
    p <- ggplot(df, aes(x=year,y=resid)) + geom_point() +
      labs(title = input$caption, x='Year',y='Residuals') +          
      geom_hline(yintercept=0) + geom_smooth()
   print(p)    
  })
 
#-----------------------------------------------------------------------
#  Time serise
#-----------------------------------------------------------------------  
output$srt <- renderPlot({
#  par(mfrow=c(1,1),mar = c(1.75,1.5,1.5,1.75),oma = c(3,3,3,3))
  u <- as.numeric(input$ui)
  mult <- ifelse(u>1,paste0('(x',u,')'),'')
  df1 <- sr.data()
  df1.melt <- melt(df1,id ='Yr')
  p <- ggplot(df1.melt, aes(x=Yr,y=value,group=variable)) +
      geom_line(aes(linetype=variable)) + 
      scale_linetype_discrete(name  ="",breaks=c("S","R"),labels=c("Spawner", "Recruit")) +
      coord_cartesian(ylim=c(0,with(df1,max(R,S)))) +
      scale_y_continuous(labels = function(x){paste0(x/u)}) +
      labs(title = input$caption, x='Year',y=paste('Spawner / Recruit',mult))
  print(p)
 })

#-----------------------------------------------------------------------
#  SR Parameters Distribution   
#-----------------------------------------------------------------------
# Bootstrap summary 
output$bsummary <- renderPrint({
  print(summary(boot()[,c(4,2,5:10)]),digits=c(2,10,0,0,0,0,0,0))
})

output$bhist <- renderPlot({
  df <- boot()
  p1 <- ggplot(df,aes(x=alpha))+geom_density()+ggtitle("Ricker alpha")+
  theme(axis.title=element_blank(),axis.text.y=element_blank())
  p2 <- ggplot(df,aes(x=beta))+geom_density()+ggtitle("Ricker beta")+
  theme(axis.title=element_blank(),axis.text.y=element_blank())
  p3 <- ggplot(df,aes(x=Seq))+geom_density()+ggtitle("SEQ")+
  theme(axis.title=element_blank(),axis.text.y=element_blank())
  p4 <- ggplot(df,aes(x=Smsy))+geom_density()+ggtitle("Smsy")+
  theme(axis.title=element_blank(),axis.text.y=element_blank())
  p5 <- ggplot(df,aes(x=RSmsy))+geom_density()+ggtitle("R Smsy")+
  theme(axis.title=element_blank(),axis.text.y=element_blank())
  p6 <- ggplot(df,aes(x=MSY))+geom_density()+ggtitle("MSY")+
  theme(axis.title=element_blank(),axis.text.y=element_blank())
  p7 <- ggplot(df,aes(x=Smax))+geom_density()+ggtitle("Smax")+
  theme(axis.title=element_blank(),axis.text.y=element_blank())
  p8 <- ggplot(df,aes(x=Rmax))+geom_density()+ggtitle("Rmax")+
  theme(axis.title=element_blank(),axis.text.y=element_blank())
  print(plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,ncol=3,nrow=3))
  })

#-----------------------------------------------------------------------
#  Smsy Goal Profile Plot  
#-----------------------------------------------------------------------
output$bsmsy <- renderPlot({
  mp.1 <- input$p1/100
  mp.2 <- input$p2/100
  ap <- input$p3/100
  u <- as.numeric(input$ui)
  bn <- isolate(input$bn)
  boot.s <- b.SA()$boot.s 
  Y.prob.1 <- b.SA()$Y.prob.1
  Y.prob.2 <- b.SA()$Y.prob.2
  BEG.1 <- b.SA()$BEG.1
  BEG.2 <- b.SA()$BEG.2

#---------------------------------------------------------------------------
#  Smsy Optimum Profile Plot  
#---------------------------------------------------------------------------
    df1 <- data.frame(cbind(boot.s,Y.prob.1,Y.prob.2))
    names(df1) <- c('S','p1','p2')
    df1.melt <- melt(df1,id ='S')
    p <- ggplot(df1.melt, aes(x=S,y=value,group=variable)) +
    geom_line(aes(linetype=variable)) + 
    scale_linetype_discrete(name ="",breaks=c("p1","p2"),labels=c(paste(input$p1,'% MSY'),paste(input$p2,'% MSY'))) +
    coord_cartesian(ylim=c(0,1)) +
    geom_hline(yintercept = ap,lwd=1,col=2)+  
    labs(title = 'MSY Yield probability curve', x='Escapement',y='Probability')
    print(p)
})  

#------------------------------------------------------------------------
#  Escapement goal table output 
#------------------------------------------------------------------------
output$bsmsyt <-renderText({
    paste(SA.BEG()$t1,SA.BEG()$t2,sep='\n')
})

#---------------------------------------------------------------------------
#  Smsy Return and Yield Plot
#---------------------------------------------------------------------------
output$bsmsy.y <- renderPlot({
  p1 <- byield.p()
  print(p1)
 })

output$bsmsy.r <- renderPlot({
  p1 <- brun.p()
  print(p1)
 })


#-----------------------------------------------------------------------
#  Yield Goal Plot and output  
#-----------------------------------------------------------------------
 output$byield <- renderPlot({
   yg <- input$y1
   p1 <-  byield.p()
   p2 <- p1+geom_hline(yintercept = yg,lwd=1,col=2)
   print(p2)
   }) 
 
 output$byt <-renderText({
     u <- as.numeric(input$ui)
     BEG.l <- u*round(b.YAg()$BEG.l/u)
     BEG.m <- u*round(b.YAg()$BEG.m/u)
     BEG.u <- u*round(b.YAg()$BEG.u/u)
     t.BEG.l <- paste('Lower Limit',BEG.l[1],'-',BEG.l[2])
     t.BEG.m <- paste('Mid   Limit',BEG.m[1],'-',BEG.m[2])
     t.BEG.u <- paste('Upper Limit',BEG.u[1],'-',BEG.u[2])
     paste(t.BEG.l,t.BEG.m,t.BEG.u,sep='\n')
   })

 output$byp <- renderPlot({
   yg <- input$y1
   ypg <- input$y1p/100
   u <- as.numeric(input$ui)
   mult <- ifelse(u>1,paste0('(x',u,')'),'')
   boot.s <- b.YA()$boot.s
   boot.Yp <- b.YAg()$boot.Yp
   df1 <- data.frame(cbind(boot.s,boot.Yp))
   names(df1) <- c('S','Yp')
   p <- ggplot(df1, aes(x=S,y=Yp)) + geom_line() + 
     coord_cartesian(ylim=c(0,1)) +
     scale_x_continuous(labels = function(x){paste0(x/u)})+ 
     geom_hline(yintercept = ypg,lwd=1,col=2)+  
     labs(title = paste('Optimum',yg,'Yield probability plot'), x=paste('Escapement',mult),y='Probability')
   print(p)   
  })  
 
 output$bypt <-renderText({
   u <- as.numeric(input$ui)
   BEG.p <- u*round(b.YApg()/u)
   paste('   Optimum Goal Range:',BEG.p[1],'-',BEG.p[2])
 })
 
#-----------------------------------------------------------------------
#  Return  Goal Plot and output  
#-----------------------------------------------------------------------
 output$breturn <- renderPlot({
   rg <- input$r1
   p1 <- brun.p()
   p2 <- p1+geom_hline(yintercept = rg,lwd=1,col=2)
   print(p2)
 }) 
 
 output$brt <-renderText({
   u <- as.numeric(input$ui)
   BEG.l <- u*round(b.RAg()$BEG.l/u)
   BEG.m <- u*round(b.RAg()$BEG.m/u)
   BEG.u <- u*round(b.RAg()$BEG.u/u)
   t.BEG.l <- paste('Lower Limit',BEG.l[1],'-',BEG.l[2])
   t.BEG.m <- paste('Mid   Limit',BEG.m[1],'-',BEG.m[2])
   t.BEG.u <- paste('Upper Limit',BEG.u[1],'-',BEG.u[2])
   paste(t.BEG.l,t.BEG.m,t.BEG.u,sep='\n')
 })
 
 output$brp <- renderPlot({
   rg <- input$r1
   rpg <- input$r1p/100
   u <- as.numeric(input$ui)
   mult <- ifelse(u>1,paste0('(x',u,')'),'')
   boot.s <- b.YA()$boot.s
   boot.Rp <- b.RAg()$boot.Rp
   df1 <- data.frame(cbind(boot.s,boot.Rp))
   names(df1) <- c('S','Rp')
   p <- ggplot(df1, aes(x=S,y=Rp)) + geom_line() + 
     coord_cartesian(ylim=c(0,1)) +
     scale_x_continuous(labels = function(x){paste0(x/u)})+ 
     geom_hline(yintercept = rpg,lwd=1,col=2)+  
     labs(title = paste('Optimum',rg,'Return probability plot'), x=paste('Escapement',mult),y='Probability')
   print(p)  
 })  
 
 output$brpt <-renderText({
   u <- as.numeric(input$ui)
   BEG.p <- u*round(b.RApg()/u)
   paste('   Optimum Goal Range:',BEG.p[1],'-',BEG.p[2])
 })
 
 
#-----------------------------------------------------------------------
#  Escapement Goal Plot 
#-----------------------------------------------------------------------
 output$bGAf <- renderPlot({
     u <- as.numeric(input$ui)
     mult <- ifelse(u>1,paste0('(x',u,')'),'')
     rg <- input$rg
     yg <- input$yg
     df <- b.GA.t()
     names(df) <- c('R','Y')
     p1 <- ggplot(df,aes(x=R))+geom_density()+ggtitle("Expected Return")+
       theme(axis.title=element_blank(),axis.text.y=element_blank())+
       scale_x_continuous(labels = function(x){paste0(x/u)})+ 
       geom_vline(xintercept=rg,linetype=2,col=2)
     p2 <- ggplot(df,aes(x=Y))+geom_density()+ggtitle("Expected Yields")+
       theme(axis.title=element_blank(),axis.text.y=element_blank())+
       scale_x_continuous(labels = function(x){paste0(x/u)})+ 
       geom_vline(xintercept=yg,linetype=2,col=2)
     print(plot_grid(p1,p2,ncol=2,nrow=1))
   }) 

 output$bGAs <- renderPrint({
   print(summary(b.GA.t()),digits=0)
 })
 
 output$bGAt <- renderText({
     rg <- input$rg
     yg <- input$yg
     prg <- sum(ifelse(b.GA.t()$Return>rg,1,0))/length(b.GA.t()$Return)
     pyg <- sum(ifelse(b.GA.t()$Yields>yg,1,0))/length(b.GA.t()$Yields)
     t.prg <- paste('Meeting Target Return:',round(100*prg,0),'%')
     t.pyg <- paste('Meeting Target Yields:',round(100*pyg,0),'%')
     paste(t.prg,t.pyg,sep='\n')
 })
 
 
#-----------------------------------------------------------------------
#  Smax Goal Simulation     
#-----------------------------------------------------------------------
 output$bsmax.r <- renderPlot({
   u <- as.numeric(input$ui)
   BEG.1 <- b.SX()$rsmax
   p1 <-  brun.p()
   p2 <- p1+geom_vline(xintercept = BEG.1,lwd=1,col=2)
   print(p2)
 })
 
 output$bsmax <- renderPlot({
   u <- as.numeric(input$ui)
   mult <- ifelse(u>1,paste0('(x',u,')'),'')
   df <- data.frame(b.SX()$bR)
   names(df) <- c('bR')
   p1 <- ggplot(df,aes(x=bR))+geom_density()+
     theme(axis.text.y=element_blank())+
     scale_x_continuous(labels = function(x){paste0(x/u)})+
     labs(title ="Expected Return Distribuion", x=paste('Return size',mult))
     print(p1)
 }) 
 
 output$bsmaxt <- renderText({
   u <- as.numeric(input$ui)
   BEG.p <- u*round(b.SX()$rsmax/u)
   paste('Smax Goal Range:',BEG.p[1],'-',BEG.p[2]) 
 })
 
 output$bsum <- renderPrint({
   options("scipen"=100, "digits"=4)
   print(summary(b.SX()$bR),digits=0)
 })
 
  

})  #End Server 


# Create Shiny app ----
shinyApp(ui, server)
