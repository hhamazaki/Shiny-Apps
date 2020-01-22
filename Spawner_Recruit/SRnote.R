#initialize
library(shiny)
library(shinythemes)
library(datasets)
library(lmtest)

# UI for app
ui<-fluidPage(
  navbarPage(
    theme = shinytheme("cerulean"),  
    "Escapement Goal Analyses",
#---------------------------------------------------    
#  Panel 1:  Data Input
#---------------------------------------------------
  tabPanel("Data Input",
   sidebarPanel(width = 3,
#---------------------------------------------------    
#  File Inuput
#---------------------------------------------------
# Input: Select a file ----
  textInput(inputId='caption',label='Caption Title',value=''),
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
        choices = c(Semicolon = ";", Comma = ",", Tab = "\t"),
                 selected = ","),
    radioButtons("disp", "Display",
                choices = c(Head = "head", All = "all"),
                 selected = "head"),
# Horizontal line ----
    br(),
    p("Select Column name from data"),
    # Input: Select what to display
    selectInput(inputId="Yr","Brood Year:", choices = ""),
    selectInput(inputId="S","Spawner:", choices = ""),
    selectInput(inputId="R","Recruit:", choices = ""),
    br(),
    numericInput("bn", "Number bootstrap replicates", value=1000,min=1000,step=1000),
    p("Submit data for Analyses"),
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
      selectInput(inputId="ui","Dislpay Unit", choices = c(1,100,1000,10000,100000,1000000)),  
        checkboxInput(inputId="show.points", "show Years", TRUE), 
        checkboxInput(inputId="show.smsy", "show Smsy", TRUE),
        checkboxInput(inputId="show.int", "show Interval", TRUE),
        numericInput(inputId="p.i", "% Interval", value=90,min=0,max=100,step=5),
        selectInput(inputId="Li","Interval Type", choices = c('confidence','prediction'))         
           ),
           mainPanel(tabsetPanel(
             tabPanel("SR Plot",plotOutput(height='500px',"p"),
                      verbatimTextOutput('anova'),
                      verbatimTextOutput('RS.out')                      
                      ),
             tabPanel("Time Series",
                      plotOutput("srt")
                      ),
             tabPanel("Residuals", 
                      plotOutput("Resid"),
                      verbatimTextOutput('dwtest')),
             tabPanel("Bootstrap",
                      verbatimTextOutput('bsummary'),
                      plotOutput("bhist"))
           ))
           ),
#-----------------------------------------------------------------------
#  Panel 3 
#-----------------------------------------------------------------------
   tabPanel("Escapement Goal Analyses",
       sidebarPanel(width = 3,
         numericInput("p1", "Min % of MSY 1", value=90,min=0,max=100,step=5),
         numericInput("p2", "Min % of MSY 2", value=90,min=0,max=100,step=5),
         numericInput("p3", "Min % of Meeting the MSY Goal", value=90,min=0,max=100,step=5),
         numericInput("y1", "Yied Goal", value=100000,min=0, step=10000),
         selectInput(inputId="y2","Yield Range", choices = c('Lower','Middle','Upper'))  
                       ),
      mainPanel(tabsetPanel(
                  tabPanel("Smsy Analyses",plotOutput('bsmsy')),
                  tabPanel("Smax Analyses",plotOutput('bsmax')),
                  tabPanel("Yield Analyses",plotOutput("byield")),
                  tabPanel("To be added","To be added")
                    ))
                    
     )
  
 ))


# shiny server side code for each call
server<-shinyServer(function(input, output, session){
#-----------------------------------------------------------------------
#  Data upload
#-----------------------------------------------------------------------
# set uploaded file
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
      if(input$disp == "head") {
        return(head(data()))}
      else
        return(data())
    })
  
  output$par <- renderDataTable(
    {
      if(input$disp == "head") {
        return(head(data()))}
      else
        return(data())
    })
  
  output$summary <- renderPrint({
      summary(data())
    })

#-----------------------------------------------------------------------
#  SR Analyses 
#-----------------------------------------------------------------------
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
  
# Bootstrap summary 
 output$bsummary <- renderPrint({
   print(summary(boot()[,c(4,2,5:10)]),digits=c(2,10,0,0,0,0,0,0))
 })

#-----------------------------------------------------------------------
#  Bootstrap  
#-----------------------------------------------------------------------
 boot <- eventReactive(input$go,{
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
    return(boot.R)
  })

 Y.boot <- eventReactive(input$go,{
  boot.n <- isolate(input$bn)   
  par <-SR.out()
  Seq <- par$Seq
  boot.s <- seq(0,Seq,length.out=100)
  boot.Yield <- matrix(0,nrow=boot.n,ncol=length(boot.s))
  for(i in 1:boot.n)
  {
   boot.Ri <- boot()[i,]
   boot.Yield[i,] <- with(boot.Ri,boot.s*exp(ln.alpha-beta*boot.s)-boot.s)
  }
  return(boot.Yield)
  })
  

#-----------------------------------------------------------------------
#  Residual Plot
#-----------------------------------------------------------------------  
output$Resid <- renderPlot({
    year <- data()[,input$Yr]
    resid <-residuals(SR())
    predict <- predict(SR())
    plot(resid~year,xlab='Year',ylab='Residuals')
    abline(h=0)
    lines(smooth.spline(resid~year),col=4)
  })
 
#-----------------------------------------------------------------------
#  Time serise
#-----------------------------------------------------------------------  
output$srt <- renderPlot({
  par(mfrow=c(1,1),mar = c(1.75,1.5,1.5,1.75),oma = c(3,3,3,3))
  x <- sr.data()
  u <- as.numeric(input$ui)
  plot(R/u~Yr,data=x,type='l',xlab='Year',ylab='Recurit Spawner')
  lines(S/u~Yr,data=x,lty=2)
  legend('topright',c('Spawner','Recruit'),lty=c(2,1),box.lty=0)  
  ########  Add Texts  ###########################################################
  mult <- ifelse(u>1,paste0('(x',u,')'),'')
  mtext(input$caption, side = 3, line = 0, outer = TRUE)
  mtext(paste('Spawner / Recruit',mult), side = 2, line = 1, outer = TRUE)
  mtext(paste("Year"), side = 1, line = 1, outer = TRUE)
})

#-----------------------------------------------------------------------
#  SR plot 
#-----------------------------------------------------------------------
output$p <- renderPlot({
  mp <- input$p.i/100
  u <- as.numeric(input$ui)
  x <- sr.data()
  par <-SR.out()
  s <- seq(0,max(par$Seq,x$S), length.out =100)
  xp <- x/u
  #make sure variable and group have loaded
  par(mfrow=c(1,1),mar = c(1.75,1.5,1.5,1.75),oma = c(3,3,3,3))
  plot(R~S,data=xp,pch=19,col=1, xlab='',ylab='',
           xlim=c(0,max(xp$S)),ylim=c(0,1.2*max(xp$R)))
  abline(0,1,col=2)
  pred.sr <- predict(SR(), newdata=data.frame(S=s), interval=input$Li,level = mp)
  ER <- exp(pred.sr)*s
  df1 <- data.frame(s=s,y=ER)/u
  lines(y.fit~s,data=df1,col=1,lw=2)
      if(input$show.int==TRUE){
      lines(y.upr~s,data=df1,col=1,lw=1,lty=4)
      lines(y.lwr~s,data=df1,col=1,lw=1,lty=4)
        }
      if(input$show.points==TRUE) {
        text(R~S,data=xp, labels=x$Yr, cex= 1, pos=3,col=4)}
      if(input$show.smsy==TRUE) {abline(v=par$Smsy/u,col=1,lty=2) }
#        tex <- c(paste('alpha',round(par$alpha,1)),paste('beta',round(par$beta,5)),
#                 paste('SEQ',round(par$Seq/u)),paste('Smsy',round(par$Smsy/u)))
#        legend('topright',tex,box.lty=0)
   ########  Add Texts  ###########################################################
   mult <- ifelse(u>1,paste0('(x',u,')'),'')
   mtext(input$caption, side = 3, line = 0, outer = TRUE)
   mtext(paste('Recruit',mult), side = 2, line = 1, outer = TRUE)
   mtext(paste("Escapement",mult), side = 1, line = 1, outer = TRUE)
         })
  
#-----------------------------------------------------------------------
#  Distribution   
#-----------------------------------------------------------------------
output$bhist <- renderPlot({
  par(mfrow=c(3,3))
  plot(density(boot()$alpha), main='Ricker alpha',xlab='',ylab='')
  plot(density(boot()$beta), main='Ricker beta',xlab='',ylab='')
  plot(density(boot()$Seq), main='SEQ',xlab='',ylab='')
  plot(density(boot()$Smsy), main='Smsy',xlab='',ylab='')
  plot(density(boot()$RSmsy), main='R Smsy',xlab='',ylab='')
  plot(density(boot()$MSY), main='MSY',xlab='',ylab='')
  plot(density(boot()$Smax), main='Smax',xlab='',ylab='')
  plot(density(boot()$Rmax), main='Rmax',xlab='',ylab='')
  })

#-----------------------------------------------------------------------
#  Smsy Goal Simulation    
#-----------------------------------------------------------------------
output$bsmsy <- renderPlot({
  mp.1 <- input$p1/100
  mp.2 <- input$p2/100
  ap <- input$p3/100
  u <- as.numeric(input$ui)
  boot.n <- isolate(input$bn)
  par <-SR.out()
  Seq <- par$Seq
  boot.s <- seq(0,Seq,length.out=100)
  bMSY <- boot()$MSY
  boot.Y <- as.matrix(Y.boot())
  boot.Smsy.1 <- ifelse(boot.Y > mp.1*bMSY,1,0)
  boot.Smsy.2 <- ifelse(boot.Y > mp.2*bMSY,1,0)
  Y.prob.1 <- colSums(boot.Smsy.1)/boot.n
  Y.prob.2 <- colSums(boot.Smsy.2)/boot.n
  boot.Ym <- colMeans(boot.Y)
  boot.Yu <- apply(boot.Y,2,function(x) quantile(x,0.975))
  boot.Yl <- apply(boot.Y,2,function(x) quantile(x,0.025))
  
#---------------------------------------------------------------------------
#  Smsy Profile 
#---------------------------------------------------------------------------
  par(mfrow=c(1,2)) 
  plot(boot.s,Y.prob.1,type='l',ylab = 'Probability',xlab='Escapement',
       main=paste0(100*mp.1,'%','','MSY Yield probability curve')) 
  lines(boot.s,Y.prob.2,lty = 4)
  abline(h = ap,lwd=2,col=2)
  BEG.1 <- c(min(boot.s[which(round(Y.prob.1-ap)==0)]),max(boot.s[which(round(Y.prob.1-ap)==0)]))
  BEG.1 <- u*round(BEG.1/u)
  tex <- c(paste(paste0(100*ap,'%'),BEG.1[1],'-',BEG.1[2]))
  legend(x=min(boot.s), y=0.2, legend=tex,col=c(2),box.lty=0)
#---------------------------------------------------------------------------
#  Yield Plot
#---------------------------------------------------------------------------
  plot(boot.s,boot.Ym,type='l',ylim=c(0,max(boot.Yu)),
        main= 'Yield Curve',xlab='Escapement',ylab='Expected Yield') 
  lines(boot.s,boot.Yu,lty=2)
  lines(boot.s,boot.Yl,lty=2)
  abline(v=BEG.1[1],lwd=2,col=2)
  abline(v=BEG.1[2],lwd=2,col=2)
  if(input$y2=='Lower') {
    BEG.2 <- c(min(boot.Yl[which(round((boot.s-BEG.1[1])/BEG.1[1],1)==0)],boot.Yl[which(round((boot.s-BEG.1[2])/BEG.1[2],1)==0)]))
  } else if (input$y2=='Middle'){
    BEG.2 <- c(min(boot.Ym[which(round((boot.s-BEG.1[1])/BEG.1[1],1)==0)],boot.Ym[which(round((boot.s-BEG.1[2])/BEG.1[2],1)==0)]))
    } else {
    BEG.2 <- c(min(boot.Yu[which(round((boot.s-BEG.1[1])/BEG.1[1],1)==0)],boot.Yu[which(round((boot.s-BEG.1[2])/BEG.1[2],1)==0)]))
    }
  abline(h=BEG.2)
  BEG.2 <- u*round(BEG.2/u)
  tex <- c(paste('Min Yield',BEG.2))
  legend(x=min(boot.s), y=input$y1*0.8, legend=tex,col=c(2),box.lty=0)
})  

#-----------------------------------------------------------------------
#  Yield Goal Simulation      
#-----------------------------------------------------------------------
output$byield <- renderPlot({
  u <- as.numeric(input$ui)
  par <-SR.out()
  Seq <- par$Seq
  boot.s <- seq(0,Seq,length.out=100)
  boot.Y <- as.matrix(Y.boot())
    boot.Ym <- colMeans(boot.Y)
    boot.Yu <- apply(boot.Y,2,function(x) quantile(x,0.975))
    boot.Yl <- apply(boot.Y,2,function(x) quantile(x,0.025))
      
  #---------------------------------------------------------------------------
  #  Yield Plot
  #---------------------------------------------------------------------------
  plot(boot.s,boot.Ym,type='l',ylim=c(0,max(boot.Yu)),
       xlab='Escapement',ylab='Expected Yield') 
  lines(boot.s,boot.Yu,lty=2)
  lines(boot.s,boot.Yl,lty=2)
  abline(h=input$y1,lwd=2,col=2)
  if(input$y2=='Lower') {
    BEG.2 <- c(min(boot.s[which(round((boot.Yl-input$y1)/input$y1,1)==0)]),max(boot.s[which(round((boot.Yl-input$y1)/input$y1,1)==0)]))
  } else if (input$y2=='Middle'){
    BEG.2 <- c(min(boot.s[which(round((boot.Ym-input$y1)/input$y1,1)==0)]),max(boot.s[which(round((boot.Ym-input$y1)/input$y1,1)==0)]))
  } else {
    BEG.2 <- c(min(boot.s[which(round((boot.Yu-input$y1)/input$y1,1)==0)]),max(boot.s[which(round((boot.Yu-input$y1)/input$y1,1)==0)]))
  }
  BEG.2 <- u*round(BEG.2/u)
  tex <- c(paste('Yield Goal',BEG.2[1],'-',BEG.2[2]))
  legend(x=min(boot.s), y=input$y1*0.8, legend=tex,col=c(2),box.lty=0)
})  


#-----------------------------------------------------------------------
#  Smax Goal Simulation     
#-----------------------------------------------------------------------
output$bsmax <- renderPlot({
  mp <- input$p1/100
  ap <- input$p2/100
  boot.n <- isolate(input$bn)
  u <- as.numeric(input$ui)
  par <-SR.out()
  Seq <- par$Seq
  Smax <- par$Smax
  boot.s <- seq(0,Seq,length.out=100)
  boot.Smax <- matrix(0,nrow=boot.n,ncol=length(boot.s))
  boot.Yield <- matrix(0,nrow=boot.n,ncol=length(boot.s))
  for (i in 1:boot.n)
  {
    boot.Ri <- boot()[i,]
    pred <- with(boot.Ri,boot.s*exp(ln.alpha-beta*boot.s))
    boot.Smax[i,] <- ifelse(pred > mp*boot.Ri$Rmax,1,0)
    boot.Yield[i,] <- pred - boot.s
  }
  Y.prob <- colSums(boot.Smax)/boot.n
  boot.Ym <- colMeans(boot.Yield)
  boot.Yu <- apply(boot.Yield,2,function(x) quantile(x,0.975))
  boot.Yl <- apply(boot.Yield,2,function(x) quantile(x,0.025))
  #---------------------------------------------------------------------------
  #  Smax Profile 
  #---------------------------------------------------------------------------
  par(mfrow=c(1,2)) 
  plot(boot.s,Y.prob,type='l',ylab = 'Probability',xlab='Escapement',
       main=paste0(100*mp,'%','','Rmax probability curve')) 
  abline(h = ap,lwd=2,col=2)
  BEG.1 <- c(min(boot.s[which(round(Y.prob-ap)==0)]),max(boot.s[which(round(Y.prob-ap)==0)]))
  BEG.1 <- u*round(BEG.1/u)
  tex <- c(paste(paste0(100*ap,'%'),BEG.1[1],'-',BEG.1[2]))
  legend(x=min(boot.s), y=0.2, legend=tex,col=c(2),box.lty=0)
  #---------------------------------------------------------------------------
  #  Yield Plot
  #---------------------------------------------------------------------------
  plot(boot.s,boot.Ym,type='l',ylim=c(0,max(boot.Yu)),
       main= 'Yield Curve',xlab='Escapement',ylab='Expected Yield') 
  lines(boot.s,boot.Yu,lty=2)
  lines(boot.s,boot.Yl,lty=2)
  abline(v=BEG.1[1],lwd=2,col=2)
  abline(v=BEG.1[2],lwd=2,col=2)
  if(input$y2=='Lower') {
    BEG.2 <- c(min(boot.Yl[which(round((boot.s-BEG.1[1])/BEG.1[1],1)==0)],boot.Yl[which(round((boot.s-BEG.1[2])/BEG.1[2],1)==0)]))
  } else if (input$y2=='Middle'){
    BEG.2 <- c(min(boot.Ym[which(round((boot.s-BEG.1[1])/BEG.1[1],1)==0)],boot.Ym[which(round((boot.s-BEG.1[2])/BEG.1[2],1)==0)]))
  } else {
    BEG.2 <- c(min(boot.Yu[which(round((boot.s-BEG.1[1])/BEG.1[1],1)==0)],boot.Yu[which(round((boot.s-BEG.1[2])/BEG.1[2],1)==0)]))
  }
  abline(h=BEG.2)
  BEG.2 <- u*round(BEG.2/u)
  tex <- c(paste('Min Yield',BEG.2))
  legend(x=min(boot.s), y=input$y1*0.8, legend=tex,col=c(2),box.lty=0)
})  


})  #End Server 


# Create Shiny app ----
shinyApp(ui, server)
