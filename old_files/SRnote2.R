#initialize
library(shiny)
library(datasets)
library(shinythemes)
#-------------------------------------------------------------------------------
# ui Section:  This section dtermines the User Interface
#-------------------------------------------------------------------------------
ui<-fluidPage(
  navbarPage(
  theme = shinytheme("cerulean"),  
    "shinythemes",
  tabPanel("Data Input",
   sidebarPanel (
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
    tags$hr(),
    # Input: Select what to display
    selectInput(inputId="Yr","Brood Year:", choices = ""),
    selectInput(inputId="S","Spawner:", choices = ""),
    selectInput(inputId="R","Recruit:", choices = "")
   ), # End SidePanel
   # Horizontal line ----


#-------------------------------------------------------------------------------
# maiPanel:  This section is output display
#-------------------------------------------------------------------------------
  mainPanel(
    h3(textOutput("caption")),
    tabsetPanel(
      tabPanel("Table",dataTableOutput("table")),
      tabPanel("Summary",verbatimTextOutput('summary'))
    )
    )
  ),
  tabPanel("SR Model",
    sidebarPanel (
      checkboxInput(inputId="show.points", "show Years", TRUE),
      br(),
      actionButton("goButton", "Go!"),
      p("Click the button to update the value displayed in the main panel.")
    ),
    mainPanel(
      h3(textOutput("caption")),
      tabsetPanel(
        tabPanel("Plot",plotOutput("p")),
        tabPanel("Table","To be added")
      ) # End tabsetPanel(SR)
      ) # End mainPanel(SR)
    ) # End tabPanel(SR)
  ,
  tabPanel("Navbar 3", "This panel is intentionally left blank")
  ) #End Navbar  
 ) # End Fluid Panel

#-------------------------------------------------------------------------------
# server  Section:  This section does calculations and outputs
#-------------------------------------------------------------------------------
server<-shinyServer(function(input, output, session){
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
  
#-------------------------------------------------------------------------------
#  Created ouputs
#------------------------------------------------------------------------------- 
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

  output$SR <- renderPrint({
    SR()
  })
  
#-------------------------------------------------------------------------------
#  SR model figures 
#------------------------------------------------------------------------------- 
   output$p <- renderPlot({
      x <- data()[,c(input$S,input$R)]
      s <- seq(0,max(x[,input$S]),by=5000)
    #make sure variable and group have loaded
      SR <- lm(log(x[,input$R]/x[,input$S])~x[,input$S])
      ln.alpha <- SR$coefficients[1]
      alpha <- exp(ln.alpha)
      beta <- -SR$coefficients[2]
      ln.alpha.c <- ln.alpha+0.5*(sigma(SR))^2
      Seq <- ln.alpha.c/beta
      Smsy <- Seq*(0.5-0.07*ln.alpha.c)
      RSmsy <- Smsy*exp(ln.alpha.c-beta*Smsy)
      MSY <- RSmsy-Smsy
      Rmax <- exp(ln.alpha.c-1)/beta
      par(mfrow=c(2,1)) 
      plot(x,pch=19,col=1,xlim=c(0,max(x[,input$S])),ylim=c(0,1.2*max(x[,input$R])))
      abline(0,1,col=2)
      lines(alpha*s*exp(-beta*s)~s,col=1,lw=2)
      if(input$show.points==TRUE) {
        text(x, labels=data()[,input$Yr], cex= 1.0, pos=3)	}
        abline(v=Smsy,col=1,lty=2)
        tex <- c(paste('alpha',round(alpha,1)),paste('beta',round(beta,5)),
                 paste('SEQ',round(Seq,-3)),paste('Smsy',round(Smsy,-3)))
        legend('topright',tex,box.lty=0,xjust=0)
        plot(data()[,input$Yr],data()[,input$R],type='l',ylim=c(0,1.2*max(x[,input$R])))
        lines(data()[,input$Yr],data()[,input$S],lty=2)
         },height = 1000, width = 500)
  
#-------------------------------------------------------------------------------
#  SR model 
#------------------------------------------------------------------------------- 
   SR <- reactive({
   x <- data()[,c(input$S,input$R)]
   #make sure variable and group have loaded
   SR <- lm(log(x[,input$R]/x[,input$S])~x[,input$S])
   SR.out <- list(
     ln.alpha = SR$coefficients[1],
     alpha = exp(ln.alpha),
     beta = -SR$coefficients[2],
     ln.alpha.c = ln.alpha+0.5*(sigma(SR))^2,
     Seq = ln.alpha.c/beta,
     Smsy = Seq*(0.5-0.07*ln.alpha.c),
     RSmsy = Smsy*exp(ln.alpha.c-beta*Smsy),
     MSY = RSmsy-Smsy,
     Rmax = exp(ln.alpha.c-1)/beta,
     SRP = predict(SR),
     SRR = residuals(SR)
     ) 
   return(SR.out)
   })
  
})


# Create Shiny app ----
shinyApp(ui, server)
