#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(datasets)
library(lmtest)
library(mgcv)
library(coda)
library(R2jags)

DataInputUI <- function(id) {
  ns <- NS(id)
 tagList(  
  # Input: Select a file ----
  fileInput("file1", "Choose CSV File",
            multiple = TRUE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  
  # Horizontal line ----
  tags$hr(),
  
  # Input: Checkbox if file has header ----
  checkboxInput("header", "Header", TRUE),
  
  # Input: Select separator ----
  radioButtons("sep", "Separator",
               choices = c(Comma = ",",
                           Semicolon = ";",
                           Tab = "\t"),
               selected = ","),
  
  # Input: Select quotes ----
  radioButtons("quote", "Quote",
               choices = c(None = "",
                           "Double Quote" = '"',
                           "Single Quote" = "'"),
               selected = '"'),
  
  # Horizontal line ----
  tags$hr(),
  
  # Input: Select number of rows to display ----
  radioButtons("disp", "Display",
               choices = c(Head = "head",
                           All = "all"),
               selected = "head")
  )
}


DataOutput <- function(input, output, session){
  oupput$data <- reactive({
    req(input$file1)
    inFile <- input$file1
    #could also store in a reactiveValues
    df <- read.csv(inFile$datapath,
                   header = input$header,
                   sep = input$sep)
    return(df)
  }) 
}

DataOutputUI<-function(id){
  ns<-NS(id)
  tagList(renderTable(nS('data')))
}



# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Uploading Files"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      DataInputUI('basic')
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Data file ----
     DataOutputUI(DataInput,'basic')
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  callModule(DataOuput,'basic')
  

  
  #-----------------------------------------------------------------------
  #  Panel 1: Data upload and output 
  #-----------------------------------------------------------------------
  # Uplaed file 

  #-----------------------------------------------------------------------
  #  Panel 2: SR Data Analyses and Output  
  #-----------------------------------------------------------------------
  #-----------------------------------------------------------------------
  #  1.0: SR Model   
  #-----------------------------------------------------------------------
  Baysedata <- reactive({
    x <- data()
    x2 <- as.Date(x$Date,"%d-%b")
    # nyrs is the number of years (i.e. number of columns) 
    nyrs <- dim(x)[2] -2
    # ndays is the number of days (i.e. number of rows)  
    ndays <- dim(x)[1]
    # set data     
    y <- matrix(0,nyrs,ndays)
    for (i in 1:ndays){
      for (j in 1:nyrs){
        # Add 0.01 so that the model will not fail. 
        y[j,i]<-  ifelse(x[i,j+2]<=0,0,x[i,j+2])
      }
    }
    dat <-list(nyrs=nyrs, ndays=ndays, y=y)
    return(dat)
  })  
  
  
  
}

# Create Shiny app ----
shinyApp(ui, server)


