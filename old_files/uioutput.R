library(shiny)
library(dplyr)
library(ggplot2)


innerModUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(fluidRow(
    uiOutput(ns("inner_slider")),
    plotOutput(ns("inner_plot"))
  ))
}

innerMod <- function(input, output, session) {
  output$inner_slider <- renderUI({
    sliderInput(session$ns("slider2"), label = "inner module slider", min = round(min(mtcars$mpg)), 
                max = round(max(mtcars$mpg)), value = c(min(mtcars$mpg), max(mtcars$mpg), step = 1))
  })
  
  output$inner_plot <- renderPlot({
    req(input$slider2)
    data <- filter(mtcars, between(mpg, input$slider2[1], input$slider2[2]))
    ggplot(data, aes(mpg, wt)) + geom_point()
  })
}

outerModUI <- function(id) {
  ns <- NS(id)
  fluidPage(fluidRow(
    uiOutput(ns("outer_slider")),
    plotOutput(ns("outer_plot")),
    innerModUI(ns("inner"))
  ))
}

outerMod <- function(input, output, session) {
  callModule(innerMod, "inner")
  
  output$outer_slider <- renderUI({
    sliderInput(session$ns("slider1"), label = "outer module slider", min = round(min(mtcars$mpg)), 
                max = round(max(mtcars$mpg)), value = c(min(mtcars$mpg), max(mtcars$mpg), step = 1))
  })
  
  output$outer_plot <- renderPlot({
    req(input$slider1)
    data <- filter(mtcars, between(mpg, input$slider1[1], input$slider1[2]))
    ggplot(data, aes(mpg, wt)) + geom_point()
  })
}

ui <- fluidPage(
  fluidRow(
    outerModUI("outer")
  )
)

server <- function(input, output, session) {
  callModule(outerMod, "outer")
  
}

shinyApp(ui = ui, server = server)