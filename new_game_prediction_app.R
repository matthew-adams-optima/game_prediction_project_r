library(tidyverse)
library(shiny)
library(ggplot2)

model <- readRDS("model.rds")
df <- readRDS("data.rds")

ui <- fluidPage(
  titlePanel("Predicting my Game Ratings!"),
  sidebarLayout(
    sidebarPanel(
      titlePanel("Input New Game"),
      textInput("ng", "Game Name:", value = "New Game"),
      sliderInput("rs", "Reviewscore:", min = 0, max = 100, value = 75),
      
    )
    ,
    mainPanel(
      textOutput("prediction"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  prediction <- reactive(input$rs)
  
  output$plot <- renderPlot({
    plot(df$Reviewscore, df$Rating, pch = 20, col = 'grey', alpha = 0.7)
    points(input$rs, prediction(), col = 'red', pch = 4, cex = 1.5)
  }, res = 96)
  
  output$prediction <- renderText({
    paste("Expected Rating for ", input$ng, " is ", prediction())
  })
}

shinyApp(ui = ui, server = server)
