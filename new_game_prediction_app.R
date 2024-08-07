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
      textInput("ng", "Game Name:"),
      sliderInput("rs", "Reviewscore:", min = 0, max = 100, value = 75)
    )
    ,
    mainPanel(
      textOutput("prediction"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    plot(df$Rating, df$Reviewscore)
  }, res = 96)
  
  output$prediction <- renderText({
    paste("Expected Rating for ", input$ng, " is ", input$rs)
  })
}

shinyApp(ui = ui, server = server)
