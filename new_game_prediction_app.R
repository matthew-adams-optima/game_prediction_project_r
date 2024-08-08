library(tidyverse)
library(shiny)
library(ggplot2)

model <- readRDS("model.rds")
df <- readRDS("data.rds")

# define function to replace an input with Other if it has no match in the data
other_replace <- function(df, col_name, input) {
  
  if (input == "") {
    "Other"
  } else if (max(df[[col_name]] == input)){
    input
  } else {
    "Other"
  }
}

cols <- df[c("Reviewscore", 
             "Publisher", 
             "Franchise", 
             "Launch_Year",
             "Play_Year",
             "Category",
             "DLC_Played",
             "Perspective",
             "Played_On")]

ui <- fluidPage(
  titlePanel("Predicting my Game Ratings!"),
  sidebarLayout(
    sidebarPanel(
      h3("Input New Game"),
      textInput("ng", "Game Name:", value = "New Game"),
      sliderInput("rs", "Reviewscore:", min = 0, max = 100, value = 75),
      textInput("pub", "Publisher:"),
      textInput("fran", "Franchise:"),
      numericInput("ly", "Launch Year:", value = format(Sys.Date(), "%Y")),
      numericInput("py", "Play Year:", value = format(Sys.Date(), "%Y")),
      selectInput("cat", "Main Category:", choices = unique(df$Category)),
      selectInput("persp", "Perspective:", choices = unique(df$Perspective)),
      selectInput("po", "Played On:", choices = unique(df$Played_On)),
      selectInput("dlc", "DLC:", choices = unique(df$DLC_Played))
      
    )
    ,
    mainPanel(
      textOutput("prediction"),
      plotOutput("plot"),
      plotOutput("band")
    )
  )
)

server <- function(input, output) {
  
  prediction <- reactive({
    
    new_game <- as_tibble(cbind(input$rs, 
                                other_replace(df, "Publisher", input$pub),
                                other_replace(df, "Franchise", input$fran),
                                input$ly,
                                input$py,
                                input$cat,
                                input$persp,
                                input$po,
                                input$dlc))
    
    names(new_game) <- names(cols)
    
    model_input <- rbind(cols, new_game)
    
    predict(model, tail(model_input, 1))
    
  })
  
  band_data <- reactive({
    
    df %>% filter(abs(Reviewscore - input$rs) <= 3) %>% select(Rating)
    
  })
  
  output$plot <- renderPlot({
    plot(df$Reviewscore, df$Rating, pch = 20, col = 'grey')
    points(input$rs, prediction(), pch = 4, col = 'red', cex = 1.5)
  }, res = 96)
  
  output$prediction <- renderText({
    paste("Expected Rating for ", input$ng, " is ", prediction())
  })
  
  output$band <- renderPlot({
    plot(band_data()$Rating, integer(nrow(band_data())), pch = 20, col = 'grey')
    points(prediction(), 0, pch = 4, col = 'red', cex = 1.5)
  }, res = 96)
}

shinyApp(ui = ui, server = server)
