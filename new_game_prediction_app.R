library(tidyverse)
library(shiny)
library(ggplot2)
library(mlr)

set.seed(43)
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

lm_fit <- lm(Rating ~ Reviewscore, data = df)

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
      selectInput("dlc", "DLC:", choices = unique(df$DLC_Played)),
      actionButton("predict", "Compute Prediction!")
    )
    ,
    mainPanel(
      textOutput("prediction"),
      div(
        style = "position:relative",
        plotOutput("plot", 
                   hover = hoverOpts(id = "plot_hover", delay = 100, delayType = "debounce")),
        uiOutput("hover_info")
      ),
      plotOutput("band", height = "100px")
    )
  )
)

server <- function(input, output) {
  
  prediction <- eventReactive(input$predict, {
    
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
  
  band_data <- eventReactive(input$predict, {
    
    df %>% filter(abs(Reviewscore - input$rs) <= 3) %>% select(Rating)
    
  })
  
  review_input <- eventReactive(input$predict, {
    input$rs
  })
  
  name_input <- eventReactive(input$predict, {
    input$ng
  })
  
  output$plot <- renderPlot({
    ggplot(df, aes(x = Reviewscore, y = Rating)) +
      geom_count(color = 'grey', shape = 20) +
      geom_abline(slope = coef(lm_fit)[["Reviewscore"]], intercept = coef(lm_fit)[["(Intercept)"]], col = 'brown', linetype = "dashed", alpha = 0.5) +
      annotate("point", x = review_input(), y = prediction(), color = 'red', shape = 4, size = 3) +
    labs(title = paste("Rating vs Reviewscore With Highlighted", name_input(), "Prediction"),
           x = "Review Score",
           y = "Rating")
      
  }, res = 96)
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(df, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Game: </b>", point$Game, "<br/>",
                    "<b> Rating: </b>", point$Rating, "<br/>",
                    "<b> Review Score: </b>", point$Reviewscore, "<br/>")))
    )
  })
  
  
  output$prediction <- renderText({
    paste("Expected Rating for ", input$ng, " is ", prediction())
  })
  
  output$band <- renderPlot({
    ggplot(NULL) +
      geom_count(aes(x = band_data()$Rating, y = integer(nrow(band_data()))), color = 'grey', shape = 20) +
      annotate("point", x = prediction(), y = 0, color = 'red', shape = 4, size = 3) +
      labs(title = paste('Rating for games with similar Review Scores to', input$ng),
           x = "Rating",
           y = "") +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none")
      
  }, res = 96)
}

shinyApp(ui = ui, server = server)
