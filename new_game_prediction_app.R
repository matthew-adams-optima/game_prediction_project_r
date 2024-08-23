# Shiny App covering my game rating prediction model #

# import necessary packages
library(tidyverse)
library(shiny)
library(ggplot2)
library(mlr)
library(gbm)
library(pdp)
library(shinythemes)

## Preparation before running the Shiny App ##
set.seed(43)
model <- readRDS("model.rds")
df <- readRDS("data.rds")

# Extract model importances for analysis
importances <- summary(model)
names(importances) <- c("Variable", "Importance")

# define function to replace an input with Other if it has no match in the data
other_replace <- function(df, col_name, input) {
  
  if (input == "") {
    "Other"
  } else if (max(df[[col_name]] == input)) {
    input
  } else {
    "Other"
  }
}

# linear fit between rating and reviewscore for charts
lm_fit <- lm(Rating ~ Reviewscore, data = df)

# a dataframe containing only the input data for use in prediction page
cols <- df[c("Reviewscore", 
             "Publisher", 
             "Franchise", 
             "Launch_Year",
             "Play_Year",
             "Category",
             "DLC_Played",
             "Perspective",
             "Played_On")]

# calculate difference between rating and prediction for training set
predicts <- as_tibble(df$Rating - predict(model, df))

# define standard ggplot theme
sd_theme <- theme(axis.text = element_text(size=10),
                  plot.title = element_text(size=16, hjust = 0.5))

## UI ##

ui <- fluidPage(theme = shinytheme("flatly"),
      tags$head(
      tags$style(HTML("
      .shiny-output-error-validation {
        color: #008000;
        font-weight: bold;
        }"))
      ),
  tabsetPanel(
    
    # Page One - Model Analysis
    tabPanel("Model Analysis",
      titlePanel( h1("Analysing the Trained Model", align = "center", style = 'padding-bottom:30px')),
      sidebarLayout(
        sidebarPanel(
          # Side bar inputs in order of appearance
          sliderInput("binwidth", "Histogram bin width:", min = 0.5, max = 5, step = 0.1, value = 1.5),
          selectInput("pdvar", "Partial Dependence Variable:", choices = colnames(cols)),
          , width = 3
        ),
        mainPanel(
          fluidRow(
            column(6,
            plotOutput("model_summary"), # feature importances chart
            style = 'padding-left:0px; padding-right:10px; padding-top:5px; padding-bottom:5px'),
            column(6,
            plotOutput("freq_dist"), # partial dependence chart
            style = 'padding-left:0px; padding-right:0px; padding-top:5px; padding-bottom:5px'
            )
          ),
          fluidRow(
            column(12,
          plotOutput("partial_dependence"), # model error distribution
          style = 'padding-left:60px; padding-right:0px; padding-top:5px; padding-bottom:5px')
          )
        )
      )
    ),
    
    # Page Two - Make Predictions
    tabPanel("Prediction Tool",
      titlePanel(h1("Interactive Prediction Tool", align = "center", style = 'padding-bottom:30px')),
      sidebarLayout(
        sidebarPanel(
          # Side bar inputs in order of appearance
          actionButton("predict", "Predict New Game!", style = "margin-bottom:15px;"),
          div(style = "margin-bottom:0px; padding-bottom:0px;", textInput("ng", "Game Name:", value = "New Game")), #need to use div to style inputs
          sliderInput("rs", "Reviewscore:", min = 0, max = 100, value = 75),
          textInput("pub", "Publisher:"),
          textInput("fran", "Franchise:"),
          numericInput("ly", "Launch Year:", value = format(Sys.Date(), "%Y")),
          numericInput("py", "Play Year:", value = format(Sys.Date(), "%Y")),
          selectInput("cat", "Main Category:", choices = unique(df$Category)),
          selectInput("persp", "Perspective:", choices = unique(df$Perspective)),
          selectInput("po", "Played On:", choices = unique(df$Played_On)),
          selectInput("dlc", "DLC:", choices = unique(df$DLC_Played)),
          , width = 3
          ),
        mainPanel(
          # Main body content
          htmlOutput("prediction", #the top line printing the prediction
          style = 'padding-left:20px; padding-right:0px; padding-top:5px; padding-bottom:15px'),
          div( #main scatter plot with interactive hover element
            style = "position:relative",
            plotOutput("plot", 
                       hover = hoverOpts(id = "plot_hover", delay = 100, delayType = "debounce")),
            uiOutput("hover_info")
          ),
          plotOutput("band", height = "100px") #band of outcomes plot
        )
      )
    ),
    
    # Page Three - Recommender tool
    tabPanel("Recommender Tool",
      titlePanel(h1("Multiple Game Recommender Tool", align = "center", style = 'padding-bottom:30px')),
      sidebarLayout(
        sidebarPanel(
          fileInput("upload", "Upload a CSV:", accept = ".csv")
          , width = 3
        ),
        mainPanel(
          htmlOutput("recommend", style = 'padding-bottom:30px'),
          htmlOutput("table_header", align = "center"),
          tableOutput("head")
        )
      )
    )
  )
)

## Server ##

server <- function(input, output) {
  thematic::thematic_shiny() # set default plot themes as UI theme
  
  # Page One Relevant Server Functions
  
  # Calculate partial dependence in the model for input feature
  partial_dependence_category <- reactive({
    partial(model, pred.var = input$pdvar, n.trees = model$n.trees)
  })
  
  # Define text element theme parameters for different categorical features
  label_angle <- reactive({
    if (input$pdvar %in% c("Reviewscore", "Play_Year", "Launch_Year", "DLC_Played")) {
      c(0, 0, 0.5)
    }
    else {
      c(70, 1, 1.1)
    }
  })
  
  # Define plot output for feature importances - column chart
  output$model_summary <- renderPlot({
    ggplot(importances, aes(reorder(Variable, Importance), Importance)) +
      geom_col() +
      coord_flip() +
      labs(title = "Feature Importances",
           x = "Feature") +
      sd_theme
  }, res = 96)
  
  # Define plot output for model error distribution - histogram
  output$freq_dist <- renderPlot({
    
    ggplot(predicts, aes(x = value)) +
      geom_histogram(binwidth = input$binwidth, color = "black") +
      labs(title = "Model Error Distribution",
           x = "Actual - Prediction",
           y = "Count") +
      sd_theme
    
  }, res = 96)
  
  # Define plot output for partial dependence - interactive chart
  output$partial_dependence <- renderPlot({
    
    autoplot(partial_dependence_category()) +
      labs(title = paste("Partial Dependence Plot For", input$pdvar),
           y = "Expected Rating") +
      theme(axis.text.x = element_text(angle=label_angle()[1], vjust=label_angle()[2], hjust=label_angle()[3])) +
      sd_theme
    
  }, res = 96)
  
  # Page Two Relevant Server Functions
  
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
  
  output$prediction <- renderText({
    
    validate(need(input$predict, "Click the button at the top of the sidebar to make your first prediction!"))
    
    paste0("<font size= \"5\">",
      "Expected Rating for ", input$ng, " is ", "<b>", round(prediction(),0), "</b>", "!",
      "</font>")
  })
  
  output$plot <- renderPlot({
    ggplot(df, aes(x = Reviewscore, y = Rating)) +
      geom_count(color = 'grey', shape = 20) +
      geom_abline(slope = coef(lm_fit)[["Reviewscore"]], intercept = coef(lm_fit)[["(Intercept)"]], col = 'brown', linetype = "dashed", alpha = 0.5) +
      annotate("point", x = review_input(), y = prediction(), color = 'red', shape = 4, size = 3) +
      labs(title = paste("Rating vs Reviewscore With Highlighted", name_input(), "Prediction"),
           x = "Review Score",
           y = "Rating") +
      sd_theme
      
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
  
  output$band <- renderPlot({
    ggplot(NULL) +
      geom_violin(aes(x = band_data()$Rating, y = integer(nrow(band_data()))), color = 'grey') +
      annotate("point", x = prediction(), y = 0, color = 'red', shape = 4, size = 3) +
      labs(title = paste('Rating for games with similar Review Scores to', input$ng),
           x = "Rating",
           y = "") +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            legend.position = "none") +
      sd_theme
      
  }, res = 96)
  
  # Page Three Relevant Server Functions
  
  data <- reactive({
    # only run once the user has uploaded a file
    req(input$upload)
    
    # error if the file uploaded is not csv
    ext <- tools::file_ext(input$upload$name)
    
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           validate("Invalid file; Please upload a .csv file")
    )
  })
  
  predictions <- reactive({
    
    req(input$upload)
    
    new_games <- as_tibble(data())
    
    new_games <- new_games %>%
      mutate(Publisher = ifelse(Publisher %in% unique(df$Publisher), Publisher, "Other"),
             Franchise = ifelse(Franchise %in% unique(df$Franchise), Franchise, "Other"))
    
    cols <- cols %>% mutate(Game_Name = "Dummy", .before = Reviewscore)
    
    model_input <- rbind(cols, new_games)
    
    predicted_ratings <- round(predict(model, tail(model_input, nrow(new_games))), 0)
    
    new_games <- new_games %>% mutate(Prediction = predicted_ratings, .after = Game_Name)
    
    new_games <- new_games %>% mutate_at(c("Prediction", "Reviewscore", "Launch_Year", "Play_Year"),
                                             as.integer)
    
    arrange(new_games, desc(Prediction), desc(Reviewscore))
    
  })
  
  recommended_game <- reactive({
    
    validate(need(input$upload, "Upload a CSV file to get recommendations!"))
    
    top_game <- predictions() %>% select(Game_Name, Prediction) %>% slice(which.max(Prediction))
    
    paste0("The recommended game to play is "
           , "<b>", top_game$Game_Name, "</b>"
           , ", with an expected score of "
           , "<b>", top_game$Prediction, "</b>"
           , "!")
    
  })
  
  output$recommend <- renderText({
    
    paste0("<font size= \"5\">", recommended_game(), "</font>")
  })
  
  output$table_header <- renderText({
    
    req(input$upload)
    paste0("<font size= \"4\">", "Full list of games with predicted scores:", "</font>")
  })
  
  output$head <- renderTable({
    print(predictions())
  }, hover = TRUE, bordered = TRUE)
  
}

shinyApp(ui = ui, server = server)
