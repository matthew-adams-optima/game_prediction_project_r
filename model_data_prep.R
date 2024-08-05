library(tidyverse)
library(ggplot2)
library(caret)

set.seed(43)

#read the input data
df <- read_csv("Games Database - Main Page.csv")

# convert to using tibble
df <- as_tibble(df)

# select only the necessary columns
df <- df %>% select(Game, 
                    Rating, 
                    Reviewscore, 
                    `Launch Platform(s)`, 
                    `Played on`, 
                    `Launch Year`, 
                    `Play Year`, 
                    `DLC/ Major update played`,
                    Publisher,
                    Developer,
                    Franchise,
                    Perspective,
                    `Main Category`,
                    `Extra Descriptors`,
                    `Remaster Reviewscore`,
                    `Remaster Release Year`)

## some quick exploratory plotting and analysis ##

#bubble plot
plot_data <- df %>% count(Reviewscore, Rating)
ggplot(plot_data, aes(x = Reviewscore, y = Rating, size = n)) +
  geom_point(alpha = 0.5)

#pearson correlation
cor(df$Rating, df$Reviewscore) # 0.730121

#convert the categoricals to factor
df$`DLC/ Major update played` <- as_factor(df$`DLC/ Major update played`)
df$Publisher <- as_factor(df$Publisher)
df$Developer <- as_factor(df$Developer)
df$Franchise <- as_factor(df$Franchise)
df$Perspective <- as_factor(df$Perspective)
df$`Main Category` <- as_factor(df$`Main Category`)

#function to group low volume categoricals into other for relevant fields
group_factor <- function(df, col_name, threshold){
  
  counts <- df %>% count(.data[[col_name]]) #need to use this format for count() within a function - annoying!
  counts_joined <- left_join(df, counts, by = col_name)
  df[[col_name]] <<- fct_collapse(counts_joined[[col_name]], Other = unique(counts_joined[[col_name]][counts_joined$n < threshold]))
}

#list the fields to group and the separate threshold for each
group_factor(df, "Publisher", 3)
group_factor(df, "Developer", 3)
group_factor(df, "Franchise", 3)
group_factor(df, "Main Category", 3)

#test and train split
train_ind <- as_vector(createDataPartition(df$Rating, p = 0.75))


train <- df[train_ind, ] #select all rows with the training indexes
test <- df[-train_ind, ] #select all rows not with the training indexes

## first model, linear using basic reviewscore only, for a benchmark ##

model1 <- lm(Rating ~ Reviewscore, data = train)
summary(model1) # Adjusted R-squared 0.5498 on training data

prediction <- predict(model1, test)
predict_df1 <- add_column(test, prediction)
cor(predict_df1$Rating, predict_df1$prediction)^2 # 0.4781 R-squared in test data

## linear model using all fields ##

model2 <- lm(Rating ~ Reviewscore 
             + Publisher 
             + Franchise
             + `Launch Year`
             + `Play Year`
             + `Main Category`
             + `DLC/ Major update played`
             + `Perspective`
             + `Played on`
             , data = train)

summary(model2) # Adjusted R-squared 0.6535 on training data

# identify any in the test set that were missing from train
missing_franchise <- setdiff(unique(test$Franchise), unique(train$Franchise))
# recode the missing as Other
test <- test %>% mutate(Franchise = fct_recode(Franchise, Other = missing_franchise[1]))

prediction <- predict(model2, test)
predict_df2 <- add_column(test, prediction)
cor(predict_df2$Rating, predict_df2$prediction)^2 # 0.4526 R-squared in test data
# overfitting going on?