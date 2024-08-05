library(tidyverse)
library(ggplot2)

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
smp_size <- floor(0.75 * nrow(df))
train_ind <- sample(seq_len(nrow(df)), size = smp_size) #randomly select row indexes for training set

train <- df[train_ind, ] #select all rows with the training indexes
test <- df[-train_ind, ] #select all rows not with the training indexes

## first model, using basic reviewscore only for a benchmark ##

model <- lm(Rating ~ Reviewscore, data = train)
summary(model) # Adjusted R-squared 0.5234 on training data

prediction <- predict(model, test)

predict_df <- add_column(test, prediction)
cor(predict_df$Rating, predict_df$prediction)^2 # 0.5823 R-squared in test data
