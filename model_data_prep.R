library(tidyverse)

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

#convert the categoricals to factor
df$`DLC/ Major update played` <- as_factor(df$`DLC/ Major update played`)
df$Publisher <- as_factor(df$Publisher)
df$Developer <- as_factor(df$Developer)
df$Franchise <- as_factor(df$Franchise)
df$Perspective <- as_factor(df$Perspective)
df$`Main Category` <- as_factor(df$`Main Category`)

#group low volume categoricals into other for relevant fields
threshold <- 3
counts <- df %>% count(Publisher)
counts_joined <- df %>% left_join(counts, by = "Publisher")
df$Publisher <- fct_collapse(counts_joined$Publisher, Other = unique(counts_joined$Publisher[counts_joined$n < threshold]))


summary(df)
