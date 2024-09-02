library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(glmnet)
library(gbm)

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

# rename columns
name_lkp <- c(Launch_Platforms = "Launch Platform(s)",
              Played_On = "Played on",
              Launch_Year = "Launch Year",
              Play_Year = "Play Year",
              DLC_Played = "DLC/ Major update played",
              Category = "Main Category",
              Descriptors = "Extra Descriptors",
              Remaster_Reviewscore = "Remaster Reviewscore",
              Remaster_Year = "Remaster Release Year")

df <- rename(df, all_of(name_lkp))

## some quick exploratory plotting and analysis ##

#bubble plot
plot_data <- df %>% count(Reviewscore, Rating)
ggplot(plot_data, aes(x = Reviewscore, y = Rating, size = n)) +
  geom_point(alpha = 0.5)

#pearson correlation
cor(df$Rating, df$Reviewscore) # 0.730121

#convert the categoricals to factor
df$Played_On <- as_factor(df$Played_On)
df$DLC_Played <- as_factor(df$DLC_Played)
df$Publisher <- as_factor(df$Publisher)
df$Developer <- as_factor(df$Developer)
df$Franchise <- as_factor(df$Franchise)
df$Perspective <- as_factor(df$Perspective)
df$Category <- as_factor(df$Category)

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
group_factor(df, "Category", 3)

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

# base model formula
fmla <- formula(Rating ~ Reviewscore
+ Publisher 
+ Franchise
+ Launch_Year
+ Play_Year
+ Category
+ DLC_Played
+ Perspective
+ Played_On)

## linear model using all fields ##

model2 <- lm(fmla
             , data = train)

summary(model2) # Adjusted R-squared 0.6535 on training data

# identify any in the test set that were missing from train
missing_franchise <- setdiff(unique(test$Franchise), unique(train$Franchise))
# recode the missing as Other
test <- test %>% mutate(Franchise = fct_recode(Franchise, Other = missing_franchise[1]))

# ensure factor types are matching
levels(test$Franchise) <- levels(train$Franchise)
# need to apply the above to all factor variables for deployment to prevent any potential errors

prediction <- predict(model2, test)
predict_df2 <- add_column(test, prediction)
cor(predict_df2$Rating, predict_df2$prediction)^2 # 0.4526 R-squared in test data
# overfitting going on?

## random forest model ##

numtrees <- 1000
model3 <- randomForest(fmla
                       , data = train
                       , ntree = numtrees)
  
# train set r-squared
mean(model3$rsq) #0.4609 average of trees

prediction <- predict(model3, test)
predict_df3 <- add_column(test, prediction)
cor(predict_df3$Rating, predict_df3$prediction)^2 # 0.4546 R-squared in test data
residuals <- predict_df3$Rating - predict_df3$prediction
sqrt(mean(residuals^2)) # rss

## ridge regression model ##

train_data <- train[c("Rating",
                      "Reviewscore", 
                      "Publisher", 
                      "Franchise", 
                      "Launch_Year",
                      "Play_Year",
                      "Category",
                      "DLC_Played",
                      "Perspective",
                      "Played_On")]

test_data <- test[c("Rating",
                      "Reviewscore", 
                      "Publisher", 
                      "Franchise", 
                      "Launch_Year",
                      "Play_Year",
                      "Category",
                      "DLC_Played",
                      "Perspective",
                      "Played_On")]

x_matrices <- glmnet::makeX(train = train_data[, !names(train_data) == "Rating"],
                            test = test_data[, !names(test_data) == "Rating"])

###Calculate best lambda 
cv.out <- cv.glmnet(x_matrices$x, train_data$Rating,
                    alpha = 0, nlambda = 100,
                    lambda.min.ratio = 0.0001)

best_lambda = cv.out$lambda.min

model4 <- glmnet(x_matrices$x, train_data$Rating, alpha = 0, lamda = best_lambda)

# training R-squared
y_predicted <- predict(model4, s = best_lambda, newx = x_matrices$x)

tss <- sum((train$Rating - mean(train$Rating))^2)
rss <- sum((train$Rating - y_predicted)^2)
1 - rss/tss # 0.7373 training R-squared

#test R-squared
y_predicted <- predict(model4, s = best_lambda, newx = x_matrices$xtest)

tss <- sum((test$Rating - mean(test$Rating))^2)
rss <- sum((test$Rating - y_predicted)^2)
1 - rss/tss # 0.4725 test R-squared
# strong overfitting here

## Gradient boosted regressor ##

set.seed(2)
model5 = gbm(fmla
   , data = train
   , distribution = "gaussian"
   , n.trees = 100
   , cv.folds = 5
)
  
summary(model5) # relative importances

prediction <- predict(model5, train)
predict_train <- add_column(train, prediction)
cor(predict_train$Rating, predict_train$prediction)^2 # 0.6955 R-squared in train data
  
prediction <- predict(model5, test)
predict_test <- add_column(test, prediction)
cor(predict_test$Rating, predict_test$prediction)^2 # 0.4922 R-squared in test data

residuals <- predict_test$Rating - predict_test$prediction
sqrt(mean(residuals^2)) # rss

gbm.perf(model5)

# gbm probably best, let's reduce overfitting and maximise test performance #

## optimise number of trees ##
gbm.perf(model5, method = "cv") # about 60 is best

## optimise interaction depth ##

set.seed(2)
min_depth <- 1
max_depth <- 3
plot(NULL, xlim = c(min_depth, max_depth), ylim = c(0, 1), xlab = "depth", ylab = "test rss")
for (i in min_depth:max_depth) {
  model = gbm(fmla
               , data = train
               , distribution = "gaussian"
               , n.trees = 60
               , interaction.depth = i
  )
  prediction <- predict(model, test)
  predict_test <- add_column(test, prediction)
  rsq <- cor(predict_test$Rating, predict_test$prediction)^2
  print(rsq)
  points(i, rsq)
  
}

## 1 is best!

## Improve model formula by considering interaction terms ##

## Try model again and get scores with the improvements ##

set.seed(2)
model = gbm(fmla
             , data = train
             , distribution = "gaussian"
             , n.trees = 60
)

summary(model) # relative importances

prediction <- predict(model, train)
predict_train <- add_column(train, prediction)
cor(predict_train$Rating, predict_train$prediction)^2 # 0.6955 R-squared in train data

prediction <- predict(model, test)
predict_test <- add_column(test, prediction)
cor(predict_test$Rating, predict_test$prediction)^2 # 0.4922 R-squared in test data

residuals <- predict_test$Rating - predict_test$prediction
sqrt(mean(residuals^2)) # rss

## Re-train using full sample for best model ##

final_model = gbm(fmla
                  , data = df
                  , distribution = "gaussian"
                  , n.trees = 60
)

prediction <- predict(final_model, df)
predict <- add_column(df, prediction)
cor(predict$Rating, predict$prediction)^2 # 0.6573 R-squared in final model (full train data)

saveRDS(final_model, file="model.rds")
saveRDS(df, file="data.rds")
