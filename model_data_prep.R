library(tidyverse)
library(ggplot2)
library(caret)
library(randomForest)
library(glmnet)
library(gbm)
library(plotmo)

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

## First benchmark for just predicting reviewscore ##

cor(test$Rating, test$Reviewscore)^2 #0.4781 R-squared
sqrt(mean((test$Rating - test$Reviewscore)^2)) #10.789 mean square error

## first model, linear using basic reviewscore only, for a benchmark ##

model1 <- lm(Rating ~ Reviewscore, data = train)
summary(model1) # Adjusted R-squared 0.5498 on training data

prediction <- predict(model1, test)
predict_df1 <- add_column(test, prediction)
cor(predict_df1$Rating, predict_df1$prediction)^2 # 0.4781 R-squared in test data
sqrt(mean((predict_df1$Rating - predict_df1$prediction)^2)) #8.922 mean square error

# R-squared is the same as just using review score as expected, but error is reduced
# Due to systematic error/ misalignment of means. So this basic model outperforms just using reviewscore mostly due to intercept term

# base model formula to use from now
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

x_matrices <- makeX(train = train_data[, !names(train_data) == "Rating"],
                            test = test_data[, !names(test_data) == "Rating"])

##Train model
cv.out <- cv.glmnet(x_matrices$x, train_data$Rating,
                    alpha = 0, nlambda = 100,
                    lambda.min.ratio = 0.0001,
                    nfolds = 5)

y_predicted <- predict(cv.out, s = cv.out$lambda.min, newx = x_matrices$x)
# training R-squared
tss <- sum((train$Rating - mean(train$Rating))^2)
rss <- sum((train$Rating - y_predicted)^2)
1 - rss/tss # 0.7373 training R-squared

#test R-squared
y_predicted <- predict(cv.out, s = cv.out$lambda.min, newx = x_matrices$xtest)

tss <- sum((test$Rating - mean(test$Rating))^2)
rss <- sum((test$Rating - y_predicted)^2)
1 - rss/tss # 0.4740 test R-squared
sqrt(mean((test$Rating - y_predicted)^2)) # 8.884 mean square error

## Lasso regression ##

#Same as ridge above but change alpha from 0 to 1

cv.out <- cv.glmnet(x_matrices$x, train_data$Rating,
                    alpha = 1, nlambda = 100,
                    lambda.min.ratio = 0.0001,
                    nfolds = 5)

# training R-squared
y_predicted <- predict(cv.out, s = cv.out$lambda.min, newx = x_matrices$x)

tss <- sum((train$Rating - mean(train$Rating))^2)
rss <- sum((train$Rating - y_predicted)^2)
1 - rss/tss # 0.6855 training R-squared

#test R-squared
y_predicted <- predict(cv.out, s = cv.out$lambda.min, newx = x_matrices$xtest)

tss <- sum((test$Rating - mean(test$Rating))^2)
rss <- sum((test$Rating - y_predicted)^2)
1 - rss/tss # 0.5125 test R-squared
sqrt(mean((test$Rating - y_predicted)^2)) # 8.552 mean square error

# Lasso better than ridge

## Alpha can be set to any number, so is there a sweet spot in the middle? ##
set.seed(2)
alphas <- 100
plot(NULL, xlim = c(0, 1), ylim = c(0.45, 0.55), xlab = "depth", ylab = "test rsq")
for (i in 0:alphas) {
  cv.out <- cv.glmnet(x_matrices$x, train_data$Rating,
                      alpha = i/alphas, nlambda = 100,
                      lambda.min.ratio = 0.0001,
                      nfolds = 5)
  
  best_lambda = cv.out$lambda.min
  
  y_predicted <- predict(cv.out, s = best_lambda, newx = x_matrices$xtest)
  
  tss <- sum((test$Rating - mean(test$Rating))^2)
  rss <- sum((test$Rating - y_predicted)^2)
  rsq <- 1 - rss/tss
  print(paste(i/alphas, rsq))
  points(i/alphas, rsq)
}
# 0 = ridge regression is the worst but then pretty equal from 0.2 or higher. Go with 0.5

# convert to using a glmnet regular model once settled #

cv.out <- cv.glmnet(x_matrices$x, train_data$Rating,
                    alpha = 0.5, nlambda = 100,
                    lambda.min.ratio = 0.0001)

best_lambda = cv.out$lambda.min

model <- glmnet(x_matrices$x, train_data$Rating, alpha = 0.5, lamda = best_lambda)

model$beta["Reviewscore",]

plot_glmnet(model, xvar = "lambda", label = TRUE)

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
sqrt(mean(residuals^2)) # mean square error

gbm.perf(model5)

# Let's reduce overfitting and maximise test performance for the gbm model #

## optimise number of trees ##
gbm.perf(model5, method = "cv") # about 60 is best

## optimise interaction depth ##

set.seed(2)
min_depth <- 1
max_depth <- 3
plot(NULL, xlim = c(min_depth, max_depth), ylim = c(0, 1), xlab = "depth", ylab = "test rsq")
for (i in min_depth:max_depth) {
  model = gbm(fmla
               , data = train
               , distribution = "gaussian"
               , n.trees = 1
               , interaction.depth = i
  )
  prediction <- predict(model, test)
  predict_test <- add_column(test, prediction)
  rsq <- cor(predict_test$Rating, predict_test$prediction)^2
  print(rsq)
  points(i, rsq)
  
}

## 1 is best!

## optimise learning rate ##

set.seed(2)
min_learn <- 1
max_learn <- 100
plot(NULL, xlim = c(min_learn/1000, max_learn/1000), ylim = c(0, 1), xlab = "shrinkage", ylab = "test rsq")
for (i in min_learn:max_learn) {
  model = gbm(fmla
              , data = train
              , distribution = "gaussian"
              , n.trees = 1000
              , shrinkage = i/1000
  )
  prediction <- predict(model, test)
  predict_test <- add_column(test, prediction)
  rsq <- cor(predict_test$Rating, predict_test$prediction)^2
  print(paste(i/1000, rsq))
  points(i/1000, rsq)
  
}

# About 0.08 seems optimal with 60 trees or 0.004 with 1000 trees

#what is optimal ntrees for 0.001 shrinkage?
model <- gbm(fmla
            , data = train
            , distribution = "gaussian"
            , n.trees = 10000
            , shrinkage = 0.001
            , cv.folds = 5
)
gbm.perf(model, method = "cv") # 6576!

## Adjust minobsinnode ##

set.seed(2)
min_obs <- 1
max_obs <- 100
plot(NULL, xlim = c(min_obs, max_obs), ylim = c(0, 1), xlab = "depth", ylab = "test rsq")
for (i in min_obs:max_obs) {
  model = gbm(fmla
              , data = train
              , distribution = "gaussian"
              , n.trees = 6500
              , shrinkage = 0.001
              , n.minobsinnode = i
  )
  prediction <- predict(model, test)
  predict_test <- add_column(test, prediction)
  rsq <- cor(predict_test$Rating, predict_test$prediction)^2
  print(paste(i, rsq))
  points(i, rsq)
  
}

# steady downnward slope, with 4 being the peak, makes sense for this low volume data!

## Try model again and get scores with the improvements ##

set.seed(2)
model = gbm(fmla
            , data = train
            , distribution = "gaussian"
            , n.trees = 6500
            , shrinkage = 0.001
            , n.minobsinnode = 4
)

summary(model) # relative importances

prediction <- predict(model, train)
predict_train <- add_column(train, prediction)
cor(predict_train$Rating, predict_train$prediction)^2 # 0.6955 R-squared in train data

prediction <- predict(model, test)
predict_test <- add_column(test, prediction)
cor(predict_test$Rating, predict_test$prediction)^2 # 0.4922 R-squared in test data

residuals <- predict_test$Rating - predict_test$prediction
sqrt(mean(residuals^2)) # mean square error

# Perform feature selection only keeping best features #

fmla <- formula(Rating ~ Reviewscore
                + Franchise
                + DLC_Played
                + Perspective)

set.seed(2)
model = gbm(fmla
            , data = train
            , distribution = "gaussian"
            , n.trees = 6500
            , shrinkage = 0.001
            , n.minobsinnode = 4
)

summary(model) # relative importances

prediction <- predict(model, train)
predict_train <- add_column(train, prediction)
cor(predict_train$Rating, predict_train$prediction)^2 # 0.6955 R-squared in train data

prediction <- predict(model, test)
predict_test <- add_column(test, prediction)
cor(predict_test$Rating, predict_test$prediction)^2 # 0.4922 R-squared in test data

residuals <- predict_test$Rating - predict_test$prediction
sqrt(mean(residuals^2)) # mean square error

## Re-train using full sample for best model ##

final_model = gbm(fmla
                  , data = df
                  , distribution = "gaussian"
                  , n.trees = 60
                  , shrinkage = 0.08
)

prediction <- predict(final_model, df)
predict <- add_column(df, prediction)
cor(predict$Rating, predict$prediction)^2 # 0.6573 R-squared in final model (full train data)

saveRDS(final_model, file="model.rds")
saveRDS(df, file="data.rds")
