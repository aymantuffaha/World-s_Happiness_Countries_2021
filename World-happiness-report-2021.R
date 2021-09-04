##############################################################################
# This dataset include happiness index for the year 2021 according to UN. 
# Datasets can be found here: 
# https://www.kaggle.com/muhammedabdulazeem/worlds-happiness-countries-2021
# https://www.kaggle.com/ajaypalsinghlo/world-happiness-report-2021?select=world-happiness-report.csv
# For more information, Please can visit the below URL:
# https://worldpopulationreview.com/country-rankings/happiest-countries-in-the-world
###############################################################################

# import libraries
library(tidyverse)
library(caret)
library(DescTools)
library(ggcorrplot)


# ----- GOAL -----
# 1- Take a deep analysis of the happiness index
# 2- To make a prediction model that will help us predict happiness
# 3- Score based on multiple variables, and check if population of the country 
#    affects the happiness score.




# ----- Import dataset -----
data <- read.csv("UpdatedDataSet.csv")

# ----- Explore and visualize data -----

# shows an organized section of data frame
glimpse(data)


# shows structure of the data frame
str(data)


# shows desxcriptive statistics of each category,
#  should show everything you need
summary(data)


# shows highest 10 scores
head(data, n=10)


# shows lowest 10 scores
tail(data, n=10)


# histogram of scores
hist(data$score, freq=TRUE, col="black", border="white", 
     main="2021 Happiness Scores", xlab="Score", ylab="Count")

# plot population vs score
ggplot(data = data, aes(x = score, y = pop2021)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

# plot gdp per cap vs score
ggplot(data = data, aes(x = score, y = Logged.GDP.per.capita)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

# plot generosity vs score
ggplot(data = data, aes(x = score, Generosity)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

# plot life expectancy vs score
ggplot(data = data, aes(x = score, Healthy.life.expectancy)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE)

# Thus, moving forward, let us explore the correlation between the other 
# predictors (population, GDP per capital, Social support, Health life expectancy, freedom to make life choices, etc.) and happiness score
temp <- data[, c(3,4,5,6,7,8,9,10)]
cormat <- signif(cor(temp), 2)
ggcorrplot(cormat)


# ----- Summation model -----
# find  predicted score by sum method and calculate the corresponding RMSE
sum_model <- data %>% mutate(pred_score = pop2021 +
                                Logged.GDP.per.capita +
                                Social.support +
                                Healthy.life.expectancy +
                                Freedom.to.make.life.choices + 
                                Generosity + 
                                Perceptions.of.corruption + 
                               1.85, 
                             RMSE = RMSE(score, pred_score))
sum_model
# show top results of the summation model
sum_model %>%
  filter(happinessRank <= 5) %>%
  select(happinessRank, country, score, pred_score, RMSE)



# ----- Develop generalized linear model -----
# using generalized linear model to predict score based on
#  all other data points


# --- test for an appropriate probability in data partitioning
ps <- seq(from=.30, to=.90, by=.01)

rmses <- sapply(ps, function(p){
  train_index <- createDataPartition(data$score,
                                     times=1,
                                     p=p,
                                     list=FALSE)
  train <- data[train_index,]
  test <- data[-train_index,]
  fit <- glm(score ~ pop2021 +
               Logged.GDP.per.capita +
               Social.support +
               Healthy.life.expectancy + 
               Freedom.to.make.life.choices + 
               Generosity + 
               Perceptions.of.corruption, 
             data12 = train)
  test <- test %>% 
    mutate(pred_score = predict.glm(fit, newdata=test))
  RMSE(test$score, test$pred_score)
})
rmses
# no real clear winner in terms of best accuracy in probabilities
plot(ps, rmses_1)
ps[which.min(rmses)]
min(rmses)

rm(ps, rmses)


# ----- Data partitioning -----
# just using p=0.70
train_index <- createDataPartition(data$Score, times=1, p=0.70, list=FALSE)
train <- data[train_index,]
test <- data[-train_index,]



# --- back to our model, p used above in data separation
fit <- glm(score ~ pop2021 +
             Logged.GDP.per.capita +
             Social.support +
             Healthy.life.expectancy + 
             Freedom.to.make.life.choices + 
             Generosity + 
             Perceptions.of.corruption, 
             data1 = train)

# add predicted scores to our 'data' data frame
results <- test %>% 
  mutate(pred_score = predict.glm(fit, newdata=test))

# plot predicted scores vs actual scores
# also plot 1 to 1 line
ggplot(data = results, aes(score, pred_score)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(color='red')

# print rmse
RMSE(results$score, results$pred_score)

# print coefficients of fitted model
fit$coefficients



# ----- try fit again without population
fit_full_2 <- glm(Score ~ Logged.GDP.per.capita +
                          Social.support +
                          Healthy.life.expectancy + 
                          Freedom.to.make.life.choices + 
                          Generosity + 
                          Perceptions.of.corruption, 
                          data = train)

# add predicted scores to our 'data' data frame
results <- test %>% 
  mutate(pred_score = predict.glm(fit_full_2, newdata=test))

# plot predicted scores vs actual scores
# also plot 1 to 1 line
ggplot(data = results, aes(Score, pred_score)) + 
  geom_point(color='black') +
  geom_smooth(method = "lm", se = TRUE) +
  geom_abline(color='red')


# ----- using rmse instead of cor: https://www.r-bloggers.com/dont-use-correlation-to-track-prediction-performance/
# print residual sum of squares
RMSE(results$Score, results$pred_score)

# print coefficients of fitted model
fit_full_2$coefficients