## Set wd
setwd("C:/Users/andre/downloads")

## Load package
library(survival)
library(caret)
library(mlbench)
library(tidyverse)
library(boot)
library(car)
library(DALEX)
library(modelStudio)

## Load data
data = read.csv("cll_tim_compound_not_hot.csv")

## Data curation
data = subset(data, select=-c(X,INFEC_days_after_diag))

data$Binet_Stage = as.factor(data$Binet_Stage)
data$Binet_Stage = addNA(data$Binet_Stage)

data$IGHV_unmut = as.factor(data$IGHV_unmut)
data$IGHV_unmut = addNA(data$IGHV_unmut)

data$ECOG = as.factor(data$ECOG)
data$ECOG = case_when(
  data$ECOG %in% c("0") ~ "0",
  data$ECOG %in% c("1") ~ "1",
  data$ECOG %in% c("2") ~ "2",
  data$ECOG %in% c("3","4","5") ~ "3")
data$ECOG = as.ordered(data$ECOG)

data$FamCLL = as.factor(data$FamCLL)
data$FamCLL = addNA(data$FamCLL)

data$Beta2m = as.factor(data$Beta2m)
data$Beta2m = addNA(data$Beta2m)

data$CD38 = as.factor(data$CD38)
data$CD38 = addNA(data$CD38)

data$ZAP70 = as.factor(data$ZAP70)
data$ZAP70 = addNA(data$ZAP70)

data$Gender = as.factor(data$Gender)

data$del13 = as.factor(data$del13)
data$del13 = addNA(data$del13)

data$del11 = as.factor(data$del11)
data$del11 = addNA(data$del11)


data$tri12 = as.factor(data$tri12)
data$tri12 = addNA(data$tri12)

data = data %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

set.seed(6575)
training.samples <- data$INFEC_after_diag %>% 
  createDataPartition(p = 0.80, list = FALSE)
train.data  <- data[training.samples, ]
test.data <- data[-training.samples, ]

modelglm <- glm(INFEC_after_diag ~ ., data = train.data, family = binomial)
# Binet_Stage + IGHV_unmut + ECOG + FamCLL + Beta2m + CD38 + ZAP70 + Gender + del13

summary(model)

probabilities <- modelglm %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

mean(predicted.classes == test.data$INFEC_after_diag)

explainer1 <- explain(modelglm,
                      data=test.data,
                      y = test.data$INFEC_after_diag,
                      label='glm')

obs <- test.data[1:10,]

modelStudio(explainer1,obs)

modelrf = ranger::ranger(INFEC_after_diag~., data = train.data, classification = TRUE, probability = TRUE)
modelrf

probabilities <- modelrf %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

mean(predicted.classes == test.data$INFEC_after_diag)

explainer2 <- explain(modelrf,
                      data=test.data,
                      y = test.data$INFEC_after_diag,
                      label='rf')

modelStudio(explainer2,obs)

modelgbm = gbm::gbm(INFEC_after_diag~., data = train.data, distribution = "bernoulli")
modelgbm

probabilities <- modelgbm %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

mean(predicted.classes == test.data$INFEC_after_diag)

explainer3 <- explain(modelgbm,
                      data=test.data,
                      y = test.data$INFEC_after_diag,
                      label='gbm')

modelStudio(explainer3,obs)
