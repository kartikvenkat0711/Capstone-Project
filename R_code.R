#Set working directory

getwd()

setwd('C:/R work/data_sets')

#Load required packages

library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(Hmisc)
library(mice)
library(jsonlite)
library(ROCR)
library(xlsx)

## Load the full bank dataset

bankFull <- read.csv("bank-additional-full-clean.csv")

View(bankFull)

## Exclude variable "duration" from the dataset as it is said to be highly predictive in the problem statement

bankFull1 <- select(bankFull, -duration)

View(bankFull1)

## Split this dataset into training and testing

set.seed(144)

split <- sample.split(bankFull$y, SplitRatio = 0.8)

bankTrain <- subset(bankFull, split == TRUE)

bankTest <- subset(bankFull, split == FALSE)

## Load training data as new data frame

bankTest <- read.csv("bank-additional.csv")

str(bankTest)

## Logistic regression model with marital and job as inputs
 
model1 <- glm(y ~ ., data = bankTrain, family = binomial) ## Create model

## summary(model1)

predictBank <- predict(model1, type = "response", newdata = bankTest) ## Predict model parameters

## summary(predictBank)

ROCRpred1 <- prediction(predictBank, bankFull1$y) 

ROCRperf1 <- performance(ROCRpred1, "tpr", "fpr")

ROCRauc1 <- performance(ROCRpred1, "auc")

ROCRauc1@y.values ## AUC keeping the missing values

plot(ROCRperf1, colorize = TRUE)

table(bankFull1$y, predictBank > 0.54)

971/(971+3669) ## Sensitivity

36094/(36094+454) ## Specificity

971/(971+454) ## True positive rate

3669/(36094+3669)

## End of prediction for data w/ missing values

summary(predictBank)

table(bankFull1$job, bankFull1$y)

model3 <- glm(y ~ year, data = bankFull1, family = binomial)

summary(model3)

summary(bankFull1$job)

bankFull2 <- bankFull1 %>%
  arrange(marital) %>%
  slice(1:41108)

table(bankFull3$job, bankFull3$y)

summary(bankFull2$marital)

bankFull3 <- bankFull2 %>%
  arrange(marital) %>%
  slice(1:40787)

View(bankFull3)

summary(bankFull3$marital)

## Model for data w/o missing values

model4 <- glm(y ~ ., data = bankFull3, family = binomial)

coef(summary(model4))

predictTrain <- predict(model4, type = "response")

table(bankFull3$y, predictTrain > 0.5)

2053/(2053+2541) ## Sensitivity

35650/(35650+543) ## Specificity

ROCRpred <- prediction(predictTrain, bankFull3$y)

ROCRperf <- performance(ROCRpred, "tpr", "fpr")

ROCRauc <- performance(ROCRpred, "auc")

ROCRauc@y.values ## AUC w/o missing values

plot(ROCRperf, colorize = TRUE)

## End of model for data w/o missing values

