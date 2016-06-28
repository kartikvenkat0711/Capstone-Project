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

## Load training data as new data frame

bankTest <- read.csv("bank-additional.csv")

str(bankTest)

## Logistic regression model with marital and job as inputs

model1 <- glm(y ~ ., data = bankFull1, family = binomial)

summary(model1)

predictBank <- predict(model1, type = "response")

ROCRpred1 <- prediction(predictBank, bankFull1$y)

ROCRperf1 <- performance(ROCRpred1, "tpr", "fpr")

ROCRauc1 <- performance(ROCRpred1, "auc")

ROCRauc1@y.values ## AUC keeping the missing values

plot(ROCRperf1, colorize = TRUE)

## End of prediction for data w/ missing values

summary(predictBank)

table(bankFull1$job, bankFull1$y)

model3 <- glm(y ~ housing, data = bankFull1, family = binomial)

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

