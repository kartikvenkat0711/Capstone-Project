#Set working directory

getwd()

setwd('C:/R work/data_sets')

#Load required packages

install.packages("devtools")

library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(Hmisc)
library(mice)
library(ROCR)
library(xlsx)
library(caTools)

## Load the full bank dataset

bankFull <- read.csv("bank-additional-full-clean.csv")

View(bankFull)

## Exclude variable "duration" from the dataset as it is said to be highly predictive in the problem statement

bankFull1 <- select(bankFull, -duration)

View(bankFull1)

## Split this dataset into training and testing

set.seed(144)

split <- sample.split(bankFull1$y, SplitRatio = 0.8)

bankTrain <- subset(bankFull1, split == TRUE)

bankTest <- subset(bankFull1, split == FALSE)

## Create new dataset with subset of variables

bankTrain1 <- bankTrain[, 12:22]

str(bankTrain1)

bankTest1 <- bankTest[, 12:22]

str(bankTest1)

## Logistic regression model with marital and job as inputs

model1 <- glm(y ~ ., data = bankTrain1, family = binomial) ## Create model

summary(model1)

predictBank <- predict(model1, type = "response", newdata = bankTest1) ## Predict model parameters

str(predictBank)

bankTest1$y <- as.vector(bankTest1$y)

summary(bankTest1$y)

length(predictBank) == length(bankTest1$y)

ROCRpred1 <- prediction(predictBank, bankTest1$y) 

ROCRperf1 <- performance(ROCRpred1, "tpr", "fpr")

ROCRauc1 <- performance(ROCRpred1, "auc")

ROCRauc1@y.values ## AUC keeping the missing values

plot(ROCRperf1, colorize = TRUE)

table(bankTest1$y, predictBank > 0.5)

171/(171+757) ## Sensitivity

7223/(7223+87) ## Specificity

171/(171+87) ## Precision

3669/(36094+3669)

(171+7223)/8238 ## Accuracy

predictBank <- as.vector(predictBank)

View(predictBank)

bankTest2 <- mutate(bankTest1, predictBank)

str(bankTest2)

testOrder <- arrange(bankTest2, desc(predictBank))

View(testOrder)

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
