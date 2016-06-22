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

str(bankFull1)

## Logistic regression model with marital and job as inputs

model1 <- glm(y ~ ., data = bankFull1, family = binomial)

coef(summary(model1))
