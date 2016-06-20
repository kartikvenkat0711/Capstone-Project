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

## Load the full bank dataset

bankFull <- read.csv("bank-additional-full.csv", sep = ';')

View(bankFull)

## Exclude variable "duration" from the dataset as it is said to be highly predictive in the problem statement

bankFull1 <- select(bankFull, -duration)

str(bankFull1)

## Logistic regression model with marital and job as inputs

model1 <- glm(y ~ marital + job, data = bankFull, family = binomial)

summary(model1)

## Add year field to dataset

## Assigning year to records - problem statement mentions year range from May 2008 to Nov 2010

bankFull <- bankFull %>%
  slice(1:27691) %>%
  mutate(year = 2008)

bankFull <- bankFull %>%
  slice(27692:39131) %>%
  mutate(year = 2009)

bankFull <- bankFull %>%
  slice(39132:41189) %>%
  mutate(year = 2010)

View(bankFull)








