#Set working directory

getwd()

setwd('C:/Springboard Data Science work/datasets')

#Load required packages

library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(Hmisc)
library(mice)
library(ROCR)
library(caTools)
library(caret)

## Install h2o

if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }

if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

pkgs <- c("methods","statmod","stats","graphics","RCurl","jsonlite","tools","utils")

for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-turin/4/R")))

## Load h2o

library(h2o)

localH2O = h2o.init(nthreads=-1) ## Initialize h2o instance

## Load the full bank dataset

bankFull <- read.csv("bank-additional-full-clean.csv")

View(bankFull)

## Exclude variable "duration" from the dataset as it is said to be highly predictive in the problem statement

bankFull1 <- select(bankFull, -duration)

str(bankFull1)

colSums(is.na(bankFull1)) ## Test for missing values

## Distribution of variables in dataset

head(bankFull1)

corBank <- cor(df.bankTest)

View(corBank)

corBank1 <- cor(df.hex$year, df.hex$y)

View(corBank1)

## How is the education variable distributed in this dataset?

ggplot(bankFull1, aes(x = month, fill = y)) +
  geom_bar(stat = "count", position = "dodge")
  ##facet_grid(year ~ .)

ggplot(bankFull1, aes(x = job, fill = y)) +
  geom_bar(stat = "count", position = "dodge")
##facet_grid(year ~ .)

ggplot(bankFull1, aes(x = euribor3m, fill = y)) +
  geom_histogram(binwidth = 5)
##facet_grid(year ~ .)

ggplot(bankFull1, aes(x = loan, fill = y)) +
  geom_bar(stat = "count", position = "dodge")

## Split this dataset into training and testing

set.seed(144)

split <- sample.split(bankFull1$y, SplitRatio = 0.8)

bankTrain <- subset(bankFull1, split == TRUE)

bankTest <- subset(bankFull1, split == FALSE)

## Create glm model for training dataset

df.hex <- as.h2o(bankTrain)

df.bankTest <- as.h2o(bankTest) ## Convert test data in h20 frame object

cols <- c(1:21)

model2 <- h2o.glm(y = "y", x = cols, training_frame = df.hex,
                  family = "binomial", nfolds = 0, alpha = 0.5, lambda_search = FALSE)

summary(model2)

## Predictions made for test set

predictBank2 <- h2o.predict(model2, newdata = df.bankTest)

View(predictBank2)

## Create Random Forest model

model3 <- h2o.randomForest(y = "y", x = cols, training_frame = df.hex,
              nfolds = 0)

summary(model3)

## Prediction using Random Forest model

predictBank3 <- h2o.predict(model3, newdata = df.bankTest)

View(predictBank3)

## Create GBM model

model4 <- h2o.gbm(y = "y", x = cols, training_frame = df.hex, distribution = "bernoulli")

summary(model4)

gainLift <- h2o.gainsLift(model4, df.hex) ## Generate gain lift metrics

View(gainLift)

predictBank4 <- h2o.predict(model4, newdata = df.bankTest)

View(predictBank4)
