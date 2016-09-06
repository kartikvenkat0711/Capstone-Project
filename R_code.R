
## 1. Getting started

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
library(ROCR)
library(xlsx)
library(caTools)
library(lattice)
library(ellipse)
library(corrplot)

# The following two commands remove any previously installed H2O packages for R.
if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("methods","statmod","stats","graphics","RCurl","jsonlite","tools","utils")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-turin/4/R")))
library(h2o)
localH2O = h2o.init(nthreads=-1)

## 2. Data Wrangling

## Load the full bank dataset

bankFull <- read.csv("bank-additional-full-clean.csv")

## Exclude variable "duration" from the dataset as it is said to be highly predictive in the problem statement

bankFull1 <- select(bankFull, -duration)

## Split this dataset into training and testing

set.seed(144)

split <- sample.split(bankFull1$y, SplitRatio = 0.8)

bankTrain <- subset(bankFull1, split == TRUE)

bankTest <- subset(bankFull1, split == FALSE)

cols <- c(1:21)

## Converting training and testing datasets into H20 objects

df.bankTrain <- as.h2o(bankTrain)

df.bankTest <- as.h2o(bankTest)

## 3. Exploratory Data Analysis
## In code below we look at relationships between our dependent variable, age of 
## the customer and other demographic factors

## We look at the "job" variable first to see if the result
## depends on any particular job category.
  
bankJobYes <- bankTrain %>%
  select(y, job) %>%
  filter(y == "yes")

plot(bankJobYes$job)

bankJobNo <- bankTrain %>%
  select(job, y) %>%
  filter(y == "no")

plot(bankJobNo$job)

## We noticed in our previous plots that there is a slight variation when
## the job category is "retired" or "student". Let us examine these variables
## in detail.

bankRetd <- bankTrain %>%
  select(job, y) %>%
  filter(job == "retired")

prop.table(table(bankRetd$y)) * 100

## We notice that for retired customers, the rate of "yes" response is significantly
## higher than the "no" response. Hence we can conclude that retired people will
## be more likely to subscribe to a deposit after being part of our campaign.

## Now let us look at the "student" job category distribution

bankStu <- bankTrain %>%
  select(job, y) %>%
  filter(job == "student")

prop.table(table(bankStu$y)) * 100

## We also notice that the rate of "yes" response among students is far higher
## than that of our population. Hence we conclude that students are much more
## likely to open a deposit account with the bank after they have been
## contacted by our campaign.

## Next we will see if the marital status of customer has any impact on our result.
 
bankMaritalYes <- bankTrain %>%
  select(y, marital, age) %>%
  filter(y == "yes")

plot(bankMaritalYes$marital)

bankMaritalNo <- bankTrain %>%
  select(y, marital, age) %>%
  filter(y == "no")

plot(bankMaritalNo$marital)

## We find that the distribution is almost similar except for a slight increase
## in the number of single people who said "yes". Let us examine the distribution
## for this group.

bankMaritalSingle <- bankTrain %>%
  select(y, marital, age) %>%
  filter(marital == "single")

prop.table(table(bankMaritalSingle$y)) * 100

## We find that only 14% of single customers said yes, which is only slightly
## higher than the proportion of people who said yes in the population. Thus
## we conclude that this variable does not have much of an impact on the result.

ggplot(bankTrain, aes(x = y, y = age)) + ## Comparing age against educational qualification
  geom_boxplot() +
  facet_grid(. ~ education)

bankEduYes <- bankTrain %>%
  select(y, education, age) %>%
  filter(y == "yes")

plot(bankEduYes$education)

bankEduNo <- bankTrain %>%
  select(y, education, age) %>%
  filter(y == "no")

plot(bankEduNo$education)

## We find that the distributions of "education" variable 
## are fairly similar for our result. Hence we conclude that this variable
## does not impact our result.

## Next we look at if credit default status has any impact on the result

bankDefYes <- bankTrain %>%
  select(y, default, age) %>%
  filter(y == "yes")

plot(bankDefYes$default)

bankDefNo <- bankTrain %>%
  select(y, default, age) %>%
  filter(y == "no")

plot(bankDefNo$default)

## Distribution similar for credit default status. Result does not depend on this variable.

## Compare dependent variable against housing loan status

bankHouYes <- bankTrain %>%
  select(y, housing, age) %>%
  filter(y == "yes")

plot(bankHouYes$housing)

bankHouNo <- bankTrain %>%
  select(y, housing, age) %>%
  filter(y == "no")

plot(bankHouNo$housing)

## We observe that the distributions are fairly the same for housing loan status
## and our result does not depend on housing loan status.

ggplot(bankTrain, aes(x = y, y = age)) + ## Comparing age against personal loan status
  geom_boxplot() +
  facet_grid(. ~ loan)

## Next we look at variables related to the last contact for the current campaign
## First we look at the mode of contact to see if it has any impact.

ggplot(bankTrain, aes(x = y)) +
  geom_bar(stat = "count")+
  facet_grid(. ~ contact)

bankContYes <- bankTrain %>%
  select(y, contact) %>%
  filter(y == "yes")

plot(bankContYes$contact)

bankContNo <- bankTrain %>%
  select(y, contact) %>%
  filter(y == "no")

plot(bankContNo$contact)

bankContCell <- bankTrain %>%
  select(y, contact) %>%
  filter(contact == "cellular")

prop.table(table(bankContCell$y)) * 100

## Looking at the contact type "cellular" we find that about 14.5% of people
## contacted using this mode were most likely to say yes.

ggplot(bankTrain, aes(x = y, y = age)) +
  geom_boxplot()+
  facet_grid(. ~ month)

bankMonYes <- bankTrain %>%
  select(y, month) %>%
  filter(y == "yes")

plot(bankMonYes$month)

bankMonNo <- bankTrain %>%
  select(y, month) %>%
  filter(y == "no")

plot(bankMonNo$month)

prop.table(table(bankTrain$y, bankTrain$month)) * 100

ggplot(bankTrain, aes(x = y, y = age)) +
  geom_boxplot()+
  facet_grid(. ~ day_of_week)

prop.table(table(bankTrain$y, bankTrain$day_of_week)) * 100

# Next we look at additonal variables related to this and the previous campaign

ggplot(bankTrain, aes(x = y, y = campaign)) +
  coord_cartesian(ylim = c(0,5)) +
  geom_boxplot()

pdaysYes <- bankTrain %>%
  select(pdays, y) %>%
  filter(y == "yes")

plot(pdaysYes$pdays)

prop.table(table(bankTrain$y, bankTrain$previous)) * 100

prop.table(table(bankTrain$y, bankTrain$poutcome)) * 100

prop.table(table(bankTrain$y, bankTrain$year)) * 100

ggplot(bankTrain, aes(x = y)) +
  geom_bar(stat = "count") +
  facet_grid(. ~ year)

## We wil also look at distributions of social and economic indicators.
## Social and economic indicators have been known to highly influence 
## consumer financial decisions

## First we look at the employment variance rate.

ggplot(bankTrain, aes(x = y, y = emp.var.rate)) +
  geom_boxplot()

empVarRateYes <- bankTrain %>%
  select(y, emp.var.rate) %>%
  filter(y == "yes") 

hist(empVarRateYes$emp.var.rate)

## The histogram shows that when the employment variance rate is less than 0
## there are more people saying yes than when it is greater than 0.

ggplot(bankTrain, aes(x = y, y = cons.price.idx)) +
  geom_boxplot()

bankConsConfYes <- bankTrain %>%
  select(y, cons.price.idx) %>%
  filter(y == "yes")

hist(bankConsConfYes$cons.price.idx)

bankConsPriNo <- bankTrain %>%
  select(y, cons.price.idx) %>%
  filter(y == "no")

hist(bankConsPriNo$cons.price.idx)

ggplot(bankTrain, aes(x = y, y = cons.conf.idx)) +
  geom_boxplot()

ggplot(bankTrain, aes(x = y, y = euribor3m)) +
  geom_boxplot()

euriNo <- bankTrain %>%
  select(euribor3m, y) %>%
  filter(y == "no") %>%
  filter(euribor3m >= 4)

hist(euriNo$euribor3m)

euriYes <- bankTrain %>%
  select(euribor3m, y) %>%
  filter(y == "yes") %>%
  filter(euribor3m <= 2)

hist(euriYes$euribor3m)

## Looking at the euribor quarterly rate, it can be safely said that
## when the euribor rate is below 2, more customer are likely to subscribe to
## a deposit compared to when the rate is above 4 when most customers are likely to
## say no.

ggplot(bankTrain, aes(x = y, y = nr.employed)) +
  geom_boxplot()

bankEmpRateYes <- bankTrain %>%
  select(y, nr.employed) %>%
  filter(y == "yes")

hist(bankEmpRateYes$nr.employed)

bankEmpRateNo <- bankTrain %>%
  select(y, nr.employed) %>%
  filter(y == "no")

hist(bankEmpRateNo$nr.employed)

## We see that when the number of employees is below 5100 more people are
## likely to say yes compared to when it is close 5200 when people are likely 
## not to subscribe to a deposit.

ggplot(bankTrain, aes(x = y, y = gold.price)) +
  geom_boxplot()

bankGoldYes <- bankTrain %>%
  select(y, gold.price) %>%
  filter(y == "yes")

hist(bankGoldYes$gold.price)

bankGoldNo <- bankTrain %>%
  select(y, gold.price) %>%
  filter(y == "no")

hist(bankGoldNo$gold.price)

## 4. Building the model

## 4.1 - Logistic model

model1 <- h2o.glm(y = "y", x = cols, training_frame = df.bankTrain, 
                  family = "binomial", nfolds = 0, alpha = 0.5, lambda_search = FALSE)

summary(model1)

scoreImp <- h2o.varimp(model1)

View(scoreImp)

## Prediction using model created

predictTrain <- h2o.predict(model1, newdata = df.bankTest)

summary(predictTrain)

1140/(7098 + 1140)

## We see from the logistic model that it gives close to 15% of our customers as
## responding with a yes. This is better than the 11% that is the proportion
## of the population responding with a yes.

## 4.2 - Create a Random Forest model

model2 <- h2o.randomForest(y = "y", x = cols, training_frame = df.bankTrain)

summary(model2)

scoreImp1 <- h2o.varimp(model2)

View(scoreImp1)

predictTrain1 <- h2o.predict(model2, newdata = df.bankTest)

summary(predictTrain1)

1034/(1034 + 7204)

## We see from the RF model analysis that it gives close to 12.5% of our customers as
## responding with a yes. This is slightly better than the 11% that is the proportion
## of the population responding with a yes.

## 4.3 - create a Gradient Boosted model

model3 <- h2o.gbm(y = "y", x = cols, training_frame = df.bankTrain, distribution = "bernoulli")

summary(model3)

scoreImp2 <- h2o.varimp(model3)

View(scoreImp2)

## Prediction using Gradient Boosted model

predictTrain2 <- h2o.predict(model3, newdata = df.bankTest)

summary(predictTrain2)

808/(7430 + 808) * 100

## We see from the GB model analysis that it gives about 9.8% of our customers as
## responding with a yes. This is worse than the 11% that is the proportion
## of the population responding with a yes.
