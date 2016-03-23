
library(dplyr)

library(readxl)

library(tidyr)

library(readr)

#Load data in RStudio
titanic <- read_excel(path = "C:/Springboard/titanic_original.xls")

#Replace missing value
titanic$embarked[titanic$cabin == 'B28'] <- 'S'

#Calculate mean and replace NA values
titanic$age[is.na(titanic$age)] <- 0

age_mean <- mean(titanic$age)

titanic$age[titanic$age == 0] <- age_mean

#Fill empty slots with dummy value
titanic$boat[is.na(titanic$boat)] <- 'NA'

#Create new cabin number column
titanic$has_cabin_number <- ifelse(is.na(titanic$cabin),0,1)

write_csv(titanic, path = "C:/Springboard/titanic_clean.csv")
