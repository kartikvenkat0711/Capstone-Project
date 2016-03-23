
titanic <- read_excel(path = "C:/Springboard Data Science work/Titanic project/titanic_original.xls")

titanic$embarked[titanic$cabin == 'B28'] <- 'S'

titanic$age[is.na(titanic$age)] <- 0

age_mean <- mean(titanic$age)

age_mean

titanic$age[titanic$age == 0] <- age_mean

titanic$boat[is.na(titanic$boat)] <- 'NA'
