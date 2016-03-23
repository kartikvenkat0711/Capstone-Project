
titanic <- read_excel(path = "C:/Springboard Data Science work/Titanic project/titanic_original.xls")

titanic$embarked[titanic$cabin == 'B28'] <- 'S'

age_mean <- mean(titanic$age)

age_people <- filter(titanic, age != 'NA')

age_people <- as.vector(age_people)
