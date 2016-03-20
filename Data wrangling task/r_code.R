install.packages("readxl")
install.packages("readr")
install.packages("rvest")

library(readxl)

#Import data file

x <- read_excel(path = "C:/Springboard Data Science work/refine_original.xlsx")

library(dplyr)

library(tidyr)

y <- tbl_df(x)

y$company[y$company == 'Phillips'] <- 'philips'

y$company[y$company == 'phillips'] <- 'philips'

y$company[y$company == 'phllips'] <- 'philips'

y$company[y$company == 'phillps'] <- 'philips'

y$company[y$company == 'phillipS'] <- 'philips'

y$company[y$company == 'Akzo'] <- 'akzo'

y$company[y$company == 'AKZO'] <- 'akzo'

y$company[y$company == 'akz0'] <- 'akzo'

y$company[y$company == 'ak zo'] <- 'akzo'

y$company[y$company == 'Akzo'] <- 'akzo'

y$company[y$company == 'fillips'] <- 'philips'

y$company[y$company == 'phlips'] <- 'philips'

y$company[y$company == 'Van Houten'] <- 'van houten'

y$company[y$company == 'van Houten'] <- 'van houten'

y$company[y$company == 'Van Houten'] <- 'van houten'

y$company[y$company == 'unilver'] <- 'unilever'

y$company[y$company == 'Unilever'] <- 'unilever'

View(y)

y$address <- separate(y, address, c("line_1","line_2"))

y <- separate(y, product_name, c("Product_code","number"))

select(y, "Product code / number")

rename(y, Product code/number = Product code number)

str(y)

View(y)

colnames(y)

names(y)[2] <- "product_name"

y <- as.data.frame(y)

names(y)[2]<-"new_name"

y <- mutate(y, product_category = Product_code)

replace(y, y$product_category == 'p', 'Smartphone')

y$product_category[y$product_category == 'p'] <- 'Smartphone'

y$product_category[y$product_category == 'v'] <- 'TV'

y$product_category[y$product_category == 'x'] <- 'Laptop'

y$product_category[y$product_category == 'q'] <- 'Tablet'

y <- unite(y, col = full_address, address:country, sep = ",")

y$company_philips <- ifelse(y$company == "philips",1,0)

y$company_akzo <- ifelse(y$company == "akzo",1,0)

y$company_vanhouten <- ifelse(y$company == "van houten",1,0)

y$company_unilever <- ifelse(y$company == "unilever",1,0)

y$product_smartphone <- ifelse(y$product_category == "Smartphone",1,0)

y$product_tv <- ifelse(y$product_category == "TV",1,0)

y$product_laptop <- ifelse(y$product_category == "Laptop",1,0)

y$product_tablet <- ifelse(y$product_category == "Tablet",1,0)

write.csv(y, file = "C:\\Springboard Data Science work\\Data wrangling task\\refine_clean.csv")
