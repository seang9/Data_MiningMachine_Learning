##Packages
install.packages("gmodels")
install.packages("C50")
library(C50)
library(gmodels)
help(gmodels)



##reading in data
dataset <- read.csv("data/insurance.csv", stringsAsFactors = T)
View(dataset)
help("summary")

#dataset$Sex <- NULL
#dataset$Height<-NULL


##visualization data
summary(dataset)
pairs(dataset) ##scatterplots
plot(dataset$age,dataset$bmi)
cor(dataset[c("age", "bmi", "children", "expenses")])
pairs(dataset[c("age", "bmi", "children", "expenses")])

hist(dataset$expenses, breaks = 20)


##Modelling
model <- lm(expenses ~ ., data = dataset)
summary(model)
##improved Model
dataset$age2 <- dataset$age^2
dataset$bmi30 <- ifelse(dataset$bmi >= 30, 1, 0)
ins_model2 <- lm(expenses ~ age + age2 + children + bmi + sex +
                   bmi30*smoker + region, data = dataset)

summary(ins_model2)
