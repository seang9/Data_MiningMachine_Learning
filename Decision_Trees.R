##Installing Packages
install.packages('C50')
install.packages('gmodels')
install.packages('caret')
install.packages('e1071', dependencies=TRUE)
library(C50)
library(gmodels)
library(caret)
library(e1071)

## Reading in Data 
cars <- read.csv("data/car_evaluation.csv")

str(cars)
summary(cars)
set.seed(1)

##Training and Test Data
cars_rand <- cars[order(runif(1728)),]
cars_train <- cars_rand[1:1200,]
cars_test <- cars_rand[1201:1728,]

prop.table(table(cars_train$Condition))
prop.table(table(cars_test$Condition))

##str(cars_train$Condition)
cars_train$Condition<-as.factor(cars_train$Condition)

##Modelling
model <- C5.0(cars_train$Condition ~ ., data = cars_train)
model
plot(model)
summary(model)

## Analysing and visualizing data
predictions <- predict(model, cars_test)
summary(predictions)

View(cars_train)
table <- CrossTable(predictions, cars_test$Condition,
                    prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                    dnn = c('predicted default', 'actual default'))
library(caret)
cars_test$Condition<-as.factor(cars_test$Condition)
str(cars_train$Condition)
confusionMatrix(table(predictions, cars_test$Condition))


