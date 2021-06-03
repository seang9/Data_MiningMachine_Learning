##Installing packages
install.packages('gmodels')
install.packages('class')
install.packages('caret')
library(gmodels)
library(class)
library(caret)

##Reading in data
banknotes <- read.csv("data/banknote_authentication.csv")
View(banknotes)
summary(banknotes)
set.seed(1)

##Training and Test Data
notes_rand <- banknotes[order(runif(1372)),]
notes_z <- as.data.frame(scale(notes_rand[-5]))
notes_train <- notes_z[1:1000,]
notes_test <- notes_z[1001:1372,]
notes_train_labels <- notes_rand[1:1000, 5]
notes_test_labels <- notes_rand[1001:1372, 5]

#Class Attribute 0 = Real, 1 = Fake/Counterfeit


##K=3
predictions <- knn(train = notes_train, test = notes_test, 
                   cl = notes_train_labels, k=3)
CrossTable(predictions, notes_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
confusionMatrix(table(predictions ,notes_test_labels))

##K=5
predictions <- knn(train = notes_train, test = notes_test, 
                   cl = notes_train_labels, k=5)
CrossTable(predictions, notes_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
confusionMatrix(table(predictions ,notes_test_labels))

##K=7
predictions <- knn(train = notes_train, test = notes_test, 
                   cl = notes_train_labels, k=7)
CrossTable(predictions, notes_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
confusionMatrix(table(predictions ,notes_test_labels))

##K=9
predictions <- knn(train = notes_train, test = notes_test, 
                   cl = notes_train_labels, k=9)
CrossTable(predictions, notes_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
confusionMatrix(table(predictions ,notes_test_labels))

##K=11
predictions <- knn(train = notes_train, test = notes_test, 
                   cl = notes_train_labels, k=11)
CrossTable(predictions, notes_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
confusionMatrix(table(predictions ,notes_test_labels))

##K=13
predictions <- knn(train = notes_train, test = notes_test, 
                   cl = notes_train_labels, k=13)
CrossTable(predictions, notes_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
confusionMatrix(table(predictions ,notes_test_labels))

##K=19
predictions <- knn(train = notes_train, test = notes_test, 
                   cl = notes_train_labels, k=19)
CrossTable(predictions, notes_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)
confusionMatrix(table(predictions ,notes_test_labels))







