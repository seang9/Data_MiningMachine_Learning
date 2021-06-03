##Installing packages 
install.packages('factoextra')
library("foreign")
library("factoextra")
factoextra

##reading in the data 
seed <- read.csv("data/Seed_Data.csv")
View(seed)
# scaling numerical data
set.seed(4)
seed_z <- scale(seed[,-8])
# rename column and correcting data type
names(seed) <- c("Area", "Perimeter", "Compactness", "Length", "Width", "Asymetry.coef", "Grove.length", "Type")
seed$Type <- as.factor(seed$Type)


##Modelling data 
model <- kmeans(seed, 3)
model
model$cluster
model$tot.withinss
model$centers
seed$Type

##visualization of the data
table(seed$Type, model$cluster)
plot(seed[c("Length", "Width")], col = model$cluster)
# plot cluster centers
points(model$centers[,c("Length", "Width")], col = 1:3, pch = 8, cex=2)
seed$cluster <- as.factor(seed_k$cluster)
head(seed)
fviz_cluster(object = seed_k, 
             data = seed_z)