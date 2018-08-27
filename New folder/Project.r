setwd("C:/Users/AKSHATA SHELAR/Documents/R/data analysis/iris_Dataset")
dataset=read.csv('iris.csv')
View(dataset)
head(dataset)
dim(dataset)
names(dataset)
class(dataset)
is.na(dataset)
str(dataset)
summary(dataset)
x <- dataset[,1:4]
y <- dataset[,5]
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(iris)[i])
}
##table(iris$Species)
table(iris$Species)
pie(table(iris$Species),)
dev.off()
sepalLength6 <- subset(dataset, species == "virginica" & sepal_length > 6)
tail(sepalLength6) 
hist(dataset$sepal_width)
hist(dataset$petal_length)


x <- sepalLength6[,1:4]
y <- sepalLength6[,5]
par(mfrow=c(1,4))
for(i in 1:4) {
  boxplot(x[,i], main=names(sepalLength6)[i])
}

petalLength6 <- subset(dataset, species == "virginica")
petalLengthOnly<-petalLength6[,3]
str(petalLengthOnly)
hist(petalLengthOnly)

petalWidth6 <- subset(dataset, species == "setosa")
petalWidthOnly<-petalWidth6[,4]
str(petalWidthOnly)
hist(petalWidthOnly)

print(cor(dataset$sepal_length,dataset$sepal_width))
print(cor(dataset$petal_length,dataset$petal_width))


type <- levels(dataset$species)
print(type[1])
cor(dataset[dataset$species==type[1],1:4])

print(type[2])
cor(dataset[dataset$species==type[3],1:4])

print(type[3])
cor(dataset[dataset$species==type[3],1:4])

###
#validation_index <- createDataPartition(dataset$species, p=0.80, list=FALSE)
# select 20% of the data for validation
#validation <- dataset[-validation_index,]
# use the remaining 80% of data to training and testing the models
#dataset <- dataset[validation_index,]
#install.packages("rattle")
#library(rattle)
#library(class)
#set.seed(7)
#fit.knn <- train(species~., data=dataset, method="knn", metric=metric, trControl=control)
#library(caTools)
library(gmodels)
#dataset<-dataset[-1]
View(dataset)
split = sample.split(dataset$species, SplitRatio = 0.80)
# Split based off of split Boolean Vector 
train = subset(dataset, split == TRUE) 
test = subset(dataset, split == FALSE)
str(train)
traindata<-train[-5]
head(traindata)
testdata<-test[,-5]
train_labels <- train$species
test_labels <- test$species
library(class)
prc_test_pred <- knn(train = traindata, test = testdata,cl = train_labels, k=10)

CrossTable(x=test_labels,y=prc_test_pred,prop.chisq=FALSE)
















library(party)


library(rpart.plot)
print(head(dataset))
input.dat<-dataset[c(1:105),]
fm=species~sepal_length+sepal_width+petal_length+petal_width
fit<-rpart(formula=fm,input.dat,method='class')
rpart.plot(fit)












