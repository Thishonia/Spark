#THISHONIA PREETHI
#TSF DS TASK 4
library("rpart")
library("party")
library("rpart.plot")
mydata=read.csv("iris.csv")
str(iris)
set.seed(522)

# runif function returns a uniform distribution which can be further conditionally split into 75-25 ratio
iris[, 'train'] <- ifelse(runif(nrow(iris)) < 0.75, 1, 0)

trainSet <- iris[iris$train == 1,]
testSet <- iris[iris$train == 0, ]

trainColNum <- grep('train', names(trainSet))

trainSet <- trainSet[, -trainColNum]
testSet <- testSet[, -trainColNum]

treeFit <- rpart(Species~.,data=trainSet,method = 'class')
rpart.plot(treeFit, box.col=c("red", "green"))


tree<-rpart(Species ~ SepalLengthCm + SepalWidthCm + PetalLengthCm+ PetalWidthCm,
            data=mydata,
            method="class")
a<-data.frame(SepalLengthCm=c(4.3), SepalWidthCm=c(3.0), PetalLengthCm=c(1.1), PetalWidthCm=c(0.1))
result<-predict(tree,a)
result
#1-yes 0-no
#correctly predicted as setosa
iris_ctree <- ctree(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data=iris)
print(iris_ctree)
plot(iris_ctree)
plot(iris_ctree, type="simple")