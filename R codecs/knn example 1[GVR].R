data("iris")
require(stats)
require(class)
require(corplot)
require(gmodels)

summary(iris)
#set.seed(331)
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
trainData <- iris[ind==1,]
testData <- iris[ind==2,]
#removing factorvariable from training and test datasets
traindata1 <- trainData[-5]
testdata1 <- testData[-5]


# data_norm <- function(x){ ((x-min(x))/(max(x)-min(x)))}
# iris_norm <- as.data.frame(sapply(iris[,-5],data_norm))
# traindata2 <- iris_norm[ind==1,-5]
# testdata2  <- iris_norm[ind==2,-5]

model1 <- knn(train = traindata1, test = testdata1 , cl=trainData$Species , k = 3, prob = TRUE)
model2 <- knn(train = traindata1, test = testdata1 , cl=trainData$Species , k = 6, prob = TRUE)

#CrossTable(x = testData$Species, y = model1 ,prop.chisq=FALSE)
mean(testData$Species == model1)
#CrossTable(x = testData$Species, y = model2 ,prop.chisq=FALSE)
mean(testData$Species == model2)