require(Amelia)
require(corrplot)
library(readr)
titanic_train <- read_csv("~/train.csv")
titanic_test <- read_csv("~/test.csv")


sapply(titanic_train,function(x) sum(is.na(x)))
sapply(titanic_train, function(x) length(unique(x)))

#subsetting
data <- subset(titanic_train,select=c(2,3,5,6,7,8,10,12))

# visualization of missing values
par(mfrow=c(1,2))
missmap(titanic_train, main = "Missing values vs observed")
missmap(data, main = "Missing values vs observed")

#dealing with missing values
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

# visualization of missing values
par(mfrow=c(1,2))
missmap(titanic_train, main = "Missing values vs observed")
missmap(data, main = "Missing values vs observed")
str(titanic_train)

sample <- subset(titanic_train,select = c(1,2,3,6,7,8,10))
corrplot(cor(sample))
sapply(sample,function(x) sum(is.na(x)))
sapply(sample, function(x) length(unique(x)))

test <- subset(titanic_test,select = c(1,2,5,6,7,9))
# logistics model
log_model <- glm(Survived~.,data = sample[,-4] , family = binomial)
# PCA 
pca_model <- prcomp(sample[,-4])
biplot(pca_model, scale = 0)

# predicting for training data
prediction <- predict(log_model,sample[,-2])
lth <- (max(prediction)-min(prediction))
ask <- (prediction-min(prediction))/lth







