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
missmap(data, main = "Missing values vs observed")
str(titanic_train)

# hotcoding gender  female = 0, male = 1
data$Sex <- replace(data$Sex,data$Sex=='female',0)
data$Sex <- replace(data$Sex,data$Sex=='male',1)
data$Sex <- as.numeric(data$Sex)

# hotcoding embarked  C = 0, Q - 1,S = 2
data$Embarked <- replace(data$Embarked,data$Embarked=='C',0)
data$Embarked <- replace(data$Embarked,data$Embarked=='Q',1)
data$Embarked <- replace(data$Embarked,data$Embarked=='S',2)
data$Embarked <- as.numeric(data$Embarked)
data$Embarked[is.na(data$Embarked)] <- 0


corrplot(cor(data))
sapply(sample,function(x) sum(is.na(x)))
sapply(sample, function(x) length(unique(x)))


a <- list()

for(i in 1:nrow(predicted)) {
  a <- rbind(a, names(predicted[i,which.max(predicted[i, ])]))
}