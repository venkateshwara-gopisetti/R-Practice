sapply(titanic_test,function(x) sum(is.na(x)))
sapply(titanic_test, function(x) length(unique(x)))


test <- subset(titanic_test,select = c(1,2,4,5,6,7,9,11))
par(mfrow=c(1,2))
missmap(test, main = "Missing values vs observed")


# hotcoding gender  female = 0, male = 1
test$Sex <- replace(test$Sex,test$Sex=='female',0)
test$Sex <- replace(test$Sex,test$Sex=='male',1)
test$Sex <- as.numeric(test$Sex)

# hotcoding embarked  C = 0, Q - 1,S = 2
test$Embarked <- replace(test$Embarked,test$Embarked=='C',0)
test$Embarked <- replace(test$Embarked,test$Embarked=='Q',1)
test$Embarked <- replace(test$Embarked,test$Embarked=='S',2)
test$Embarked <- as.numeric(test$Embarked)

# filling in missing ages
set.seed(123)
x <- rnorm(sum(is.na(test$Age)),mean(test$Age,na.rm = TRUE),sd(test$Age,na.rm = TRUE))
x <- round(abs(x),1)
x[x%%1>0.5] <- ceiling(x[x%%1>0.5])
x[x%%1<0.5] <- floor(x[x%%1<0.5])
x[x%%1>0.5] <- ceiling(x[x%%1>0.5])
test$Age[is.na(test$Age)] <- x
missmap(test, main = "Missing values vs observed")

#filling in na
test$Fare[is.na(test$Fare)]<- mean(test$Fare[!is.na(test$Fare)])

test_rpart_predict <- predict(rpart_model,test)

thold <- 0.473744
filter <- test_rpart_predict > thold
filter <- as.numeric(filter)
data <- data.frame(test[,1],filter)
colnames(data) <- c('PassengerId','Survived')
write.csv(data,file='rpart_predict2.csv',row.names=FALSE)

