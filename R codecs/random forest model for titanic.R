require(randomForest)
port <- subset(titanic_train,select=c(2,3,5,6,7,8,10,12))


# hotcoding gender  female = 0, male = 1
port$Sex <- replace(port$Sex,port$Sex=='female',0)
port$Sex <- replace(port$Sex,port$Sex=='male',1)
port$Sex <- as.numeric(port$Sex)

# hotcoding embarked  C = 0, Q - 1,S = 2
port$Embarked <- replace(port$Embarked,port$Embarked=='C',0)
port$Embarked <- replace(port$Embarked,port$Embarked=='Q',1)
port$Embarked <- replace(port$Embarked,port$Embarked=='S',2)
port$Embarked <- as.numeric(port$Embarked)


# filling in missing ages
# set.seed(123)
# x <- rnorm(sum(is.na(port$Age)),mean(port$Age,na.rm = TRUE),sd(port$Age,na.rm = TRUE))
# x <- round(abs(x),1)
# x[x%%1>0.5] <- ceiling(x[x%%1>0.5])
# x[x%%1<0.5] <- floor(x[x%%1<0.5])
# x[x%%1>0.5] <- ceiling(x[x%%1>0.5])
# port$Age[is.na(port$Age)] <- x
port$Age[is.na(port$Age)] <- mean(port$Age[!is.na(port$Age)])

# filling in na in embarked
port$Embarked[is.na(port$Embarked)] <- 0
missmap(port)
corrplot(cor(port))

# randomforest model
rf_model <- randomForest(as.factor(Survived) ~.,data = port , ntree = 200)
rf_predict <- predict(rf_model,port[,-1])
sum((as.numeric(rf_predict)-1)==port[,1])/891




