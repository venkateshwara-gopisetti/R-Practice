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
set.seed(123)
x <- rnorm(sum(is.na(port$Age)),mean(port$Age,na.rm = TRUE),sd(port$Age,na.rm = TRUE))
x <- round(abs(x),1)
x[x%%1>0.5] <- ceiling(x[x%%1>0.5])
x[x%%1<0.5] <- floor(x[x%%1<0.5])
x[x%%1>0.5] <- ceiling(x[x%%1>0.5])
port$Age[is.na(port$Age)] <- x

# filling in na in embarked
port$Embarked[is.na(port$Embarked)] <- 0

# linear model
l_model <- lm(Survived ~.,data=port)
l_predict <- predict(l_model,port)
l_predict <- l_predict-min(l_predict)
l_predict <- l_predict/max(l_predict)
thold <- summary(l_predict)[5]
filter = l_predict > thold
l_predict[filter] = 1
l_predict[!filter] = 0

# logistics model
log_model <- glm(Survived ~.,data=port,family=binomial)
log_predict <- predict(log_model,port)

# knn model


log_predict <- log_predict-min(log_predict)
log_predict <- log_predict/max(log_predict)


  


