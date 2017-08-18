require(gbm)
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
missmap(port)
corrplot(cor(port))

#gmb model
gbm_model <- gbm(Survived~.,data = port ,n.trees = 5000,train.fraction = 0.8)
gbm_predict <- predict(gbm_model,port[,-1])
gbm_predict <- gbm_predict-min(gbm_predict)
gbm_predict <- gbm_predict/max(gbm_predict)
thold <- seq(0.3155069-0.3,0.3155069+0.3,0.01)
thold <- thold[thold>0&thold<1]
matrix <- data.frame(matrix(nrow=length(gbm_predict)))
filter <- c()
for(i in 1:length(thold)){
  filter <- gbm_predict > thold[i]
  filter <- as.numeric(filter)
  matrix <- cbind(matrix,filter)
}
matrix <- matrix[,-1]
accuracy <- sapply(matrix,function(x) sum(x==port[,1]))/891
