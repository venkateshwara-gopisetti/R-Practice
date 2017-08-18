require(stats)
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
port$Embarked[is.na(port$Embarked)] <- 0

# filling in missing ages
# set.seed(123)
# x <- rnorm(sum(is.na(port$Age)),mean(port$Age,na.rm = TRUE),sd(port$Age,na.rm = TRUE))
# x <- round(abs(x),1)
# x[x%%1>0.5] <- ceiling(x[x%%1>0.5])
# x[x%%1<0.5] <- floor(x[x%%1<0.5])
# x[x%%1>0.5] <- ceiling(x[x%%1>0.5])
# port$Age[is.na(port$Age)] <- x
port$Age[is.na(port$Age)] <- mean(port$Age[!is.na(port$Age)])

logit_model <- glm(Survived ~ Fare*Pclass + Sex + Age + SibSp + Parch + Embarked, data = port, family = binomial )
logit_predict <- predict(logit_model,port[,-1])
logit_predict <- logit_predict - min(logit_predict)
logit_predict <- logit_predict/max(logit_predict)
thold <- signif(seq(summary(logit_predict)[5]-3,summary(logit_predict)[5]+3,0.1),6)
thold <- thold[thold>0&thold<1]
matrix <- data.frame(matrix(nrow=length(logit_predict)))
filter <- c()
for(i in 1:length(thold)){
  filter <- logit_predict > thold[i]
  filter <- as.numeric(filter)
  matrix <- cbind(matrix,filter)
}
matrix <- matrix[,-1]
accuracy <- sapply(matrix,function(x) sum(x==port[,1]))/891

newt <- seq(0.5300170,0.731677,0.0002)
newt <- newt[newt>0&newt<1]
matrix <- data.frame(matrix(nrow=length(logit_predict)))
filter <- c()
for(i in 1:length(newt)){
  filter <- logit_predict > newt[i]
  filter <- as.numeric(filter)
  matrix <- cbind(matrix,filter)
}
matrix <- matrix[,-1]
accuracy <- sapply(matrix,function(x) sum(x==port[,1]))/891

thold <- 0.602017
filter = logit_predict > thold
filter <- as.numeric(filter)
sum(filter == port[,1])/891
