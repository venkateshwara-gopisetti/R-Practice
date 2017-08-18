titanic_test$Survived <- NA;
titanic_test$Set  <- "Test";
titanic_train$Set <- "Train";
sample <- rbind(titanic_train,titanic_test)

# filling na in age
sample$Age[is.na(sample$Age)] <- mean(sample$Age,na.rm=T)
# hotcoding gender  female = 0, male = 1
sample$Sex <- replace(sample$Sex,sample$Sex=='female',0)
sample$Sex <- replace(sample$Sex,sample$Sex=='male',1)
sample$Sex <- as.numeric(sample$Sex)

# hotcoding embarked  C = 0, Q - 1,S = 2
sample$Embarked <- replace(sample$Embarked,sample$Embarked=='C',0)
sample$Embarked <- replace(sample$Embarked,sample$Embarked=='Q',1)
sample$Embarked <- replace(sample$Embarked,sample$Embarked=='S',2)
sample$Embarked <- as.numeric(sample$Embarked)
sample$Embarked[is.na(sample$Embarked)] <- 0

# filling in missing Fare
sample$Fare[is.na(sample$Fare)] <- mean(sample$Fare,na.rm=T)

#grouping by last names
names <- c()
for(i in 1:nrow(sample)){
  names <- rbind(names,strsplit(sample$Name[i]," ")[[1]][1])
}
sample$Name <- names
data <-sample[order(sample$Name),]

# hotcoding names
uname <- sort(unique(names))
for(i in 1:length(uname)){
  data$Name[data$Name == uname[i]] <- i
}
data$Name = as.numeric(data$Name)

# grouping by fares
fares <- sort(unique(data$Fare))

# filling in cabins
for(i in 1:length(cabins)){
  data$Cabin[data$Fare == data$Fare[data$Cabin==cabins[i]]] <- cabins[i]
}
data$Fare[data$Cabin==cabins[i]]

for(i in 1:nrow(data)) {
  if(!is.na(data$Cabin[i])){
    data$Cabin[i] = strsplit(data$Cabin[i], "")[[1]][1]
  }
}

knn_train <- data[!is.na(data$Cabin),c(1,3,4,5,6,7,8,10,11,12)]
knn_train1 <- knn_train[order(knn_train$Fare),]
knn_test <- data[is.na(data$Cabin),c(1,3,4,5,6,7,8,10,11,12)]
knn_test <- knn_test[order(knn_test$Fare),]
knn_test1 <- knn_test[1:50,]
count <- 0
for(i in 1:20){
  model123 <- knn3(Cabin~.,data = knn_train1)
  predicted <- predict(model123,knn_test1)
  knn_test1$Cabin <- apply(predicted,1,function(x) names(which.max((x))))
  knn_train1 <- rbind(knn_train1,knn_test1)
  knn_test1 <- knn_test[((50*i)+1):(50*(i+1)),]
  count <- count+1
}
knn_test1 <- knn_test[1001:1013,]
model123 <- knn3(Cabin~.,data = knn_train1)
predicted <- predict(model123,knn_test1)
knn_test1$Cabin <- apply(predicted,1,function(x) names(which.max((x))))
knn_train1 <- rbind(knn_train1,knn_test1)
knn_train2 = knn_train1[order(knn_train1$PassengerId),]
data2 = data[order(data$PassengerId),]
data2$Cabin <- knn_train2$Cabin

# hotcoding cabins
cabins <- sort(unique(data$Cabin))
for(i in 1:length(cabins)){
  data2$Cabin[data2$Cabin==cabins[i]] <- i
}
data2$Cabin <- as.numeric(data2$Cabin)
data2 <- data2[,-9]


train <- data2[data2$Set=='Train',-12]
test <- data2[data2$Set=='Test',-12]

#####    linear model
l_model <- lm(Survived~.,data = train)
l_predict <- predict(l_model,train)
l_predict <- l_predict-min(l_predict)
l_predict <- l_predict/max(l_predict)

thold <- signif(seq(summary(l_predict)[5]-3,summary(l_predict)[5]+3,0.1),6)
thold <- thold[thold>0&thold<1]
matrix <- data.frame(matrix(nrow=length(l_predict)))
filter <- c()
for(i in 1:length(thold)){
  filter <- l_predict > thold[i]
  filter <- as.numeric(filter)
  matrix <- cbind(matrix,filter)
}
matrix <- matrix[,-1]
accuracy <- sapply(matrix,function(x) sum(x==train[,2]))/891
newt <- seq(0.4483400,0.6483440,0.0002)
newt <- newt[newt>0&newt<1]
matrix <- data.frame(matrix(nrow=length(l_predict)))
filter <- c()
for(i in 1:length(newt)){
  filter <- l_predict > newt[i]
  filter <- as.numeric(filter)
  matrix <- cbind(matrix,filter)
}
matrix <- matrix[,-1]
accuracy <- sapply(matrix,function(x) sum(x==train[,2]))/891

thold <- 0.5969733
filter = l_predict > thold
filter <- as.numeric(filter)
sum(filter == train[,2])/891

test_l_predict <- predict(l_model,test)
filter = test_l_predict > thold
test_l_predict[filter] = 1
test_l_predict[!filter] = 0
data <- data.frame(test[,1],test_l_predict)
colnames(data) <- c('PassengerId','Survived')
write.csv(data,file='l_predict4[14august].csv',row.names=FALSE)


#####   random forest

rf_model <- randomForest(as.factor(Survived) ~.,data = train , ntree = 200)
rf_predict <- predict(rf_model,train)
sum((as.numeric(rf_predict)-1)==train[,2])/891

test_rf_predict <- predict(rf_model,test)
data <- data.frame(test[,1],test_rf_predict)
colnames(data) <- c('PassengerId','Survived')
write.csv(data,file='rf_predict3[august14].csv',row.names=FALSE)

#####  logistics regression

logit_model <- glm(Survived~.,data = train,family='binomial')
logit_predict <- predict(logit_model,)
#####   elstic net
enet.fit <- glmnet(as.matrix(sapply(train[,-2], as.numeric)),as.matrix(sapply(train[,2], as.numeric)),family = 'binomial',alpha = 1)
bara <- predict(enet.fit,train)
plot(enet.fit, xvar="lambda")






