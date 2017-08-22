require(Amelia)
require(ggplot2)
require(dplyr)
setwd("E:/R-Practice/titanic/")
port <- read.csv(file = "train.csv",as.is=T,na.strings = "")
trap <- read.csv(file = "test.csv", as.is=T,na.strings = "")
trap$Survived <- "NA"
port$key <- "train"
trap$key <- "test"
comb <- rbind(port,trap)
comb$Surname <- sapply(as.character(comb$Name), FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})
comb$Survived <- factor(comb$Survived)

comb$SLogL <- rep(0,nrow(comb));
# define function to compute log likelihood of a/(1-a)
logl <- function(a) {
  a <- max(a,0.1); # avoids log(0)
  a <- min(a,0.9); # avoids division by 0
  return (log(a/(1-a)));
}

sapply(comb , function(x) sum(is.na(x))/length(x))
missmap(comb)

#filling in missing fare
comb$Fare[is.na(comb$Fare)] <- mean(comb$Fare,na.rm=T)

#filling in missing age
dat <- rnorm(n=sum(is.na(comb$Age)),mean=mean(comb$Age,na.rm=T),sd=6)
comb$Age[is.na(comb$Age)] <- dat

#filling in missing Embarked
comb$Embarked[is.na(comb$Embarked)] <- "S"

#adding relatives
comb$relatives <- comb$Parch+comb$SibSp
# for()

#converting Survived
comb$Survived[comb$key=="train"] <- as.numeric(comb$Survived[comb$key=="train"])-1

#filling in loglikelihood values
ff <- sort(unique(comb$Fare))

for (sex in c("male", "female")) {
 for (class in c(1:3)) {
   for(i in 1:length(ff)){
        comb$SLogL[comb$Sex==sex& comb$Pclass==class& comb$Fare==ff[i]] <- 
        logl(nrow(comb %>% filter(Survived==1, Sex==sex, Pclass==class, Fare==ff[i]))
             /max(nrow(comb %>% filter( key == "train",Sex==sex, Pclass==class, Fare==ff[i])),0.0001))
    }
  }
}

# 
# ggplot(comb, aes(Sex,fill=!is.na(Cabin))) + geom_bar(position="dodge") + labs(title="Passenger Has Cabin",fill="Has Cabin")
# 
# ggplot(comb %>% filter(key=="train"), aes(Fare)) + 
#   geom_density(alpha=0.5, aes(fill=factor(Survived))) + labs(title="Survival density per Age for Pclass ") + theme(panel.grid.major = element_line(colour = "black"),panel.grid.minor = element_line(colour = "gray"))
# 
# ggplot(comb %>% filter(key=="train"), aes(Sex)) + 
#   geom_bar(aes(fill=factor(Survived)),position="dodge") + labs(title="Survival density per Age for Pclass ") + theme(panel.grid.major = element_line(colour = "black"),panel.grid.minor = element_line(colour = "gray"))
# 
# 
ggplot(comb %>% filter(key=="train"), aes(x=SLogL, y=Survived)) + geom_jitter(aes(color=Survived)) +
  labs(title="SLogL vs Pclass vs TFreq")

 slogl1 <- comb$SLogL
# slogl2 <- comb$SLogL
# comb$SLogL <- slogl2


################# bias with age ########


ggplot(comb %>% filter(key=="train"), aes(Age)) + 
  geom_density(alpha=0.5, aes(fill=factor(Survived))) + labs(title="Survival density per Age for Pclass ") + theme(panel.grid.major = element_line(colour = "black"),panel.grid.minor = element_line(colour = "gray"))

comb$SLogL[comb$Age<9] <- comb$SLogL[comb$Age<9]+3

comb$SLogL[comb$Age>=9 & comb$Age<15] <- comb$SLogL[comb$Age>=9 & comb$Age<15]+1.2

comb$SLogL[comb$Age>=15 & comb$Age<18] <- comb$SLogL[comb$Age>=15 & comb$Age<18]-1.2

comb$SLogL[comb$Age>=18 & comb$Age<21] <- comb$SLogL[comb$Age>=18 & comb$Age<21]-2

comb$SLogL[comb$Age>=21 & comb$Age<29] <- comb$SLogL[comb$Age>=21 & comb$Age<29]-3

comb$SLogL[comb$Age>=29 & comb$Age<37] <- comb$SLogL[comb$Age>=29 & comb$Age<37]+1.2

comb$SLogL[comb$Age>=37 & comb$Age<42] <- comb$SLogL[comb$Age>=37 & comb$Age<42]+0.7

comb$SLogL[comb$Age>=42 & comb$Age<47] <- comb$SLogL[comb$Age>=42 & comb$Age<47]-1

comb$SLogL[comb$Age>=47 & comb$Age<58] <- comb$SLogL[comb$Age>=47 & comb$Age<58]+1.3

comb$SLogL[comb$Age>=58 & comb$Age<67] <- comb$SLogL[comb$Age>=58 & comb$Age<67]-1.1

comb$SLogL[comb$Age>=67 & comb$Age<74] <- comb$SLogL[comb$Age>=78]-1.6

comb$SLogL[comb$Age>=74] <- comb$SLogL[comb$Age>=74]+0.6

#################  bias with Pclass  ################# 

ggplot(comb %>% filter(key=="train"), aes(Pclass)) + 
  geom_density(alpha=0.5, aes(fill=factor(Survived))) + labs(title="Survival density per Age for Pclass ") + theme(panel.grid.major = element_line(colour = "black"),panel.grid.minor = element_line(colour = "gray"))

comb$SLogL[comb$Pclass==1] <- comb$SLogL[comb$Pclass==1] + 1

comb$SLogL[comb$Pclass==2] <- comb$SLogL[comb$Pclass==2] + 0.6

comb$SLogL[comb$Pclass==3] <- comb$SLogL[comb$Pclass==3] -1.4




#################  bias with Sex  #####################


ggplot(comb %>% filter(key=="train"), aes(Sex)) + 
  geom_bar(aes(fill=factor(Survived)),position="dodge") + labs(title="Survival density per Age for Pclass ") + theme(panel.grid.major = element_line(colour = "black"),panel.grid.minor = element_line(colour = "gray"))

comb$SLogL[comb$Sex=="female"] <- comb$SLogL[comb$Sex=="female"] + 2.2

comb$SLogL[comb$Sex=="male"] <- comb$SLogL[comb$Sex=="male"] - 3.5

#################  bias with Fare ####################

ggplot(comb %>% filter(key=="train"), aes(Fare)) + 
  geom_density(alpha=0.5, aes(fill=factor(Survived))) + labs(title="Survival density per Age for Pclass ") + theme(panel.grid.major = element_line(colour = "black"),panel.grid.minor = element_line(colour = "gray"))
# p2 <- ggplot(comb %>% filter(key=="train",Fare<50), aes(Fare)) + 
#   geom_density(alpha=0.5, aes(fill=factor(Survived))) + labs(title="Survival density per Age for Pclass ") + theme(panel.grid.major = element_line(colour = "black"),panel.grid.minor = element_line(colour = "gray"))
# p3 <- ggplot(comb %>% filter(key=="train",Fare>80 & Fare<120), aes(Fare)) + 
#   geom_density(alpha=0.5, aes(fill=factor(Survived))) + labs(title="Survival density per Age for Pclass ") + theme(panel.grid.major = element_line(colour = "black"),panel.grid.minor = element_line(colour = "gray"))
# grid.arrange(p1,p2,p3,ncol=2)

comb$SLogL[comb$Fare<13] <- comb$SLogL[comb$Fare<13]-2.5

comb$SLogL[comb$Fare>=13 & comb$Fare<22] <- comb$SLogL[comb$Fare>=13 & comb$Fare<22]+1.2

comb$SLogL[comb$Fare>=22 & comb$Fare<26] <- comb$SLogL[comb$Fare>=22 & comb$Fare<26]-0.4

comb$SLogL[comb$Fare>=26 & comb$Fare<80] <- comb$SLogL[comb$Fare>=26 & comb$Fare<80]+1.2

comb$SLogL[comb$Fare>=80 & comb$Fare<200] <- comb$SLogL[comb$Fare>=80 & comb$Fare<200]+1.5

comb$SLogL[comb$Fare>=200] <- comb$SLogL[comb$Fare>=200]+1.1

################## predictions  #######

baratrain <- comb[comb$key=="train",c("SLogL","Survived")]
baratrain$Survived <- as.numeric(baratrain$Survived)-1
baratest <- comb[comb$key=="test",c("SLogL","Survived")]


trControl <- trainControl(method="repeatedcv", number=7, repeats = 5); 
fms <- formula("Survived ~ SLogL")
model_m <- train(fms, data = comb %>% filter(key=="train"),
                 metric="Accuracy", trControl = trControl, method = "knn"); 
comb$Pred <- predict(model_m, comb)
print(model_m$results)



linear_model <- lm(Survived~SLogL,data=baratrain)
linear_predict <- predict(linear_model,data=comb$SLogL[comb$key=="train"])
linear_predict <- linear_predict - min(linear_predict)
linear_predict <- linear_predict/max(linear_predict)
thold <-0.3930461      #0.6416
filter <- as.numeric(linear_predict>thold)
x <- comb$Survived[comb$key=="train"]

# test = comb[comb$key=="test",]
# test_predict <- predict(linear_model, data = test)
# thold <- 0.4040038
# filter <- as.numeric(test_l_predict>thold)
# data <- data.frame(test[,1],filter)
# colnames(data) <- c('PassengerId','Survived')
# write.csv(data,file='l_predict3.csv',row.names=FALSE)




