require(earth)
require(stats)
require(plyr)
laksh<-rain[rain$SUBDIVISION == "LAKSHADWEEP",-c(1,15:19)]
laksh <- replace(laksh,is.na(laksh),0)

 
 x <- laksh$YEAR
 y <- laksh[,2]
# ## spliting into train/test

smp_size <- floor(0.79 * nrow(y))
set.seed(123)
train_ind <- sample(seq_len(nrow(y)), size = smp_size)
train_ind <- sort(train_ind,decreasing = FALSE)
train <- laksh[train_ind,1:2 ]
test <- laksh[-train_ind,1:2 ]

# FIT AN ADDITIVE MARS MODEL
  mars.fit <-earth(train[,1], train[,2])
  par(mfrow = c(3,4), mar=c(2, 2, 2, 2), pty="s")
  plot(x,as.matrix(y),xlim=c(1890,2020),ylim=c(0,300))
  plot(train,xlim=c(1890,2020),ylim=c(0,300),col="red")
  xt <- predict(mars.fit, train[,1])
  plot(t(as.matrix(train[,1])),xt,type = "p",col="green",xlim=c(1890,2020),ylim=c(0,300))
  
  plot(test,xlim=c(1890,2020),ylim=c(0,300),col="blue")
  
  xf <- predict(mars.fit, test[,1])
  plot(t(as.matrix(test[,1])),xf,type = "p",col="green",xlim=c(1890,2020),ylim=c(0,300))

  rel <- lm(as.matrix(train[,2])~as.matrix(train[,1]))
  
  plot(as.matrix(train[,1]),as.matrix(train[,2]),abline(rel),xlim=c(1890,2020),ylim=c(0,300))
  
  t<-test[,1]
  xp <- predict(rel,t)
  
  tt <- t(as.matrix(train[,1]))
  
  plot(t(as.matrix(test[,1]))[1,],xp[1:24],col="green",xlim=c(1890,2020),ylim=c(0,300))

  
  
  
  
  
  
  
