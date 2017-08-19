
TN <- TAD[TAD$`STATE/UT` == "Tamil Nadu",]
data<-TN[TN$TYPE == "Road Accidents",'TOTAL']
x <- unique(TN$YEAR)
y <- as.matrix(data)
rel <- lm(y[,1]~x)
lines(x,y,abline(rel,col="green"),col="orange")

mars.fit <- mars(x,y[,1])
# SHOW CUT POINTS OF MARS
xr <- range(x)

for (i in 1:14)
{
  xp <- seq(xr[1], xr[2])
  xf <- predict(mars.fit, xp)
  plot( xp,xf ,  xlab = names(laksh)[i], ylab = "", type = "l",col="green",pch=15)

}