library(RColorBrewer)
library(sp)
x <- trees
plot(x$Girth,x$Volume)
# lowess
points(lowess(x$Girth,x$Volume), col="red")
lines(lowess(x$Girth,x$Volume), col="red", lwd=2)

# different smoothing
lines(lowess(x$Girth,x$Volume, f=0.33), col="magenta", lwd=2)
lines(lowess(x$Girth,x$Volume, f=0.80), col="green", lwd=2)

# loess -- first model
loess.model <- loess(x$Volume~x$Girth)
loess.model
# second model, smoother curve
loess.model2 <- loess(x$Volume~x$Girth, span=0.90, degree=2)
loess.model2
plot(loess.model)
plot(x$Volume~x$Girth)
hat1 <- predict(loess.model)
lines(x$Girth[order(x$Girth)], hat1[order(x$Girth)], col="red", lwd=2)
hat2 <- predict(loess.model2)
lines(x$Girth[order(x$Girth)], hat2[order(x$Girth)], col="blue", lwd=2)