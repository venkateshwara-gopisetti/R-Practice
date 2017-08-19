require(corrplot)
require(class)
require(ridge)
library(readr)
hdata <- read_csv("~/ML repo/week2[july31-aug4]/kc_house_data.csv")
View(hdata)

cor <- cor(hdata[,-c(1,2,3,17)])
corrplot ( cor , method = "color", type = "lower")

hdata1 <- hdata[1:1500,]
model <- linearRidge(hdata$price ~., lambda = seq(0 ,0.1 ,0.001),data = hdata)
model1 <- lm.ridge(hdata$price ~ hdata$bedrooms+hdata$bathrooms, lambda = seq(0 ,0.1 ,0.001))
plot(model)
legend(0,125000,c("bedrooms","bathrooms"),lty=c(1,1), lwd=c(2.5,2.5), col=c("cyan","red"))
