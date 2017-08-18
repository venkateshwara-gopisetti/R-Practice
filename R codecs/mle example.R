# there are n number of crates in a day
# each crate contains 2000 bottles
# we sample random 100 crates
# and note the number of defective bottles
library(stats4)
set.seed(123)
# x <- rnorm(100,mean = 3 , sd = 2)
x <- sample(0:80,40)
LL <- function(mu, sigma) {
          R = dnorm(x, mu, sigma)
          -sum(log(R))
}
mle(LL, start = list(mu = mean(x), sigma= sd(x)))

means <- sort(seq(floor((mean(x)-1)),ceiling((mean(x)+1)),0.1))
sds <- sort(seq(floor((sd(x)-1)),ceiling((sd(x)+1)),0.1))
sds <- sds[sds>0]
table <- matrix ( nrow = length(sds) , ncol = length(means) )
for(i in 1:length(sds)){
  for(j in 1:length(means)){
    table[i,j] <- LL(means[j],sds[i])
  }
}
ind <- which(table == min(table),arr.ind = TRUE)
cat("mean is:",means[ind[2]])
cat("sd is:",sds[ind[1]])
table2 <- exp(-table)
ind <- which(table2 == max(table2),arr.ind = TRUE)
cat("mean is:",means[ind[2]])
cat("sd is:",sds[ind[1]])

require(plot3D)
persp3D(z = table, theta = 30 , phi = 10)
hist3D(z = table2, theta = 30 , phi = 30)

ci(means[ind[2]],sds[ind[1]],n=2000,level=99)

# for (i in 1:180){
#   png(filename=paste("mle",formatC(i,width=4,flag="0"),".png",sep=""))
#   hist3D(z = table2, theta = (80+2*i) , phi = 60,xlab = "sd",ylab = "mean" , zlab = "likelihood" )
#   dev.off()
# }