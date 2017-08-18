dat <- c(1,2,3)
rbind(
  prod( dnorm( dat, mean=1.5, sd=1 ) ),
  prod( dnorm( dat, mean=2, sd=1 ) ),
  prod( dnorm( dat, mean=2.5, sd=1 ) )
)

dat <- c(1,2,3)
mean_grid <- seq(0, 4, by=0.1) ## values of the mean to check the likelihood at
myLikelihood <- rep(0, length(mean_grid) )
for( i in seq_along( myLikelihood ) ) {
  myLikelihood[i] <- prod( dnorm( dat, mean = mean_grid[i], sd=1 ) )
}
plot( myLikelihood ~ mean_grid, type="b" )

#Suppose we flip a biased coin that comes up heads 25% of the time. 
#If it's heads, we draw from a Normal distribution with mean 1, standard deviation 1. 
#If it's tails, we draw from a Normal distribution with mean 7, standard deviation 1. We repeat this process 1000 times.
set.seed(123)
tau_1_true <- 0.25
x <- y <- rep(0,1000)
for( i in 1:1000 ) {
  if( runif(1) < tau_1_true ) {
    x[i] <- rnorm(1, mean=1)
    y[i] <- "heads"
  } else {
    x[i] <- rnorm(1, mean=7)
    y[i] <- "tails"
  }
}

densityPlot( ~x)

tau_1 <- 0.5 ## our initial believed proportion from dist. 1, chosen arbitrarily
tau_2 <- 0.5 ## our initial believed proportion from dist. 2, chosen arbitrarily

T_1 <- tau_1 * dnorm( x[1], mean=0 )
T_2 <- tau_2 * dnorm( x[1], mean=1 )

print(T_1)
T_1 <- tau_1 * dnorm( x, mean=0 )
T_2 <- tau_2 * dnorm( x, mean=1 )

head( T_1 / (T_1 + T_2) )

P_1 <- T_1 / (T_1 + T_2)
P_2 <- T_2 / (T_1 + T_2)

mu_1 <- sum( P_1 * x ) / sum(P_1)
mu_2 <- sum( P_2 * x ) / sum(P_2)

c(mu_1, mu_2)

## set the initial guesses for the distribution parameters
mu_1 <- 0
mu_2 <- 1

## as well as the latent variable parameters
tau_1 <- 0.5
tau_2 <- 0.5

for( i in 1:10 ) {
  
  ## Given the observed data, as well as the distribution parameters,
  ## what are the latent variables?
  
  T_1 <- tau_1 * dnorm( x, mu_1 )
  T_2 <- tau_2 * dnorm( x, mu_2 )
  
  P_1 <- T_1 / (T_1 + T_2)
  P_2 <- T_2 / (T_1 + T_2) ## note: P_2 = 1 - P_1
  
  tau_1 <- mean(P_1)
  tau_2 <- mean(P_2)
  
  ## Given the observed data, as well as the latent variables,
  ## what are the population parameters?
  
  mu_1 <- sum( P_1 * x ) / sum(P_1)
  mu_2 <- sum( P_2 * x ) / sum(P_2)
  
  ## print the current estimates
  
  print( c(mu_1, mu_2, mean(P_1)) )
  
}
require(mixtools)
myEM <- normalmixEM( x, mu = c(0,1), sigma=c(1,1), sd.constr=c(1,1) )
myEM$mu
myEM$lambda