data(cars)


plot(cars$speed, cars$dist, main="lowess(cars)")
lines(lowess(cars$speed, cars$dist), col=2)
lines(lowess(cars$speed, cars$dist, f=.2), col=3)
legend(5, 120, c(paste("f=", c("2/3", ".2"))), lty=1, col=2:3)

#
# formula method: plot, then calculate the lowess smoother,
#                 then add smooth to the plot
#
plot(dist ~ speed, data=cars, main="lowess(cars)")
lines(lowess(cars$dist ~ cars$speed), col=2, lty=2)
lines(lowess(cars$dist ~ cars$speed, f=.2), col=3) # smaller bandwith
legend(5, 120, c(paste("f=", c("2/3", ".2"))), lty=1, col=2:3)

#
# formula method: calculate lowess() smoother, then call plot()
#                  on the lowess object
#
lw <- lowess(cars$dist ~ cars$speed)
plot(lw, main="lowess(cars)"  )

#
# formula method: calculate and plot in a single command
#
plot.Lowess(cars$dist ~ cars$speed, main="lowess(cars)")
