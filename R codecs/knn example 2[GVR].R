library(plyr)
require(caret)
library(ggplot2)
require(class)
#set.seed(123)

# Create training and testing data sets
idx = sample(1:nrow(iris), size = 100)
train.idx = 1:nrow(iris) %in% idx
test.idx =  ! 1:nrow(iris) %in% idx

train = iris[train.idx, 1:4]
test = iris[test.idx, 1:4]

# Get labels
labels = iris[train.idx, 5]

# Do knn
fit = knn(train, test, labels ,k=2)
fit

# Create a dataframe to simplify charting
plot.df = data.frame(test, predicted = fit)

# Use ggplot
# 2-D plots example only
# Sepal.Length vs Sepal.Width

# First use Convex hull to determine boundary points of each cluster
plot.df1 = data.frame(x = plot.df$Sepal.Length, 
                      y = plot.df$Sepal.Width, 
                      predicted = plot.df$predicted)

find_hull = function(df) df[chull(df$x, df$y), ]
boundary = ddply(plot.df1, .variables = "predicted", .fun = find_hull)

ggplot(plot.df, aes(Sepal.Length, Sepal.Width, color = predicted, fill = predicted)) + 
  geom_point(size = 5) + 
  geom_polygon(data = boundary, aes(x,y), alpha = 0.5)