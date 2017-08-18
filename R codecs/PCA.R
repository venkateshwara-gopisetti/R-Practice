data("USArrests")

plot(USArrests)
model <- prcomp(USArrests)
biplot(model, scale = 0)

std_dev <- model$sdev
pr_var <- std_dev^2
pr_var[1:4]
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:4]

#scree plot
plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")
