#---------------------------------------
# example code for linear model lecture
# utilizing iris data set
# ML 09/25/20
#---------------------------------------


#susbet for Iris virginica
flower <- iris[iris$Species == "virginica",]

# linear model relating petal length to sepal length
fit <- lm(flower$Petal.Length~flower$Sepal.Length)

# view results
summary(fit)

# create a scatter plot
plot(flower$Sepal.Length,flower$Petal.Length,
     main = "Iris virginica",
     xlab = "Sepal Length",
     ylab = "Petal Length",
     col = "purple", pch = 16)

# plot the residuals, stored in regression summary
plot(flower$Sepal.Length,summary(fit)$residuals,
     xlab = "Sepal Length", 
     ylab = "Residuals", 
     col = "purple",
     pch = 16)

# add a horizontal line ro reference
abline(h=0,
       lty = "dashed")

# histogram of residuals
hist(summary(fit)$residuals,
     main = "Regression Residuals",
     xlab = "Residual",
     col = "purple")

# shapiro wilks test
shapiro.test(summary(fit)$residuals)

# qq plot
qqnorm(summary(fit)$residuals, pch = 16)

qqline(summary(fit)$residuals, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7, pch = 16)

