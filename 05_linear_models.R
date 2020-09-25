#---------------------------------------
# example code for linear model lecture
# utilizing iris data set
# ML 09/25/20
#---------------------------------------


#susbet for Iris virginica
flower <- iris[iris$Species=="virginica",]

# linear model relating petal length to sepal length
fit <- lm(flower$Petal.Length~flower$Sepal.Length)

# view results
summary(fit)

# create a scatter plot
plot(flower$Sepal.Length,flower$Petal.Length,
     main = "Iris virginica",
     xlab = "Sepal Length",
     ylab = "Petal Length")

# plot the residuals, stored in regression summary
plot(flower$Sepal.Length,summary(fit)$residuals,
     xlab = "Sepal Length", 
     ylab = "Residuals")

# add a horizontal line ro reference
abline(h=0)

# histogram of residuals
hist(summary(fit)$residuals)

# shapiro wilks test
shapiro.test(summary(fit)$residuals)
