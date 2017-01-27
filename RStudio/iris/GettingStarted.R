
# attach the iris dataset, so we don't need to put iris$ in front of everything
attach(iris)

## BASIC Data investigation =============================================
# show the header data rows
head(iris)

# show the tail data rows
tail(iris)

# show a summary of the variables
summary(iris)

# PLOTS =================================================================
# everything by everything
plot(iris)

# selected variables and add some colour
pairs(iris[1:4], main = "Plot 1. Edgar Anderson's Iris Data", 
                 pch = 21, 
                 bg = c("red", "green3", "blue")[unclass(iris$Species)])

# length by width
plot(iris$Petal.Length, iris$Petal.Width, 
                 main="Plot 2. Edgar Anderson's Iris Data")

# length by width
plot(Petal.Length, Petal.Width, 
     main="Plot 3. Edgar Anderson's Iris Data")
