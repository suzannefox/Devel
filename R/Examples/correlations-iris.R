

# ========================================================================
# use corrplot to have a quick look at correlations 
# need numeric columns only
# ========================================================================
library(corrplot)

myData <- mtcars 
# Make a vector of the columns which are numeric
i <- sapply(myData, is.numeric)
# make data frame of just numerics
myDatanum <- myData[,i]
# make correlation matrix of numeric variables
M <- cor(myDatanum)
corrplot(M, method="circle", type="lower")

# T <- table(iris$Sepal.Width, iris$Species)
# 
# # polychoric correlation
# # x is a contingency table of counts
# library(polycor)
# polychor(T)

# heterogeneous correlations in one matrix
# pearson (numeric-numeric),
# polyserial (numeric-ordinal),
# and polychoric (ordinal-ordinal)
# x is a data frame with ordered factors
# and numeric variables
#hetcor(x) 

M

# split iris dataframe into a list of 3 datarames, one for each Species
g <- iris$Species
l <- split(iris, g)


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
plot(iris$Petal.Length, iris$Petal.Width, 
     main="Plot 3. Edgar Anderson's Iris Data")
