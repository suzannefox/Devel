
testdata <- read.csv("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/TestData.csv")

# identify all the variables suitable for numeric stats
iris.mean <- as.data.frame(suppressWarnings(sapply(testdata, mean, na.rm=TRUE)))
colnames(iris.mean) <- "Value"

# get the numerical variables
iris.numericols <- row.names(subset(iris.mean, Value!="NA"))

# make empty dataframe
iris.correlations <- data.frame(Var1=character(),
                                Var2=character(),
                                cor=double())
# calculate correlations
iris.cor <- as.data.frame(cor(testdata[,c(iris.numericols)]))

# make a long format dataframe
for (i in names(iris.cor)) {
  temp.Var1 <- iris.numericols
  temp.Var2 <- rep_len(i, length(temp.Var1))
  temp.Var3 <- iris.cor[,c(i)]
  temp.df = data.frame("Var1" = temp.Var1, "Var2" = temp.Var2, "Value" = temp.Var3)
  iris.correlations <- rbind2(iris.correlations, temp.df)
}

# for(i in names(iris)) {
#   print(i)
#   print(nrow(subset(iris.correlations, Var1==i)))
# }
  

# x <- stack(iris.cor)
# xrows <- rep_len(row.names(iris.cor),nrow(x))
# 
# row.names(x) <- rep_len(row.names(iris.cor),length(x))
# 
# 
# 
# # calculate covariances
# iris.cov <- as.data.frame(cov(iris[,c(iris.numericols)]))
# 
# 
# names(iris.mean) <- c("Mean")
# iris.mean <- as.data.frame(sapply(iris[,c(iris.numericols)],mean,na.rm=TRUE))
# iris.median <- as.data.frame(sapply(iris[,c(iris.numericols)],median,na.rm=TRUE))
# iris.max <- as.data.frame(sapply(iris[,c(iris.numericols)],max,na.rm=TRUE))
# iris.min <- as.data.frame(sapply(iris[,c(iris.numericols)],min,na.rm=TRUE))
# iris.skew <- as.data.frame(sapply(iris[,c(iris.numericols)],skewness,na.rm=TRUE))
# 
# iris.cor <- as.data.frame(cor(iris[,c(iris.numericols)]))
# 
# iris.comp <- cbind(iris.mean,iris.median,iris.max,iris.min)
# names(iris.comp) <- c("Mean","Median","Max","Min")
# iris.comp$Spread <- iris.comp$Max - iris.comp$Min
# 
# iris.comp$meanx <- (iris.comp$Mean - iris.comp$Min) / iris.comp$Spread
# iris.comp$medianx <- (iris.comp$Median - iris.comp$Min) / iris.comp$Spread
# 
# 
# # Burrow
# Burrow.testdata <- read.csv("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/TestData.csv")
# Burrow.mean <- as.data.frame(sapply(Burrow.testdata, mean, na.rm=TRUE))
# names(Burrow.mean) <- c("Mean")
# Burrow.mean <- subset(Burrow.mean, Mean!="NA")
# Burrow.numerics <- row.names(Burrow.mean)
# Burrow.median <- as.data.frame(sapply(Burrow.testdata[,c(Burrow.numerics)],median,na.rm=TRUE))
# names(Burrow.median) <- c("Median")
# 
# 
# 
# # ===================================
# iris.pl <- iris$Petal.Length
# iris.pw <- iris$Petal.Width
# iris.sl <- iris$Sepal.Length
# iris.sw <- iris$Sepal.Width
# 
# iris.pl.mean <- mean(iris.pl)
# iris.pl.median <- median(iris.pl)
# 
# #source("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/Burrow.R")
# 
# Burrow.testdata <- read.csv("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/TestData.csv")
# Burrow.stack <- stack(Burrow.testdata)
# Burrow.unstack <- unstack(Burrow.stack)
# 
# 
# 
# #x <- Burrow(testdata, "test data csv", TRUE)
# #x <- Burrow(testdata, "test data csv", TRUE)
# #Burrow <- x$longBurrow
# 
# #x <- subset(Burrow, InfoType=="BEST GUESS")
# 
# x <- iris
# xs <- stack(iris)
# xa <- aov(stack(iris)$values ~ stack(iris)$ind)
# 
# x.summary <-as.data.frame(summary(iris))
# 
# x.diff.pl <- mean(iris$Petal.Length) - median(iris$Petal.Length)
# x.diff.sl <- mean(iris$Sepal.Length) - median(iris$Sepal.Length)
# 
# 
# #x$Text <- format("Length is %01f", x$Petal.Length)
# x$Test <- sprintf("Petal Length %03f", x$Petal.Length)
# 
# x.var <- x$Sepal.Length
# #y.var = x$Petal.Width
# #y.var <- x$Species
# y.var <- x$Test
# 
# xcor <- cor(x.var, y.var, method="spearman")
# plot(x.var, y.var, xlab="x-label", ylab="y-label", pch=19)
# abline(lm(y.var ~ x.var))
