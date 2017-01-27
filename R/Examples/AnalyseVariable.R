# Clear environment
rm(list = ls())
# Clear any plots
graphics.off()

library(tibble)

# x <- c(3,4,5,6,7)
# y <- c(1,2,6,4,3)
# plot(x,y)

data(iris)
str(iris)
plot(iris$Sepal.Length, iris$Petal.Width)
pairs(iris)

# names in data
names <- names(iris)
mystr <- str(iris)

class(iris)
tibble1 <- as_data_frame(iris)
class(tibble1)
head(tibble1)

class(tibble1$Sepal.Length)
class(tibble1$Species)

mysum <- summary(tibble1$Sepal.Length)
class(mysum)
tibble2 <- as_data_frame(mysum)
attributes(mysum)


mymean <- mean(tibble1$Sepal.Width)
mymedian <- median(tibble1$Sepal.Width)

write.table(paste("MEAN",mymean,sep=","), file = "G:/Dissertation/R/MyTestData.csv", row.names = FALSE, col.names = FALSE, append = TRUE)
write.table(paste("MEDIAN",mymedian,sep=","), file = "G:/Dissertation/R/MyTestData.csv", row.names = FALSE, col.names = FALSE, append = TRUE)

library(ggplot2)
# violin plot
p <- ggplot(iris, aes(factor(Species), Petal.Width))
p + geom_violin()
p + geom_violin() + geom_jitter(height = 0)

testx <- iris$Sepal.Length

# index plot
qplot(seq_along(iris$Sepal.Length), testx)

# histogram
# qplot(iris$Sepal.Length, geom="histogram") 
ggplot(data=iris, aes(testx)) + geom_histogram() 

# cumulative hist
ggplot(NULL,aes(testx))+geom_histogram(aes(y=cumsum(..count..)))+
  stat_bin(aes(y=cumsum(..count..)),geom="line",color="green")

# try normal dist
library(fitdistrplus)
library(logspline)
fit.norm <- fitdist(testx, "norm")
plot(fit.norm)

# get frequency count and save as a tibble
freq <- table(testx)
library (tibble)
tibble2 <- as_data_frame(as.data.frame(freq))

y <- rt(200, df = 5)
qqnorm(y); qqline(y, col = 2)
qqplot(y, rt(300, df = 5))

qqnorm(precip, ylab = "Precipitation [in/yr] for 70 US cities")
