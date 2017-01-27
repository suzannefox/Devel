

FileIn <- "D:/R/iris/iris.csv"
xmyData <- read.csv(FileIn)

i <- sapply(xmyData, is.numeric)
# make data frame of just numerics
myDatanum <- xmyData[,i]

y <- myDatanum[,1]
y1 <- qqnorm(y); 
y2 <- qqline(y1, col = 2)
y3 <- qqplot(y, rt(300, df = 5))

