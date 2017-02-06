
library(ggplot2)

workdir <- "C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/"

source("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/Burrow.R")
testdata <- read.csv(paste(workdir,"TestData.csv",sep=""))

Burrow.test <- Burrow(testdata, "synthetic test dataset", TRUE)
Burrow.test.data <- Burrow.test$data
Burrow.test.best <- subset(Burrow.test.data,InfoType=="BEST GUESS")
Burrow.test.number <- subset(Burrow.test.best, myData1=="NUMBER")


# Generate data
data.title <- "Dist1"
data.orig <- as.data.frame(testdata[,c(data.title)])
xx <- plot_nums(data.orig, data.title)
xx

