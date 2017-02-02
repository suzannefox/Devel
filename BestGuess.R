


source("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/Burrow.R")

testdata <- read.csv("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/TestData.csv")
#x <- Burrow(testdata, "test data csv", TRUE)
x <- Burrow(testdata, "test data csv", TRUE)
Burrow <- x$longBurrow

x <- subset(Burrow, InfoType=="BEST GUESS")

