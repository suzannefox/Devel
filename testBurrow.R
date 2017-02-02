
source("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/Burrow.R")

#testdata <- read.csv("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/TestData.csv")
testdata <- iris

x <- Burrow(testdata, "test data csv", TRUE)
Burrow <- x$longBurrow

bg <- subset(Burrow, InfoType=="BEST GUESS")
cr <- subset(Burrow, InfoType=="ANALYSIS")
