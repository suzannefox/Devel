
workdir <- "C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/"

source("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/Burrow.R")
testdata <- read.csv(paste(workdir,"TestData.csv",sep=""))

#testdata <- iris

Burrow.test <- Burrow(testdata, "synthetic test dataset", TRUE)
Burrow.test.data <- Burrow.test$data
Burrow.test.best <- subset(Burrow.test.data,InfoType=="BEST GUESS")
Burrow.test.number <- subset(Burrow.test.best, myData1=="NUMBER")

# write.csv(Burrow.test.data,paste(workdir,"IrisBurrow.csv",sep=""))

