
workdir <- "C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/"

source("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/Burrow.R")
testdata <- read.csv(paste(workdir,"TestData.csv",sep=""))

#testdata <- iris

Burrow.test <- Burrow(iris, "iris dataset", TRUE)
Burrow.test.data <- Burrow.test$data

write.csv(Burrow.test.data,paste(workdir,"IrisBurrow.csv",sep=""))

