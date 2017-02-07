
library(ggplot2)

workdir <- "C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/"

#source("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/Burrow.R")
testdata <- read.csv(paste(workdir,"TestData.csv",sep=""))

Burrow.test <- Burrow(testdata, "synthetic test dataset", TRUE)
Burrow.test.data <- Burrow.test$data
Burrow.test.best <- subset(Burrow.test.data,InfoType=="BEST GUESS")
Data.vars.number <- subset(Burrow.test.data, 
                           InfoType=="BEST GUESS" & (myData1=="DECIMALS" | myData1=="NUMBER"))

Data.pairs <- as.list(Data.vars.number[c("Variable1")])

Data.scatter <- testdata[,c(Data.pairs$Variable1)]
pairs(Data.scatter[,c(1:4)])
pairs(Data.scatter[,c(5:8)])

if (nrow(Data.vars.number)==0) {
  print("No numeric variables in this dataset")
  
} else {
  
  for (i in 1:nrow(Data.vars.number)) {
    # Generate data
    data.title <- Data.vars.number[i,c("Variable1")]
    print("Charting =======================")
    print(data.title)
    data.orig <- as.data.frame(testdata[,c(data.title)])
    xx <- plot_nums(data.orig, data.title)
    print(xx)
  }
}