
library(ggplot2)

workdir <- "C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/"

source("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/Burrow.R")
testdata <- read.csv(paste(workdir,"TestData.csv",sep=""))
testtitle <- "synthetic dataset"

#testdata <- iris
#testtitle <- "iris"

Burrow.test <- Burrow(testdata, testtitle, TRUE)
Burrow.test.data <- Burrow.test$data

Data.vars.number <- subset(Burrow.test.data, 
                           InfoType=="BEST GUESS" & (myData1=="DECIMALS" | myData1=="NUMBER"))

library(GGally)
if (nrow(Data.vars.number)==0) {
  print("No numeric variables in this dataset")

} else {
  Data.pairs <- as.list(Data.vars.number[c("Variable1")])
  Data.scatter <- testdata[,c(Data.pairs$Variable1)]

  ptot <- ncol(Data.scatter)

  p1 <- 1
  p2 <- 4
  if (ptot < 4 ) {p2==ptot}

  while(ptot > 0) {

    print("in loop")    
    print(ptot)
    
    Data.plot <- Data.scatter[,c(p1,p2)]
    print(ggpairs(Data.plot))

    ptot <- ptot-4
    if (ptot > 0 ) {
      p1 <- p1+4
      p2 <- p2+4
      if (ptot < 4 ) {p2==ptot}

      print("Next pass, total, p1, p2")
      print(ptot)
      print(p1)
      print(p2)
    }
  }
}


