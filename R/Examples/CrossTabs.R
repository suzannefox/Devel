
MyMachine <- Sys.info()["nodename"]

if (MyMachine=="P37") {MyDisk="G"
} else {MyDisk="D"
}

MyFile <- paste(MyDisk,":/Dissertation/DataSets/Pruned.csv",sep="")
MyData <- read.csv(file=MyFile, header=TRUE, sep=",")
names(MyData)

library(Hmisc)
dd <- data.frame(Q1=sample(1:3, 20, replace=T), Q2=sample(1:3, 20, replace=T), 
                 Q3=sample(1:3, 20, replace=T))  #fake data

tabs <- summary(~Q1+Q2+Q3, data=dd, fun=table)
tabs
