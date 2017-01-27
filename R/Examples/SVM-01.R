
MyFile <- "D:/Dissertation/DataSets/Pruned.csv"
MyData <- read.csv(file=MyFile, header=TRUE, sep=",")
names(MyData)

#split into a subset by column
TestData <- MyData[, c(1, 7, 8, 9)]

#rename column to something more manageable
names(TestData)
library(plyr)
#play with creating a new column
#TestData$Classx <- TestData$Class

TestData <- rename(TestData, c("SourceFieldClassification"="Class"))
table(TestData$Class, TestData$Class)

# recode lowercase to upper
TestData$Class[TestData$Class=="Numeric"] <- "NUMERIC"
TestData$Class[TestData$Class=="Alpha"] <- "ALPHA"

# drop the unused factors
TestData$Class <- TestData$Class[,drop = TRUE]


#table(TestData)
#TestData <- c(MyData$FieldClass,MyData$PropAlpha,MyData$PropNumeric)
#plot(MyData, pch=16)
#library("e1071")
#model_1 <- svm(MyData$SourceFieldClassification ~ ., data = iris)