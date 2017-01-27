
MyMachine <- Sys.info()["nodename"]

if (MyMachine=="P37") {MyDisk="G"
} else {MyDisk="D"
}

MyFile <- paste(MyDisk,":/Dissertation/DataSets/Pruned.csv",sep="")
MyData <- read.csv(file=MyFile, header=TRUE, sep=",")
names(MyData)

#split into a subset by column
TestData <- MyData[, c(1, 7, 8, 9)]

#rename column to something more manageable
names(TestData)
library(plyr)
#play with creating a new column
#TestData$Classx <- TestData$Class

# change difficult column name
TestData <- rename(TestData, c("SourceFieldClassification"="Class"))
table(TestData$Class, TestData$Class)

# recode lowercase to upper
TestData$Class[TestData$Class=="Numeric"] <- "NUMERIC"
TestData$Class[TestData$Class=="Alpha"] <- "ALPHA"

# drop the unused factors
TestData$Class <- TestData$Class[,drop = TRUE]

# change difficult column name
MyData <- rename(MyData, c("SourceFieldClassification"="Class"))

# recode lowercase to upper
MyData$Class[MyData$Class=="Numeric"] <- "NUMERIC"
MyData$Class[MyData$Class=="Alpha"] <- "ALPHA"

# drop the unused factors
MyData$Class <- MyData$Class[,drop = TRUE]
table(MyData$Class, MyData$Class)

attach(MyData)
library('e1071')

x <- subset(MyData, select = -Class)
y <- Class

# model1 <- svm(MyData$Class~., MyData)
# model <- svm( iris$Species~., iris )
# res <- predict( model, newdata=iris )


#table(TestData)
#TestData <- c(MyData$FieldClass,MyData$PropAlpha,MyData$PropNumeric)
#plot(MyData, pch=16)
#library("e1071")
#model_1 <- svm(MyData$SourceFieldClassification ~ ., data = iris)