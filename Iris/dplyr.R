
setwd("C:/Users/Suzanne/OneDrive - Suzanne Fox/MSc Course/Dissertation")
getwd()
#install.packages("readr")
#install.packages("dplyr")
library(readr)
library(dplyr)

# want to write a file which is 
# col 1 - sourceid "IRIS"
# col 2 - file name
# col 3 - field name
# col 4 - field offset
# col 5 - data type
# col 6 - data

SourceId <- "IRIS"
SourceFile <- "C:/Users/Suzanne/OneDrive - Suzanne Fox/MSc Course/Dissertation/Data/01-Iris/iris.csv"

Data_iris <- read_csv(SourceFile,col_names=TRUE)
Data_Names <- names(Data_iris)

temp <- Data_iris[1]
colnames(temp) <- c("Data")
temp$Source <- c(SourceId)
temp$FileName <- c(SourceFile)
temp$FieldOffset <- c(1)
temp$Datatype <-c("number")
temp$Datatypesub <- c("float")

View(temp)


