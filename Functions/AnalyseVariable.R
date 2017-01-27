# Clear environment
rm(list = ls())
# Clear any plots
graphics.off()

library(tibble)

# x <- c(3,4,5,6,7)
# y <- c(1,2,6,4,3)
# plot(x,y)

# --------------------------------------------------------
# Get current environment, and set working path accordingly
myMachine <- Sys.info()["nodename"]
if (myMachine == 'P37') {
  myOutPath = "D:/R/Functions"
} else {
  myOutPath = "G:/Dissertation/R"
}
setwd(myOutPath)
getwd()
# --------------------------------------------------------

# Load iris data
data(iris)

# Play around a bit
str(iris)
plot(iris$Sepal.Length, iris$Petal.Width)
pairs(iris)

class(iris)
tibble1 <- as_data_frame(iris)
class(tibble1)
head(tibble1)

class(tibble1$Sepal.Length)
class(tibble1$Species)

mysum <- summary(tibble1$Sepal.Length)
class(mysum)
tibble2 <- as_data_frame(mysum)
attributes(mysum)

mymean <- mean(mysum)
mymedian <- median(mysum)

write.table(paste("MEAN",mymean,sep=","), file = "MyTestData.csv", row.names = FALSE, col.names = FALSE, append = TRUE)
write.table(paste("MEDIAN",mymedian,sep=","), file = "MyTestData.csv", row.names = FALSE, col.names = FALSE, append = TRUE)

myTest <- mysummary(mysum, FALSE, TRUE)

# ========================================================================
# change data to be a matrix, so it uses the levels integer values
# for factor variables, which means we can put it into corrplot
# ========================================================================
mydata_corr_matrix <- data.matrix(iris)
str(mydata_corr_matrix)

# ========================================================================
# use corrplot to have a quick look at the data 
# ========================================================================
library(corrplot)
M <- cor(mydata_corr_matrix)
corrplot(M, method = "circle", type="lower")

# n, nmiss, unique, mean, 5,10,25,50,75,90,95th percentiles
# 5 lowest and 5 highest scores
library(Hmisc)
M <- describe(iris)
# n, nmiss, unique, mean, 5,10,25,50,75,90,95th percentiles
# 5 lowest and 5 highest scores

# nbr.val, nbr.null, nbr.na, min max, range, sum,
# median, mean, SE.mean, CI.mean, var, std.dev, coef.var 
library(pastecs)
stat.desc(iris)

# item name ,item number, nvalid, mean, sd,
# median, mad, min, max, skew, kurtosis, se
library(psych)
describe(iris)

# produces mpg.m wt.m mpg.s wt.s for each
# combination of the levels of cyl and vs 
library(doBy)
summaryBy(mpg + wt ~ cyl + vs, data = mtcars,
          FUN = function(x) { c(m = mean(x), s = sd(x)) } )

library(plyr)

tester <- c(1.3,4,5,3,7,3)
tester <- c("jim","harry","wohg")
u1 <- count(tester)
uclass <- sapply(u1,class)
if (uclass[1]=="factor") {
  print ("Yay!")
}
max(u1[1])

uniques <- myunique(tester)

write(uniques,"",sep=",")

# outInt <- c(3,6,7,8)
# for(i in seq_along(outInt)){
#   print(outInt[i])
# }

outString <- c("x","d","f","j")
write.table(outString,"")

# declare output file
fileout <- "D:/R/Stuff/x.csv"
# clear file if it exists already
write.table("",fileout,append=FALSE,row.names=FALSE,col.names=FALSE,quote=FALSE,eol="")

# write the contents of the vector to the file
for(i in seq_along(outString)){
  write.table("fred,",fileout,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
  write.table(outString[i],fileout,append=TRUE,row.names=FALSE, col.names=FALSE, quote=FALSE,eol="")
  write.table(",jim",fileout,append=TRUE,row.names=FALSE, col.names=FALSE, quote=FALSE)
}

fileout <- ""
write.table("",fileout,append=FALSE,row.names=FALSE,col.names=FALSE,quote=FALSE,eol="")
# write the contents of the vector to the file
for(i in seq_along(u1$x)){
  write.table("field,content,dataitem,",fileout,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
  write.table(u1$x[i],fileout,append=TRUE,row.names=FALSE, col.names=FALSE, quote=FALSE,eol="")
  write.table(",count,",fileout,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
  write.table(u1$freq[i],fileout,append=TRUE,row.names=FALSE, col.names=FALSE, quote=FALSE,eol="")
  # force newline
  write.table("", fileout, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)
}

print("fred")

# iterations = 10
# variables = 2
# 
# output <- matrix(ncol=variables, nrow=iterations)
# 
# for(i in 1:iterations){
#   output[i,] <- runif(2)
#   
# }
# 
# output

tester <- data.frame(V1 = c(1,3,4,3,2,5,6,7))
x1 <- count(tester)
str(x1)
x2 <- x1[,1]
x3 <- x1$V1
x4 = nrow(x3)
mean(x2)

str(x3)

# --------------------------------------------------------
# analyse the content of a field for unique values and
# write to a .csv file

# ==============================================================
myContent <- function(Field, FieldName, FileName) {
  
  # Get unique values and their count
  library(plyr)
  FieldCounts <- count(Field)

  nrows <- nrow(Field)
  nuniques <- nrow(FieldCounts)
  
  uclass <- sapply(u1,class)
  # Data type
  write.table(paste("Field,",FieldName,",",sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
  write.table(paste("PARAMS,DataType,",uclass[1],sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
  write.table("", FileName, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)

  # Length
  write.table(paste("Field,",FieldName,",",sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
  write.table(paste("PARAMS,RECORDS,",nrows,sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
  write.table("", FileName, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)
  
  # write number of uniques
  write.table(paste("Field,",FieldName,",",sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
  write.table(paste("PARAMS,UNIQUES,",nuniques,sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
  write.table("", FileName, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)
  
  # Density
  write.table(paste("Field,",FieldName,",",sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
  write.table(paste("PARAMS,DENSITY,",nrows/nuniques,sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
  write.table("", FileName, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)
  
  # print stats for numeric variables
  if (uclass[1]=="numeric") {
  
        # Max value
    write.table(paste("Field,",FieldName,",",sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
    write.table(paste("PARAMS,MAX,",max(FieldCounts[,1]),sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
    write.table("", FileName, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)

    # Min value
    write.table(paste("Field,",FieldName,",",sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
    write.table(paste("PARAMS,MIN,",min(FieldCounts[,1]),sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
    write.table("", FileName, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)

    # Mean value
    write.table(paste("Field,",FieldName,",",sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
    write.table(paste("PARAMS,MEAN,",mean(FieldCounts[,1]),sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
    write.table("", FileName, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)

    # Std Dev
    write.table(paste("Field,",FieldName,",",sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
    write.table(paste("PARAMS,STDERR,",sd(Field),sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
    write.table("", FileName, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)
  
    # Var
    write.table(paste("Field,",FieldName,",",sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
    write.table(paste("PARAMS,VAR,",var(Field),sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
    write.table("", FileName, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)
  
    # Median
    write.table(paste("Field,",FieldName,",",sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
    write.table(paste("PARAMS,MEDIAN,",median(Field),sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
    write.table("", FileName, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)
  }
  
  # write the contents of the vector to the csv file
  for(i in seq_along(FieldCounts$x)){
    write.table(paste("Field,",FieldName,",",sep=""),FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")

    write.table("content,dataitem,",FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
    write.table(FieldCounts$x[i],FileName,append=TRUE,row.names=FALSE, col.names=FALSE, quote=FALSE,eol="")

    write.table(",count,",FileName,append=TRUE,row.names=FALSE, col.names=FALSE,quote=FALSE,eol="")
    write.table(FieldCounts$freq[i],FileName,append=TRUE,row.names=FALSE, col.names=FALSE, quote=FALSE,eol="")
    # force newline
    write.table("", FileName, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)
  }
}
myContent(tester$V1,"tester$V1","")

# ==============================================================
mysummary <- function(x, npar=TRUE, print=TRUE) {
  
  myMean <- mean(x)
  myStdDev <- sd(x)
  myVar <- var(x)
  myMedian <- median(x)
  
  result <- list(MeanIs=myMean, StdDevIs=myStdDev, VarIs=myVar, MedianIs=myMedian)
  return(result)
}
