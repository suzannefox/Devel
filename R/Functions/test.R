
warnings()

x <- sapply(myData,typeof)


myTest <- function(myData, Field, FileName) {
  str(myData)
  print(ncol(myData))
  print(nrow(myData))
}
myTest(iris,"Header","")

myContent(iris,"File","")

mynames <- names(iris)
icount <- 0
for (i in names(iris)) {
  icount <- icount + 1
  Ftext = sprintf("Field%02d",icount)
  print (Ftext)
  print (i)
}

return()

myTest("iris","Header","")

for(i in names(iris)){
  myContent(iris,i,"")
}


string1 <- "hsjdh ak  kasdhak."
string2 <- "ak"

pos1 <- regexpr(string2, string1)
pos2 <- gregexpr(string2, string1)

keep = substr(string1, pos1[1], pos1[1]+1)


library(plyr)
ztest <- c("Jack","Jill","hsakdhkas ksahdk kasdhjskhadh","h","jim@fred.com")

myInfo(ztest)

return()

FieldCounts <- count(ztest)
# str(FieldCounts)
# 
# for (i in seq_along(FieldCounts)) {
#   str(FieldCounts[i])
# }

# Make a vector of the columns which are factors
i <- sapply(FieldCounts, is.factor)
# change any factor columns to character type
FieldCounts[i] <- lapply(FieldCounts[i], as.character)
# add a column which is the length of the strings
FieldCounts$Lengths <- nchar(FieldCounts[,1])
# add various column which count the occurances of string features
FieldCounts$AtSigns <- sapply(FieldCounts[,1], function(x) contains("@",x))

f2 <- function(a,b,c) {
  #x1 <- formals()
  #x2 <- find("$a")
#  x3 <- deparse(sys.calls()[[sys.nframe()-1]])
  x3 <- toString(sys.calls())
  start <- regexpr('f2', x3)[1]
  comma <- regexpr(',', x3)[1]
  var2 <- substring(x3,start+nchar("f2")+1,comma-1)
  x3 = var2
  
    #str(sys.calls())  

#  print(paste("current frame is", sys.nframe(), "\n"))
#  print(paste("parents are", sys.parents(), "\n"))
#  print(sys.function(0)) # ggg
#  print(sys.function(2)) # gg
  
  #print (x1)
  #print(x2)
#  print(x3)
#  a * 9
  
  print("......")
  print(x3)
  print("......")
  
  
  return(x3)
}


tester <- f2(45,4,6)
# start <- regexpr('f2', tester)[1]
# comma <- regexpr(',', tester)[1]
# var2 <- substring(tester,start+3,comma-1)
# keep = substr(string1, pos1[1], pos1[1]+1)



