
firstrun <- 0

if (firstrun==1) {
  # Clear environment
  rm(list = ls())
  # Clear any plots
  graphics.off()
  
  # library(tibble)
  
  # --------------------------------------------------------
  # Get current environment, and set working path accordingly
  myMachine <- Sys.info()["nodename"]
  if (myMachine == 'P37') {
    myOutPath = "E:/MSc Course/Dissertation/R"
  } else {
    myOutPath = "G:/Dissertation/R"
  }
  setwd(myOutPath)
  getwd()
  
  source("myContent.R")
  
  # return() only works in functions
  stop("Finished set-up", call. = FALSE, NULL)
}
# --------------------------------------------------------

# Load iris data
class(iris)
# create the burrow data
FileName <- ""

FileIn <- "D:/R/iris/iris.csv"
FileIn <- "E:/MSc Course/Dissertation/Data/FL_insurance_sample.csv"
FileIn <- "E:/MSc Course/Dissertation/Data/SacrementoCrime/SacramentocrimeJanuary2006.csv"
FileName <- paste(myOutPath,"/Burrow.csv", sep="")

# write the header to the burrow file and  
# make a dataframe of the file
myData <- myContent("Header", FileIn, FileName)
for(i in names(myData)) {
  myContent(myData,i,FileName)
}

# x <- c("sdkh","qwhkjas34hdk","nn","fred@gy","@345", "https://hgsadgJD","http:","www.")
# myData <- data.frame(x)
# myData <- myContent("Header", myData, FileName)
# for(i in names(myData)) {
#   myContent(myData,i,FileName)
# }

burrow <- read.csv(FileName)
burrow.density <- burrow[(burrow$InfoType=="DENSITY"),]
burrow.density


# --------------------------------------------------------

# Load iris burrow data
burrow <- read.csv(FileName)
head(burrow)
FieldCounts <- data.frame(table(burrow$Param, dnn="Param"))

