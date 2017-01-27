
firstrun <- 0

if (firstrun==1) {
  # Clear environment
  rm(list = ls())
  # Clear any plots
  graphics.off()
  
  library(tibble)
  
  # --------------------------------------------------------
  # Get current environment, and set working path accordingly
  myMachine <- Sys.info()["nodename"]
  if (myMachine == 'P37') {
    myOutPath = "D:/R/Functions"
  } else {
    myOutPath = "D:/Dissertation/R/Functions"
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

FileIn <- "D:/Dissertation/R/iris/iris.csv"
FileName <- "D:/Dissertation/R/Burrow.csv"

# write the header to the burrow file and 
# make a dataframe of the file
myData <- myContent("Header", FileIn, FileName)

#testdata <- data(scat)

#myData <- myContent("Header", testdata, FileName)

for(i in names(myData)) {
  myContent(myData,i,FileName)
}

burrow <- read.csv(FileName)
burrow


# --------------------------------------------------------

# Load iris burrow data
burrow <- read.csv(FileName)
head(burrow)
FieldCounts <- data.frame(table(burrow$Param, dnn="Param"))

