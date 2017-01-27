
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
    myOutPath = "G:/Dissertation/R"
  }
  setwd(myOutPath)
  getwd()
  
  return
}
# --------------------------------------------------------

# Load iris data
data(iris)

# create the burrow data
FileName <- "D:/R/Burrow.csv"
#FileName <- ""

myContent("","Header",FileName)
myContent(iris,"File",FileName)

for(i in names(iris)){
  myContent(iris,i,FileName)
}

# --------------------------------------------------------

# Load iris burrow data
burrow <- read.csv(FileName)
#g <- burrow$ParamType
#l <- split(burrow, g)

#burrowVars <- burrow[burrow$Info1=="DataType",]

head(burrowVars)
