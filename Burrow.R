
if (!exists("firstrun")) {
  
  # Clear environment
  rm(list = ls())
  firstrun <- 1
  library(ggplot2)
  # Clear any plots
  graphics.off()
  
  #diagnostics <- 0 # Set this to 1 for diagnostic messages
  
  # --------------------------------------------------------
  # Get current environment, and set working path accordingly
  myMachine <- Sys.info()["nodename"]
  if (myMachine == 'P37') {
    myOutPath = "E:/MSc Course/Dissertation/R/"
    myDataPath = "E:/MSc Course/Dissertation/Data/"
  } else {
    myOutPath = "D:/Dissertation-Temp/"
    myDataPath = "D:/Dissertation-Backup/Data/"
  }
  setwd(myOutPath)
  getwd()
  
  source("myBurrow.R")
  source("myBurrowFeatures.R")

  # return() only works in functions
  stop("Finished set-up", call. = FALSE, NULL)
}

if (firstrun==1) {
  #diagnostics <- 0

  # development dataset
  test.input <- read.csv(paste(myOutPath,"TestData.csv",sep=""))
  test.burrow <- myBurrow(test.input)
  test.features <- myBurrowFeatures(test.burrow)
  test.bestguess <- myBestGuess(test.input,"Synthetic Development data")
  
  test.na <- subset(test.burrow, InfoDetail=="COUNT_NA",c("Variable1","myData1"))
  rownames(test.na) <- NULL
  colnames(test.na) <- c("VariableName","Missing")
  
  test.unique <- subset(test.burrow, InfoDetail=="COUNT_UNIQUES",c("myData1"))
  rownames(test.unique) <- NULL
  colnames(test.unique) <- c("UniqueValues")
  
  test.detail <- cbind2(test.na,test.unique)

  ggplot(data=test.detail, 
         aes(x=VariableName, y=UniqueValues)) +
         geom_bar(colour="black", fill="#4C1E9C", width=.8, stat="identity") + 
                  guides(fill=FALSE) +
                  xlab("Variables") + ylab("Record Count") +
                  ggtitle("Unique Value") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        legend.position="none")
  
}

if (firstrun==2) {
    # iris
  test.bestguess <- myBestGuess(iris)
  results.bestguess <- rbind(results.bestguess, test.bestguess)

  # mtcars
  test.bestguess <- myBestGuess(mtcars)
  results.bestguess <- rbind(results.bestguess, test.bestguess)
  
  # UCI arrythemia
  test.input <- read.csv(paste(myOutPath,"arrhythemia.csv",sep=""))
  test.bestguess <- myBestGuess(test.input,"UCI Repository arrhythemia")
  results.bestguess <- rbind(results.bestguess, test.bestguess)
  
  # UCI Dow Jones Index
  test.input <- read.csv("D:/Dissertation-Backup/Data/DowJones/dow_jones_index.data")
  test.bestguess <- myBestGuess(test.input,"Dow Jones")
  results.bestguess <- rbind(results.bestguess, test.bestguess)
  
} 

