
if (!exists("firstrun")) {
  
  # Clear environment
  rm(list = ls())
  firstrun <- 1

    # Clear any plots
  graphics.off()
  
  diagnostics <- 0 # Set this to 1 for diagnostic messages
  
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
  diagnostics <- 0

  # development dataset
  test.input <- read.csv(paste(myOutPath,"TestData.csv",sep=""))
  test.burrow <- myBurrow(test.input)
  test.features <- myBurrowFeatures(test.burrow)
  test.bestguess <- myBestGuess(test.input,"Synthetic Development data")
  
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

