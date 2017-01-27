
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
    #myOutPath = "D:/Dissertation-Temp/"
    #myDataPath = "D:/Dissertation-Backup/Data/"
    myOutPath = "C:/Users/suzan/Documents/Dissertation-temp/R/"
    myDataPath = "C:/Users/suzan/Documents/Dissertation-temp/R/"
  }
  setwd(myOutPath)
  getwd()
  
  #source("myBurrow.R")
  #source("myBurrowFeatures.R")
  source("function_Burrow.R")

  # return() only works in functions
  stop("Finished set-up", call. = FALSE, NULL)
}

if (firstrun==1) {
  # input is the synthetic test data
  burrow.inputdata <- read.csv(paste(myOutPath,"TestData.csv",sep=""))
  burrow.title <- "mtcars dataset"
  
  #burrow.inputdata <- iris
  #burrow.title <- "iris dataset"
  
  # get the outputs
  burrow.data <- BurrowData(burrow.inputdata)
  burrow.bestguess <- BurrowBestGuess(burrow.inputdata,burrow.title)
  burrow.report <- BurrowReport(burrow.inputdata)

  # get the plots
  burrow.plothist <- subset(burrow.report, BEST_GUESS=="DECIMALS")
  
  for (i in seq_len(nrow(burrow.plothist))) {
    plotvar <- burrow.plothist[i,c("VariableName")]
    print(paste("Plotting",plotvar))
    
    # possibleError <- tryCatch(
    #   myPlot <- myPlot_Int(burrow.inputdata,plotvar),
    #   error=function(e) e
    # )
    
    p1 <- myPlot_Int(burrow.inputdata,plotvar,1)
    print(p1)

    # if(inherits(possibleError, "error")) {
    #   print(paste("error plotting",plotvar,"is",possibleError))
    #   next
    # }
    
  }

}  

if (firstrun==2) {
  #diagnostics <- 0

  # development dataset
  test.input <- read.csv(paste(myOutPath,"TestData.csv",sep=""))
  test.burrow <- BurrowData(test.input)
  test.features <- BurrowDataFeatures(test.burrow)
  test.bestguess <- BurrowBestGuess(test.input,"Synthetic Development data")
  
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
  test.bestguess <- BurrowBestGuess(iris)
  results.bestguess <- rbind(results.bestguess, test.bestguess)

  # mtcars
  test.bestguess <- BurrowBestGuess(mtcars)
  results.bestguess <- rbind(results.bestguess, test.bestguess)
  
  # UCI arrythemia
  test.input <- read.csv(paste(myOutPath,"arrhythemia.csv",sep=""))
  test.bestguess <- BurrowBestGuess(test.input,"UCI Repository arrhythemia")
  results.bestguess <- rbind(results.bestguess, test.bestguess)
  
  # UCI Dow Jones Index
  test.input <- read.csv("D:/Dissertation-Backup/Data/DowJones/dow_jones_index.data")
  test.bestguess <- BurrowBestGuess(test.input,"Dow Jones")
  results.bestguess <- rbind(results.bestguess, test.bestguess)
  
} 

