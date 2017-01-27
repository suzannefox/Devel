
firstrun <- 0

if (firstrun==1) {
  # Clear environment
  rm(list = ls())
  # Clear any plots
  graphics.off()
  
  diagnostics <- 0 # Set this to 1 for diagnostic messages
  
  # --------------------------------------------------------
  # Get current environment, and set working path accordingly
  myMachine <- Sys.info()["nodename"]
  if (myMachine == 'P37') {
    myOutPath = "E:/MSc Course/Dissertation/R"
    myDataPath = "E:/MSc Course/Dissertation/Data"
  } else {
    myOutPath = "D:/Dissertation-Temp/"
    myDataPath = "D:/Dissertation-Backup/Data"
  }
  setwd(myOutPath)
  getwd()
  
  source("myBurrow.R")
  source("myBurrowFeatures.R")
  
  # return() only works in functions
  stop("Finished set-up", call. = FALSE, NULL)
}

if (firstrun==0) {

  burrow <- myBurrow(mtcars)
  features <- myBurrowFeatures(burrow)
  features.all <- features
  
  burrow <- myBurrow(iris)
  features <- myBurrowFeatures(burrow)
  features.all <- features
  
  burrow <- myBurrow(crimtab)
  features <- myBurrowFeatures(burrow)
  features.all <- rbind(features.all, features)
  
  burrow <- myBurrow(AirPassengers)
  features <- myBurrowFeatures(burrow)
  features.all <- rbind(features.all, features)
  
  myFile <- paste(myDataPath, "/Test-UK500/uk-500.csv", sep="")
  burrow <- myBurrow(myFile)
  features <- myBurrowFeatures(burrow)
  features.all <- rbind(features.all, features)
  
  myFile <- paste(myDataPath, "/Test-CA500/ca-500.csv", sep="")
  burrow <- myBurrow(myFile)
  features <- myBurrowFeatures(burrow)
  features.all <- rbind(features.all, features)
  
  write.csv(features.all,paste(myDataPath,"/Features.csv",sep=""))
  stop("Finished processing", call. = FALSE, NULL)
}

barplot(features$DENSITY_UNIQUES)
barplot(features$REGEX_NUMBER)

ggplot(data = features, aes(COLUMN_NAME, DENSITY_UNIQUES))+
geom_bar(stat="identity", fill="white")

library(ggplot2)
ggplot(data = features, aes(COLUMN_NAME, DENSITY_UNIQUES, labels=COLUMN_NAME))+
  geom_bar(stat="identity", fill="white")+
  geom_text(features, angle=90, colour="blue")
#  geom_text(features, mapping=aes(x=COLUMN_NAME, y=0.05), label=COLUMN_NAME, angle=90, colour="blue")

# myFile <- paste(myDataPath, "/ml-20m/ratings.csv", sep="")
# burrow <- myBurrow(myFile)
# features <- myBurrowFeatures(burrow)
# features.all <- rbind(features.all, features)

# BigBurrow <- burrow
# write.csv(BigBurrow, file = paste(myOutPath,"BigBurrow.csv",sep=""))