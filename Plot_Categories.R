
workdir <- "C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/"
source("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/Burrow.R")

Data.choose <- "Hair"
#Data.choose <- "iris"

if (Data.choose=="iris") {
  Data.source <- iris
  Data.title <- "iris"

} else if (Data.choose=="mtcars") {
  Data.source <- mtcars
  Data.title <- "mtcars"
  
} else if (Data.choose=="Hair") {
  Data.source <- as.data.frame(HairEyeColor)
  Data.title <- "HairEyeColor"
  
} else {
  Data.source <- read.csv(paste(workdir,"TestData.csv",sep=""))
  Data.title <- "synthetic dataset"
}

Data.temp <- Burrow(Data.source, Data.title, TRUE)
Data.burrow <- Data.temp$data

# Get the best guesses
Data.vars.guess <- subset(Data.burrow, 
                           InfoType=="BEST GUESS")

# Get all the variables which are number fields
Data.vars.category <- subset(Data.burrow, 
                           InfoType=="BEST GUESS" 
                           & (myData1=="CATEGORICAL"))

if (nrow(Data.vars.category)==0) {
  print("No categorical variables in this dataset")
  
} else {
  Data.list.categories <- as.list(Data.vars.category[c("Variable1")])
  Data.categorical <- as.data.frame(Data.source[,c(Data.list.categories$Variable1)])
  
  table(Data.categorical)

}

# myData <- Data.source
# myData.mean <- as.data.frame(suppressWarnings(sapply(myData, mean, na.rm=TRUE)))
# names(myData.mean) <- c("MeanVal")
# myData.mean1 <- as.data.frame(myData.mean[complete.cases(myData.mean),])
# myData.mean2 <- subset(myData.mean, !is.na(MeanVal))
