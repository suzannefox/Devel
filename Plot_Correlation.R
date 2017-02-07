
workdir <- "C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/"

source("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/Burrow.R")

Data.choose <- "iris"

if (Data.choose=="iris") {
  Data.source <- iris
  Data.title <- "iris"

} else {
  Data.source <- read.csv(paste(workdir,"TestData.csv",sep=""))
  Data.title <- "synthetic dataset"
}

Data.temp <- Burrow(Data.source, Data.title, TRUE)
Data.burrow <- Data.temp$data

library(corrplot)
corrplot(Data.source)
