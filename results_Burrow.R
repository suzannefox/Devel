
workdir <- "C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/"
datadir <- "C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/Data/ml-20m/"
source("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/Burrow.R")

# =====================================================================
# makes a results frame if one doesn't exist

if (!exists("results")) {
  results <- data.frame(InfoType=character(),
                        Variable1=character(),
                        myData1=character(),
                        InfoDetail=character(),
                        Source=character(),
                        Recsize=double(),
                        proctime=double(),
                        stringsAsFactors=FALSE)
}

# =====================================================================

source <- list(
               mtcars,"mtcars dataset",
               HairEyeColor,"Person chracteristics dataset",
               WorldPhones,"World phones",
               airmiles,"airmiles",
               attenu,"attenuation data",
               chickwts,"Chicken Weights",
               esoph,"Smoking, Alcohol and (O)esophageal Cancer",
               warpbreaks,"The Number of Breaks in Yarn during Weaving",
               swiss,"Swiss Fertility and Socioeconomic Indicators (1888)",
               quakes,"Quakes off Fiji")

startat <- 1
for (i in seq(startat, length(source), 2)) {
  
  Data.source <- as.data.frame(source[[i]])
  Data.title <- source[[i + 1]]
  
  # get the data
  Data.temp <- Burrow(Data.source, Data.title, FALSE)
  Data.burrow <- Data.temp$data
  Data.time <- Data.temp$runStatsProcTime
  Data.guess <- subset(Data.burrow, InfoType=="BEST GUESS",select=c("InfoType","Variable1","myData1","InfoDetail"))
  Data.guess$source <- Data.title
  Data.guess$recsize <- nrow(Data.source)
  Data.guess$proctime <- Data.time

  x <- sprintf("%s, %f",Data.title, Data.time)  
  print(x)
}

# # =====================================================================
# # get some examples from SQL northwind
# library(RODBC)
# sqldb <- odbcDriverConnect('driver={SQL Server};server=P37\\SQLEXPRESS;database=Northwind;trusted_connection=true;rows_at_time=1')
# 
# Data.source <- sqlQuery(sqldb, 'select * from information_schema.tables')
# Data.title <- "SQL Northwind information_schema"
# 
# # get the burrow data
# Data.temp <- Burrow(Data.source, Data.title, FALSE)
# Data.burrow <- Data.temp$data
# Data.time <- Data.temp$runStatsProcTime
# Data.guess <- subset(Data.burrow, InfoType=="BEST GUESS",select=c("InfoType","Variable1","myData1","InfoDetail"))
# Data.guess$source <- Data.title
# Data.guess$recsize <- nrow(Data.source)
# Data.guess$proctime <- Data.time
# 
# results <- rbind(results,Data.guess)
# 
# sqltxt <- "select OrderID,
# CustomerID,
# EmployeeID,
# OrderDate,
# RequiredDate,
# COALESCE(ShippedDate,'') as ShippedDate,
# ShipVia,
# Freight,
# ShipName,
# ShipAddress,
# ShipCity,
# COALESCE(ShipRegion,'') as ShipRegion,
# COALESCE(ShipPostalCode,'') as ShipPostalCode,
# ShipCountry
# from orders"
# 
# Data.source <- sqlQuery(sqldb, sqltxt)
# Data.title <- "SQL Northwind Orders table"
# 
# # get the burrow data
# Data.temp <- Burrow(Data.source, Data.title, TRUE)
# Data.burrow <- Data.temp$data
# Data.time <- Data.temp$runStatsProcTime
# Data.guess <- subset(Data.burrow, InfoType=="BEST GUESS",select=c("InfoType","Variable1","myData1","InfoDetail"))
# Data.guess$source <- Data.title
# Data.guess$recsize <- nrow(Data.source)
# Data.guess$proctime <- Data.time
# 
# Data.guess
# #results <- rbind(results,Data.guess)

# =====================================================================
# results for some csv files

csv <- list(
            "ratings.csv","Movie Ratings",
            "movies.csv","Movies",
            "tags.csv","Movie Tags",
            "accidents0514.csv","STATS19 Accident data",
            "student-por.csv","UCI Student performance dataset - Portuguese",
            "student-mat.csv","UCI Student performance dataset - Maths")

startat <- 5
for (i in seq(startat, length(csv), 2)) {
  
  Data.file <- paste(datadir,csv[[i]],sep="")
  Data.source <- read.csv(Data.file)
  Data.title <- csv[[i + 1]]
  
  # get the data
  Data.temp <- Burrow(Data.source, Data.title, FALSE)
  Data.burrow <- Data.temp$data
  Data.time <- Data.temp$runStatsProcTime
  Data.guess <- subset(Data.burrow, InfoType=="BEST GUESS",select=c("InfoType","Variable1","myData1","InfoDetail"))
  Data.guess$source <- Data.title
  Data.guess$recsize <- nrow(Data.source)
  Data.guess$proctime <- Data.time
  
  x <- sprintf("%s, %f",Data.title, Data.time)  
  print(x)
  
}
