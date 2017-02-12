
workdir <- "C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/"
source("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/Burrow.R")

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

  Data.temp <- Burrow(Data.source, Data.title, FALSE)
  Data.burrow <- Data.temp$data

  # get all the variables
  vars <- subset(Data.burrow, InfoDetail=="COLUMN_NAME", select = c("myData1"))
  vars <- vars[["myData1"]]

  for (j in seq(1, length(vars))) {
  
    # header
    myvar <- vars[[j]]
    print(myvar)
    
    n = c("Variable")
    m = c(myvar)
    header = data.frame(InfoDetail=n, myData1=m, stringsAsFactors = FALSE)
  
    # get the best guess
    guess <- subset(Data.burrow, InfoType=="BEST GUESS" & Variable1==myvar, 
                    select=c("InfoType", "myData1"))
  
    n = c("Guess")
    m = guess[["myData1"]]
    bestguess = data.frame(InfoDetail=n, myData1=m, stringsAsFactors = FALSE)
    
    header <- rbind(header, bestguess)
    
    # get the features
    feats <- subset(Data.burrow, InfoType=="FEATURE" & VarLevel=="UNIVARIATE" & Variable1==myvar, 
                                 select=c("InfoDetail", "myData1"))
  
    
    # add the variable name as a feature
    feats <- rbind(header, feats)
    # get names
    mynames <- feats$InfoDetail
    # get data
    myfeats <- as.data.frame(feats$myData1)
    # transposed dataframe
    featst <- as.data.frame(t(myfeats))
    # set column names
    colnames(featst) <- mynames
  
    if (j==1) {
      final <- featst
    } else {
      final <- rbind(final, featst)
    }
    print(nrow(final))
    print("")
  }

  if (i==1) {
    allfeats <- final
  } else {
    allfeats <- rbind(allfeats, final)
  }
}