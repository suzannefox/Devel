
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

if (firstrun==1) {
  test.input <- read.csv(paste(myOutPath,"TestData.csv",sep=""))
  test.data <- test.input
  #test.data <- subset(test.input,,c("ID","Dist1","Dist3"))

  test.burrow <- myBurrow(test.data)
  test.features <- myBurrowFeatures(test.burrow)
  #test.Dist1  <- subset(test.burrow, Variable1=="Dist1" )
  
  # for each variable test the regex list and find the first match  
  regexsearch <- c(
    "REGEX_DATE_DDMMYYYY_DASHES",
    "REGEX_DATE_DDMMYYYY_SLASHES",
    "REGEX_CURRENCY_UK",
    "REGEX_CURRENCY_USA",
    "REGEX_BIT_ONEZERO",
    "REGEX_BIT_TRUEFALSE",
    "REGEX_BIT_TF",
    "REGEX_BIT_YN",
    "REGEX_GEO_POSTCODE",
    "REGEX_SCALE_210120",
    "REGEX_PERCENT",
    "REGEX_SCIENTIFIC",
    "REGEX_TIMES10",
    "REGEX_WEB_URL",
    "REGEX_WEB_EMAIL",
    "REGEX_DECIMALS",
    "REGEX_GEO_LATITUDE",
    "REGEX_GEO_LONGITUDE",
    "REGEX_NUMBER"
  )

  # get list of variables
  myBurrowData <- test.burrow
  myData.Variables <- subset(myBurrowData, InfoDetail=="COLUMN_NAME", select=c("InfoDetail", "myData1"))
  myData.BestGuess <- c()
  
  diagnostics <- 0
  # ========================================================================================
  # for each variable, test the regex conditions and find the best guess at the data type
  # if a regex is matched by 95%+ of the data then choose that one, otherwise
  # build some more complicated test. For instance a numeric field might be mostly 
  # decimals, but have some whole numbers in too
  # ========================================================================================
  for(i in 1:nrow(myData.Variables)) { 
    
    # get variable name    
    myVariable <- myData.Variables[i, "myData1"]
    test.bestguess <- "DK"

    # make a df for each variable of all records where there is data
    test.choose <- subset(test.burrow, Variable1==myVariable & myData1 > 0)
    
    # ==============================================================
    # Step 1.
    # loop around each regex and see if 95% of the data matches any
    # ==============================================================
    for (i in 1:length(regexsearch)) { 
      # if a bestguess has been made then don't bother testing any more
      if (!test.bestguess=="DK") break
      if (diagnostics==1) print(paste(myVariable,"testing for",regexsearch[i]))
    
      test.if <- subset(test.choose, InfoDetail==regexsearch[i] & myData1 > 0.95)
      if (nrow(test.if) > 0) {
        #latitude and longitude are special cases
          if (regexsearch[i]=="REGEX_GEO_LATITUDE" & myVariable=="LATITUDE") {
              test.bestguess <- regexsearch[i]
              print(paste(".... ?? MATCH",myVariable,regexsearch[i]))

          } else if (regexsearch[i]=="REGEX_GEO_LONGITUDE" & myVariable=="LONGITUDE") {
              test.bestguess <- regexsearch[i]
              print(paste(".... ?? MATCH",myVariable,regexsearch[i]))
          }
          
          if (regexsearch[i]=="REGEX_GEO_LATITUDE" | regexsearch[i]=="REGEX_GEO_LONGITUDE") {
            next
          }

        # This is a 95% match
        test.bestguess <- regexsearch[i]
        if (diagnostics==1) print(paste("BEST GUESS IS",test.bestguess))
        break # break out of this and don't test any more
      } # end of Step 1 - 95% matched data
    }  # end of regex for loop
    
    # ==============================================================
    # Step 2
    # more complicated tests are needed
    # ==============================================================
    if (test.bestguess=="DK") {
      print(paste("..................................... Step 2 :", myVariable))
      test.if <- subset(test.choose, InfoDetail=="REGEX_DECIMALS" | InfoDetail=="REGEX_NUMBER")
      if (nrow(test.if) == 2) {
        test.regextotal <- sum(as.numeric(test.if[,c("myData1")]))
        if (test.regextotal > 0.95) test.bestguess <- "REGEX_DECIMALS"
        if (diagnostics==1) print(paste("BEST GUESS IS",test.bestguess))
      } # end of test.if has 2 records

      print(paste("..................................... Step 3 :", myVariable))
    } # end of step 2 if block
    
    myData.BestGuess <- c(myData.BestGuess , c=test.bestguess)
    if (diagnostics==1) print(paste("guesses",myData.BestGuess))
    
  } # end of variable loop

  # print the results
  for(i in 1:nrow(myData.Variables)) { 
    print(paste(myData.Variables[i, "myData1"],myData.BestGuess[i]))
  }
} 

# if (firstrun==2) {
# 
#   burrow <- myBurrow(mtcars)
#   features <- myBurrowFeatures(burrow)
#   features.all <- features
#   
#   burrow <- myBurrow(iris)
#   features <- myBurrowFeatures(burrow)
#   features.all <- features
#   
#   burrow <- myBurrow(crimtab)
#   features <- myBurrowFeatures(burrow)
#   features.all <- rbind(features.all, features)
#   
#   burrow <- myBurrow(AirPassengers)
#   features <- myBurrowFeatures(burrow)
#   features.all <- rbind(features.all, features)
#   
#   myFile <- paste(myDataPath, "/Test-UK500/uk-500.csv", sep="")
#   burrow <- myBurrow(myFile)
#   features <- myBurrowFeatures(burrow)
#   features.all <- rbind(features.all, features)
#   
#   myFile <- paste(myDataPath, "/Test-CA500/ca-500.csv", sep="")
#   burrow <- myBurrow(myFile)
#   features <- myBurrowFeatures(burrow)
#   features.all <- rbind(features.all, features)
#   
#   write.csv(features.all,paste(myDataPath,"/Features.csv",sep=""))
#   stop("Finished processing", call. = FALSE, NULL)
# }
# 
# # barplot(features$DENSITY_UNIQUES)
# # barplot(features$REGEX_NUMBER)
# # 
# # ggplot(data = features, aes(COLUMN_NAME, DENSITY_UNIQUES))+
# # geom_bar(stat="identity", fill="white")
# # 
# # library(ggplot2)
# # ggplot(data = features, aes(COLUMN_NAME, DENSITY_UNIQUES, labels=COLUMN_NAME))+
# #   geom_bar(stat="identity", fill="white")+
# #   geom_text(features, angle=90, colour="blue")
# #  geom_text(features, mapping=aes(x=COLUMN_NAME, y=0.05), label=COLUMN_NAME, angle=90, colour="blue")
# 
# # myFile <- paste(myDataPath, "/ml-20m/ratings.csv", sep="")
# # burrow <- myBurrow(myFile)
# # features <- myBurrowFeatures(burrow)
# # features.all <- rbind(features.all, features)
# 
# # BigBurrow <- burrow
# # write.csv(BigBurrow, file = paste(myOutPath,"BigBurrow.csv",sep=""))