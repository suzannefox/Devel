
# ==============================================================================
# 27 November 2016
# INPUT : A dataframe in the format created by the myBurrow function
#
# OUTPUT : A dataframe with the following structure -
#          - a row per "InfoDetail==COLUMN_NAME" in the myBurrow dataframe 
#            (ie variable in the source data). 
#          - a column per "InfoType==FEATURE" in the myBurrow dataframe
#            if more features are added to the myBurrow they will be picked
#            up automatically so there is a variable number of Columns
#          - a column with the "BEST_GUESS" at the data type of each variable
#            and a "GUESS_SOURCE" column with reason why this guess was the best
# ==============================================================================
myBurrowFeatures <- function(myBurrowData) {
  
  diagnostics <- 0
  # get the source filename
  myData.1 <- subset(myBurrowData, InfoDetail=="FILE_NAME",  select=c("InfoDetail", "myData1"))
  
  # get list of variables
  myData.Variables <- subset(myBurrowData, InfoDetail=="COLUMN_NAME", select=c("InfoDetail", "myData1"))
  
  # iterate round the variables. i is the counter of the variable
  #for(i in 1:1) { 
  for(i in 1:nrow(myData.Variables)) { 

    # get variable name    
    myVariable <- myData.Variables[i, "myData1"]

    # make a df for the name
    myData.2 <- subset(myBurrowData, Variable1=="COLNAME" & myData1 == myVariable, 
                       select=(c("InfoDetail","myData1")))
    
    # get all the data for this variable
    myData.VariableData <- subset(myBurrowData, Variable1 == myVariable)
    
    # get the r type
    myData.3 <- subset(myData.VariableData, InfoDetail=="DATA_TYPE_R", 
                       select=(c("InfoDetail","myData1")))
    
    # get the features
    myData.4 <- subset(myData.VariableData, InfoType=="FEATURE", 
                       select=(c("InfoDetail","myData1")))
    
    # This has 2 columns, InfoDetail and myData1 which contain a mix of character and numerics  
    myData.RawFeatures = rbind.data.frame(myData.1, myData.2, myData.3, myData.4)
    
    # Transpose the dataframe
    myData.5 <- t(myData.RawFeatures[,c(2)])
    # make the column names the InfoDetail text
    colnames(myData.5)<- myData.RawFeatures[,c(1)]
    # make it a dataframe with no factors
    myData.5 <- as.data.frame(myData.5, stringsAsFactors = FALSE)
    # cols 1-3 will be character data. 
    # col 4 to the end will eb numeric
    myData.6a <- myData.5[,c(1:3)]
    myData.6b <- as.data.frame(lapply(myData.5[,c(4:ncol(myData.5))], as.numeric))
    myData.6 <- cbind(myData.6a, myData.6b)
    
    # construct the dataframe to return  
    if (i==1) {
      myFeatures <- as.data.frame(myData.6)
    } else {
      myFeatures <- rbind(myFeatures, myData.6)
    }
  }

  # ===========================================================================
  # Add the BestGuess Column
  # ===========================================================================
  # put these in order of precedence and test
  # the first one that is true  is the choice
  regexsearch <- c(
    "REGEX_DATE_DDMMYYYY_DASHES",
    "REGEX_DATE_DDMMYYYY_SLASHES",
    "REGEX_CURRENCY_UK",
    "REGEX_CURRENCY_USA",
    "REGEX_BIT_ONEZERO",
    "REGEX_BIT_TRUEFALSE",
    "REGEX_BIT_TF",
    "REGEX_BIT_YN",
    "REGEX_BIT_YESNO",
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
  
  myFeatures$BEST_GUESS <- ""
  myFeatures$GUESS_SOURCE <- ""
  # ========================================================================================
  # for each variable, test the regex conditions and find the best guess at the data type
  # if a regex is matched by 95%+ of the data then choose that one, otherwise
  # build some more complicated test. For instance a numeric field might be mostly 
  # decimals, but have some whole numbers in too
  # ========================================================================================
  for(j in 1:nrow(myData.Variables)) { 
    
    # get variable name    
    myVariable <- myData.Variables[j, "myData1"]
    test.bestguess <- "DK"
    test.guesssource <- "DK"
    
    # make a df for each variable of all records where there is data
    test.choose <- subset(myBurrowData, Variable1==myVariable & myData1 > 0)
    
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
          test.bestguess <- gsub("REGEX_","",regexsearch[i])
          test.guesssource <- "REGEX MATCH"

        } else if (regexsearch[i]=="REGEX_GEO_LONGITUDE" & myVariable=="LONGITUDE") {
          test.bestguess <- gsub("REGEX_","",regexsearch[i])
          test.guesssource <- "REGEX MATCH"
        }
        
        if (regexsearch[i]=="REGEX_GEO_LATITUDE" | regexsearch[i]=="REGEX_GEO_LONGITUDE") {
          next
        }
        
        # This is a 95% match
        test.bestguess <- gsub("REGEX_","",regexsearch[i])
        test.guesssource <- "REGEX MATCH"
        if (diagnostics==1) print(paste("BEST GUESS IS",test.bestguess))
        break # break out of this and don't test any more
      } # end of Step 1 - 95% matched data
    }  # end of regex for loop
    
    # ==============================================================
    # Step 2
    # more complicated tests are needed
    # ==============================================================
    # test for decimals
    if (test.bestguess=="DK") {
      if (diagnostics==1) print(paste("........................... Decimals test :", myVariable))
      
      test.if <- subset(test.choose, InfoDetail=="REGEX_DECIMALS" | InfoDetail=="REGEX_NUMBER")
      if (nrow(test.if) == 2) {
        test.regextotal <- sum(as.numeric(test.if[,c("myData1")]))
        if (test.regextotal > 0.95) test.bestguess <- "DECIMALS"
        test.guesssource <- "MULTIPLE CONDITIONS REGEX MATCH"
        if (diagnostics==1) print(paste("BEST GUESS IS",test.bestguess))
      } # end of test.if has 2 records
    } # end of decimals test
    
    # test for category
    if (test.bestguess=="DK") {
      if (diagnostics==1) print(paste("........................... Category test :", myVariable))
      
      test.if <- subset(test.choose, InfoDetail=="DENSITY_ROW_ALPHAS" | 
                                     InfoDetail=="COUNT_UNIQUES" | 
                                     InfoDetail=="DENSITY_UNIQUES")
      if (nrow(test.if) == 3) {
        test.alphas <- as.numeric(test.if[test.if$InfoDetail=="DENSITY_ROW_ALPHAS",c("myData1")])
        test.uniques <- as.numeric(test.if[test.if$InfoDetail=="COUNT_UNIQUES",c("myData1")])
        test.uniquesden <- as.numeric(test.if[test.if$InfoDetail=="DENSITY_UNIQUES",c("myData1")])

        if (diagnostics==1) print(test.if)
        if (diagnostics==1) print(test.alphas)
        if (diagnostics==1) print(test.uniques)
        if (diagnostics==1) print(test.uniquesden)
        
        if (test.alphas > 0.95 & test.uniques < 11) {
          test.bestguess <- "CATEGORICAL"
          test.guesssource <- "REGEX ALPHAS 95% MATCH AND < 11 UNIQUES"
        } else if (test.alphas > 0.95 & test.uniques < 21) {
          test.bestguess <- "CATEGORICAL MAYBE"
          test.guesssource <- "REGEX ALPHAS 95% MATCH AND < 21 UNIQUES"
        } else if (test.alphas > 0.90) {
          test.bestguess <- sprintf("TEXT %0.0f%% UNIQUE", test.uniquesden * 100)
          test.guesssource <- "REGEX ALPHAS 90% MATCH"
        }
        if (diagnostics==1) print(paste("BEST GUESS IS",test.bestguess))
      }
    } # end of category/text tests
    
    # test for numerics but not numbers
    if (test.bestguess=="DK") {
      if (diagnostics==1) print(paste("........................... Category test :", myVariable))
      
      test.if <- subset(test.choose, InfoDetail=="DENSITY_ROW_DIGITS" | 
                          InfoDetail=="DENSITY_UNIQUES")
      if (nrow(test.if) == 2) {
        test.digits <- as.numeric(test.if[test.if$InfoDetail=="DENSITY_ROW_DIGITS",c("myData1")])
        test.uniquesden <- as.numeric(test.if[test.if$InfoDetail=="DENSITY_UNIQUES",c("myData1")])
        
        if (diagnostics==1) print(test.if)
        if (diagnostics==1) print(test.digits)
        if (diagnostics==1) print(test.uniquesden)
        
        if (test.digits > 0.95) {
          test.bestguess <- sprintf("MAINLY NUMERIC %0.0f%% UNIQUE", test.uniquesden * 100)
          test.guesssource <- "REGEX DIGITS 95% MATCH BUT MAY NOT BE NUMBER FIELD"
        }
        if (diagnostics==1) print(paste("BEST GUESS IS",test.bestguess))
      }
    } # end of decimals test
    
    myFeatures[j,"BEST_GUESS"] <- test.bestguess
    myFeatures[j,"GUESS_SOURCE"] <- test.guesssource
    if (diagnostics==1) print(paste("guesses",myData.Variables[j,"BEST_GUESS"]))
    
  } # end of variable loop
  
  # ===========================================================================
  # End of the best guess section
  # ===========================================================================

  # ===========================================================================
  # Now reorganise the output dataframe so the columns are in a sensible order
  # with the BEST_GUESS and GUESS_SOURCE at the beginning, as the number of feature 
  # columns is variable as new features can be added to the myBurrow data, it 
  # needs to be a general method of reordering
  # ===========================================================================
  
  names.all <- names(myFeatures)
  names.first <- c("FILE_NAME","COLUMN_NAME","BEST_GUESS","GUESS_SOURCE")
  names.variable <- !grepl("^FILE_NAME$|^COLUMN_NAME$|^BEST_GUESS$|^GUESS_SOURCE$",names.all)
  names.last <- names.all[names.variable]
  #names.reordered <- c(names.first,names.last)
  #myFeatures <- myFeatures[names.reordered]
  myFeatures <- myFeatures[c(names.first,names.last)]
  return(myFeatures)
}

# ==============================================================================
# 27 November 2016
# INPUT : A source dataset
#         optionally a name for this dataset, if none is specified the name
#         of myData is used, but sometimes (depending on the way the dataset
#         was processed before passing to this function) this is not meaningful
#
# OUTPUT : A dataframe with the BEST_GUESS and THE GUESS_SOURCE for each variable
#          in the source dataset
# ==============================================================================
myBestGuess <- function(myData, myDataName="") {
  if (myDataName=="") {
    myDataName <- as.character(c(deparse(substitute(myData))))
  }
  guess.burrow <- myBurrow(myData, myDataName)
  guess.features <- myBurrowFeatures(guess.burrow)
  #names.3 <- c("FILE_NAME","COLUMN_NAME","BEST_GUESS","GUESS_SOURCE")
  #return(subset(guess.features,,names.3))
  guess.features <- guess.features[c("FILE_NAME","COLUMN_NAME","BEST_GUESS","GUESS_SOURCE")]
  return (guess.features)
  
}  