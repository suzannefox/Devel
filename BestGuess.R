

diagnostics <- TRUE

# put these in order of precedence and test
# the first one that is true  is the choice
regex_precedence <- c(
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

source("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/Burrow.R")
x <- Burrow(iris, "iris dataset", TRUE)
Burrow <- x$longBurrow

diagnostics <- FALSE
# get all the fields/variables/columns in the dataset
myData.Variables <- subset (Burrow, InfoDetail == "COLUMN_NAME", select = c("myData1"))
# convert to a list
myData.Variables <- myData.Variables[['myData1']]

for (fld in 1:length(myData.Variables)) {
  test.variable <- myData.Variables[fld]
  test.bestguess <- "DK"
  test.guesssource <- "DK"
  test.features <- subset(Burrow, Variable1 == test.variable & myData1 > 0)
  
  if (diagnostics == TRUE)
    print(paste(" ... BestGuess ... field :", test.variable))

  # ==============================================================
  # Step 1.
  # loop around each regex and see if 95% of the data matches any
  # ==============================================================
  for (i in 1:length(regex_precedence)) {
    # if a bestguess has been made then don't bother testing any more
    if (!test.bestguess == "DK")
      break
    # if (diagnostics == TRUE)
    #   print(paste(test.variable, " ... BestGuess ... testing for :", regex_precedence[i]))
    
    test.if <-
      subset(test.features,
             InfoDetail == regex_precedence[i] & myData1 > 0.95)
    if (nrow(test.if) > 0) {
      #latitude and longitude are special cases
      if (regex_precedence[i] == "REGEX_GEO_LATITUDE" &
          test.variable == "LATITUDE") {
        test.bestguess <- gsub("REGEX_", "", regex_precedence[i])
        test.guesssource <- "REGEX MATCH"
        
      } else if (regex_precedence[i] == "REGEX_GEO_LONGITUDE" &
                 test.variable == "LONGITUDE") {
        test.bestguess <- gsub("REGEX_", "", regex_precedence[i])
        test.guesssource <- "REGEX MATCH"
      }
      
      if (regex_precedence[i] == "REGEX_GEO_LATITUDE" |
          regex_precedence[i] == "REGEX_GEO_LONGITUDE") {
        next
      }
      
      # This is a 95% match
      test.bestguess <- gsub("REGEX_", "", regex_precedence[i])
      test.guesssource <- "REGEX MATCH"
      if (diagnostics == TRUE)
        print(paste("BEST GUESS IS", test.bestguess))
      break # break out of this and don't test any more
    } # end of Step 1 - 95% matched data
  }  # end of regex for loop
  
  # ==============================================================
  # Step 2
  # more complicated tests are needed
  # ==============================================================
  # test for decimals
  if (test.bestguess == "DK") {
    if (diagnostics == TRUE)
      print(paste("........................... Decimals test :", test.variable))
    
    test.if <-
      subset(test.features,
             InfoDetail == "REGEX_DECIMALS" | InfoDetail == "REGEX_NUMBER")
    if (nrow(test.if) == 2) {
      test.regextotal <- sum(as.numeric(test.if[, c("myData1")]))
      if (test.regextotal > 0.95)
        test.bestguess <- "DECIMALS"
      test.guesssource <- "MULTIPLE CONDITIONS REGEX MATCH"
      if (diagnostics == TRUE)
        print(paste("BEST GUESS IS", test.bestguess))
    } # end of test.if has 2 records
  } # end of decimals test
  
  # test for category
  if (test.bestguess == "DK") {
    if (diagnostics == TRUE)
      print(paste("........................... Category test :", test.variable))
    
    test.if <-
      subset(
        test.features,
        InfoDetail == "DENSITY_ROW_ALPHAS" |
          InfoDetail == "COUNT_UNIQUES" |
          InfoDetail == "DENSITY_UNIQUES"
      )
    if (nrow(test.if) == 3) {
      test.alphas <-
        as.numeric(test.if[test.if$InfoDetail == "DENSITY_ROW_ALPHAS", c("myData1")])
      test.uniques <-
        as.numeric(test.if[test.if$InfoDetail == "COUNT_UNIQUES", c("myData1")])
      test.uniquesden <-
        as.numeric(test.if[test.if$InfoDetail == "DENSITY_UNIQUES", c("myData1")])
      
      if (diagnostics == TRUE)
        print(test.if)
      if (diagnostics == TRUE)
        print(test.alphas)
      if (diagnostics == TRUE)
        print(test.uniques)
      if (diagnostics == TRUE)
        print(test.uniquesden)
      
      if (test.alphas > 0.95 & test.uniques < 11) {
        test.bestguess <- "CATEGORICAL"
        test.guesssource <- "REGEX ALPHAS 95% MATCH AND < 11 UNIQUES"
      } else if (test.alphas > 0.95 & test.uniques < 21) {
        test.bestguess <- "CATEGORICAL MAYBE"
        test.guesssource <- "REGEX ALPHAS 95% MATCH AND < 21 UNIQUES"
      } else if (test.alphas > 0.90) {
        test.bestguess <-
          sprintf("TEXT %0.0f%% UNIQUE", test.uniquesden * 100)
        test.guesssource <- "REGEX ALPHAS 90% MATCH"
      }
      if (diagnostics == TRUE)
        print(paste("BEST GUESS IS", test.bestguess))
    }
  } # end of category/text tests
  
  # test for numerics but not numbers
  if (test.bestguess == "DK") {
    if (diagnostics == TRUE)
      print(paste("........................... Category test :", test.variable))
    
    test.if <-
      subset(
        test.features,
        InfoDetail == "DENSITY_ROW_DIGITS" |
          InfoDetail == "DENSITY_UNIQUES"
      )
    if (nrow(test.if) == 2) {
      test.digits <-
        as.numeric(test.if[test.if$InfoDetail == "DENSITY_ROW_DIGITS", c("myData1")])
      test.uniquesden <-
        as.numeric(test.if[test.if$InfoDetail == "DENSITY_UNIQUES", c("myData1")])
      
      if (diagnostics == TRUE)
        print(test.if)
      if (diagnostics == TRUE)
        print(test.digits)
      if (diagnostics == TRUE)
        print(test.uniquesden)
      
      if (test.digits > 0.95) {
        test.bestguess <-
          sprintf("MAINLY NUMERIC %0.0f%% UNIQUE", test.uniquesden * 100)
        test.guesssource <-
          "REGEX DIGITS 95% MATCH BUT MAY NOT BE NUMBER FIELD"
      }
      if (diagnostics == TRUE)
        print(paste("BEST GUESS IS", test.bestguess))
    }
  } # end of decimals test
  
  if (diagnostics == TRUE) {
    print(
      paste(
        "... BestGuess ... for ",
        test.variable,
        " is :",
        test.bestguess,
        ", because ",
        test.guesssource,
        SEP = ""
      )
    )
  }
  
}
