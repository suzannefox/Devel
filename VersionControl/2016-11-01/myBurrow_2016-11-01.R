

# This is the Input function
# myInput can be a file or a dataframe
myBurrow <- function(myInput) {
  
  # Start time
  tstart <- proc.time()
  
  # Initialise a blank dataframe for the burrow data
  df.burrow <- data.frame(InfoLevel=character(),
                          InfoType=character(),
                          InfoDetail=character(),
                          VariableLevel=character(),
                          Variable1=character(),
                          Data1=double(),
                          Variable2=character(),
                          Data2=double(),
                          Notes=character(),
                          stringsAsFactors=FALSE)

    if (diagnostics==1) {print(paste("... Input variable has class",class(myInput)))}
  
  # =============================================================
  # if dataframe has been input, just make a copy
  # if a table, convert to dataframe first
  # otherwise assume it's a file and read it into a dataframe
  # =============================================================

  if (class(myInput)=="data.frame") {
    if (diagnostics==1) print("df")
    myData <- myInput
    myInput <- c("DATA.FRAME",deparse(substitute(myInput)))
    
  } else if (class(myInput)=="table") {
    if (diagnostics==1) print("table")
    myData <- as.data.frame(myInput)
    myInput <- c("TABLE",deparse(substitute(myInput)))
    
  } else if (class(myInput)=="ts") {
    if (diagnostics==1) print("ts")
    myData <- as.data.frame(myInput)
    myInput <- c("TIMESERIES",deparse(substitute(myInput)))

  } else if (class(myInput)!="character") {
    if (diagnostics==1) print("not char")
    stop(paste("Can't handle input class of", class(myInput)))
    
  } else {
    # check file exists
    if (file.exists(myInput)) {
      myData <- read.csv(myInput, stringsAsFactors = FALSE)
      myInput <- c("FILE", myInput, basename(myInput))
    } else {
      stop(paste("No such file as", myInput))
    }
  }
  
  if (diagnostics==1) print("... writing to dataframe")
  # =============================
  # Write to the output dataframe
  # =============================

  df.burrow <- write_Fileinfo(myInput, myData, df.burrow)  # write source details
  for(i in names(myData)) {                                # write Field details
    df.burrow <- write_Fieldinfo(myData, i, df.burrow)
  }
  
  # time taken
  tused <- proc.time() - tstart
  
  myArgs.InfoLevel <- "FILE"
  myArgs.InfoType <- "PROC_TIME"
  myArgs.InfoDetail <- "USER"
  myArgs.Variable1 <- ""
  myArgs.Data1 <- tused[1]
  myArgs.Variable2 <- ""
  myArgs.Data2 <- ""
  myArgs.Notes <- ""
  df.burrow = myContentLine(df.burrow,
                            myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  myArgs.InfoDetail <- "SYSTEM"
  myArgs.Data1 <- tused[2]
  df.burrow = myContentLine(df.burrow,
                            myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)

  myArgs.InfoDetail <- "ELAPSED"
  myArgs.Data1 <- tused[3]
  df.burrow = myContentLine(df.burrow,
                            myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)

  # ==========================
  # return the dataframe
  # ==========================
  rownames(df.burrow) <- NULL
  return(df.burrow)
}

# ==============================================================
# Write the file level details
# ==============================================================
write_Fileinfo <- function(myInput, myData, df.burrow) {
  
  if (diagnostics==1) {print("... Writing source details")}
  
  # write the data source details
  myArgs.InfoLevel <- "FILE"
  myArgs.InfoType <- "SOURCE"
  myArgs.InfoDetail <- myInput[1]
  myArgs.Variable1 <- ""
  myArgs.Data1 <- myInput[2]
  myArgs.Variable2 <- ""
  myArgs.Data2 <- ""
  myArgs.Notes <- ""
  df.burrow = myContentLine(df.burrow,
                         myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                         myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                         myArgs.Notes)

    myArgs.InfoLevel <- "FILE"
    myArgs.InfoDetail <- "FILE_NAME"
    if (myInput[1]=="FILE") {
      myArgs.Data1 <- myInput[3]
    } else {
      myArgs.Data1 <- myInput[2]
    }
    df.burrow = myContentLine(df.burrow,
                              myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)
    
  # number of cols
  myArgs.InfoType <- "DETAIL"
  myArgs.InfoDetail <- "COUNT_COLUMNS"
  myArgs.Variable1 <- "NUMCOLS"
  myArgs.Data1 <- ncol(myData)
  myArgs.Notes <- "ncol(myData)"
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                myArgs.Notes)
  

  # number of rows
  myArgs.InfoType <- "DETAIL"
  myArgs.InfoDetail <- "COUNT_ROWS"
  myArgs.Variable1 <- "NUMROWS"
  myArgs.Data1 <- nrow(myData)
  myArgs.Notes <- "nrow(myData)"
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                myArgs.Notes)

  # Column names
  if (ncol(myData) < 100) {
    nameTemplate = "Field%02d"
  } else {
    nameTemplate = "Field%03d"
  }

  #nameClass <- sapply(myData,class)
  #nameTypeof <- sapply(myData,typeof)

  if (diagnostics==1) {print("... Analysing each field")}
  icount <- 0
  for (i in names(myData)) {
    icount <- icount + 1

    myArgs.InfoDetail <- "COLUMN_NAME"
    myArgs.Variable1 <- "COLNAME"
    myArgs.Data1 <- i
    myArgs.Notes <- "names(myData)"
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)


    myArgs.InfoDetail <- "ORDINAL_POSITION"
    myArgs.Variable1 <- i
    myArgs.Data1 <- icount
    myArgs.Notes <- ""
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    myArgs.InfoDetail <- "DATA_CLASS_R"
    myArgs.Variable1 <- i
    myArgs.Data1 <- class(myData[,i])
    myArgs.Notes <- "class(myData[,Field])"
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)

    myArgs.InfoDetail <- "DATA_TYPE_R"
    myArgs.Variable1 <- i
    myArgs.Data1 <- typeof(myData[,i])
    myArgs.Notes <- "sapply(myData,typeof)"
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
  }
  return(df.burrow)
  if (diagnostics==1) {print("... Finished Analysing each field")}
  if (diagnostics==1) {print("")}
}

# ==============================================================
# Write the field level details
# ==============================================================
write_Fieldinfo <- function(myData, Field, df.burrow) {
  
  if (diagnostics > 0) {
    print(sprintf("... Analysing %s", Field)) 
  }

  # write the Field details
  myArgs.InfoLevel <- "FIELD"
  myArgs.InfoType <- "COUNTS"
  myArgs.InfoDetail <- ""
  myArgs.Variable1 <- Field
  myArgs.Data1 <- ""
  myArgs.Variable2 <- ""
  myArgs.Data2 <- ""
  myArgs.Notes <- ""
  
  # Find the class of data as analysis will be different for different datatype
  varclass <- class(myData[,Field])
  if (diagnostics > 0) {print(sprintf("varclass is %s", varclass))}
  
  # ===============================================================
  # make a table of occurances and their counts
  FieldCounts <- data.frame(table(myData[,Field], dnn=Field))
  
  # ===============================================================
  # Write some stats
  # ===============================================================
  nrows <- nrow(myData[Field])
  nblanks <-  sum(is.na(myData[Field]))
  nvalids <- nrows - nblanks
  nuniques <- nrow(FieldCounts)
  
  myArgs.InfoDetail <- "ROWS"
  myArgs.Data1 <- nrows
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                myArgs.Notes)
  
  myArgs.InfoDetail <- "NA"
  myArgs.Data1 <- nblanks
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                myArgs.Notes)
  
  myArgs.InfoDetail <- "VALID_ROWS"
  myArgs.Data1 <- nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                myArgs.Notes)

  myArgs.InfoType <- "FEATURE"
  myArgs.InfoDetail <- "COUNT_RECORDS"
  myArgs.Data1 <- nrows
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  myArgs.InfoDetail <- "COUNT_NA"
  myArgs.Data1 <- nblanks
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                myArgs.Notes)

  myArgs.InfoDetail <- "COUNT_UNIQUES"
  myArgs.Data1 <- nuniques
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)

  myArgs.InfoDetail <- "DENSITY_UNIQUES"
  myArgs.Data1 <- nuniques / nrows
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                myArgs.Notes)
  
  myArgs.InfoDetail <- "DENSITY_NA"
  myArgs.Data1 <- nblanks / nrows
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  myArgs.InfoDetail <- "DENSITY_UNIQUES_EXNA"
  myArgs.Data1 <- nuniques / (nrows - nblanks)
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                myArgs.Notes)
  
  # ============================================
  # write stats appropriate to numeric variables
  # ============================================
   if (varclass=="numeric") {
     
     if (diagnostics==1) print("Numerical analysis")
     myArgs.InfoType <- "PARAM"
     myArgs.InfoDetail <- "MAX"
     myArgs.Data1 <- max(myData[,Field], na.rm=TRUE)
     df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                   myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                   myArgs.Notes)
     
     myArgs.InfoType <- "PARAM"
     myArgs.InfoDetail <- "MIN"
     myArgs.Data1 <- min(myData[,Field], na.rm=TRUE)
     df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                   myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                   myArgs.Notes)

     myArgs.InfoType <- "PARAM"
     myArgs.InfoDetail <- "MEAN"
     myArgs.Data1 <- mean(myData[,Field], na.rm=TRUE)
     df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                   myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                   myArgs.Notes)

     myArgs.InfoType <- "PARAM"
     myArgs.InfoDetail <- "STDDEV"
     myArgs.Data1 <- sd(myData[,Field], na.rm=TRUE)
     df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                   myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                   myArgs.Notes)

     myArgs.InfoType <- "PARAM"
     myArgs.InfoDetail <- "MEDIAN"
     myArgs.Data1 <- median(myData[,Field], na.rm=TRUE)
     df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                   myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                   myArgs.Notes)
   }  
  
   # Now, change all variable types to character
   FieldAsCharNA <- lapply(myData[,Field], as.character)
   FieldAsChar <- FieldAsCharNA[!is.na(FieldAsCharNA)]
   FieldAsChar.NoSpaces <- gsub("[[:space:]]", "", FieldAsChar) 
   
    # change factor columns of contents to character type
    FieldCounts[,1] <- sapply(FieldCounts[,1], as.character)
    
    # add a column (#3)  which is the length of the character
    # FieldCounts$Lengths <- nchar(FieldCounts[,1])
    if (nrow(FieldCounts) > 0) {
      FieldCounts$Lengths <- nchar(FieldCounts[,1])
    } else {
      FieldCounts$Lengths 
    }

    myArgs.InfoType <- "FEATURE"
    myArgs.InfoDetail <- "WIDTH_MAX"
    if (nrow(FieldCounts) > 0) {
      myArgs.Data1 <- max(FieldCounts[,3])
    } else {
      myArgs.Data1 = 0
    }
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    myArgs.InfoDetail <- "WIDTH_MIN"
    if (nrow(FieldCounts) > 0) {
      myArgs.Data1 <- min(FieldCounts[,3])
    } else {
      myArgs.Data1 = 0
    }
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    myArgs.InfoDetail <- "WIDTH_MEAN"
    if (nrow(FieldCounts) > 0) {
      myArgs.Data1 <- sum(FieldCounts[,2] * FieldCounts[,3]) / sum(FieldCounts[,2])
} else {
  myArgs.Data1 = 0
}
df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    

    # ================================================================
    # Densities
    # ================================================================
    density.lengths <- nchar(FieldAsChar)

    # =================================================
    # Regex match of anything that looks like a simple number
    
    myArgs.InfoDetail <- "REGEX_NUMBER"
    density.regex <- "^[0-9]*$"
    density.grepl <- grepl(density.regex, FieldAsChar.NoSpaces, perl=TRUE, ignore.case = TRUE)
    myArgs.Data1 <- sum(density.grepl > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)
    
    myArgs.InfoDetail <- "REGEX_DECIMALS"
    density.regex <- "^[0-9\\.]*$"
    density.grepl <- grepl(density.regex, FieldAsChar.NoSpaces, perl=TRUE, ignore.case = TRUE)
    myArgs.Data1 <- sum(density.grepl > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)
    
    # =================================================
    # Regex match of anything that looks like a percent

    myArgs.InfoDetail <- "REGEX_PERCENT"
    density.regex <- "^[0-9\\.\\+-]*[\\%$]$"
    density.grepl <- grepl(density.regex, FieldAsChar.NoSpaces, perl=TRUE, ignore.case = TRUE)
    myArgs.Data1 <- sum(density.grepl > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)
    
    # =================================================
    # Regex match of anything that looks like scientific notation
    
    myArgs.InfoDetail <- "REGEX_SCIENTIFIC"
    density.regex <- "(-)?([0-9]\\.?)(E)(\\+|-)([0-9])"
    density.grepl <- grepl(density.regex, FieldAsChar.NoSpaces, perl=TRUE, ignore.case = TRUE)
    myArgs.Data1 <- sum(density.grepl > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)


    myArgs.InfoDetail <- "REGEX_TIMES10"
    density.regex <- "(-)?([0-9]\\.?)(x10)(\\+|-)([0-9])"
    density.grepl <- grepl(density.regex, FieldAsChar.NoSpaces, perl=TRUE, ignore.case = TRUE)
    myArgs.Data1 <- sum(density.grepl > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)
    
    # ===========================================================
    # Regex match of anything that looks like currency
    myArgs.InfoDetail <- "REGEX_CURRENCY_UK"
    density.regex <- "^([£])([0-9\\.])"
    density.grepl <- grepl(density.regex, FieldAsChar.NoSpaces, perl=TRUE, ignore.case = TRUE)
    myArgs.Data1 <- sum(density.grepl > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)
    
    myArgs.InfoDetail <- "REGEX_CURRENCY_USA"
    density.regex <- "^([\\$])([0-9\\.])"
    density.grepl <- grepl(density.regex, FieldAsChar.NoSpaces, perl=TRUE, ignore.case = TRUE)
    myArgs.Data1 <- sum(density.grepl > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)
    
    # ===========================================================
    # Regex match of anything that looks like any sort of logical
    
    myArgs.InfoDetail <- "REGEX_BIT_ONEZERO"
    density.regex <- "^[1]$|^[0]$"
    density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
    myArgs.Data1 <- sum(density.grepl > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)

    myArgs.InfoDetail <- "REGEX_BIT_TRUEFALSE"
    density.regex <- "^[true]*$|^[false]*$"
    density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
    myArgs.Data1 <- sum(density.grepl > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)

    myArgs.InfoDetail <- "REGEX_BIT_YESNO"
    density.regex <- "^[yes]*$|^[no]*$"
    density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
    myArgs.Data1 <- sum(density.grepl > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)
    
    # ===========================================================
    # Regex match of anything that looks like any sort of url
    myArgs.InfoDetail <- "REGEX_WEB_URL"
    density.regex = "^(https://)|^(http://)|^(www.)"
    density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
    myArgs.Data1 <- sum(density.grepl > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)
    
    # ===========================================================
    # Regex match of anything that looks like an email
    # General Email Regex (RFC 5322 Official Standard)
    # http://emailregex.com/
    myArgs.InfoDetail <- "REGEX_WEB_EMAIL"
    data.regex = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,6}"
    density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
    myArgs.Data1 <- sum(density.grepl > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)
    
    
    # ===========================================================
    # Regex match of anything that looks like longitude or latitude
    myArgs.InfoDetail <- "REGEX_GEO_LATITUDE"
    # Thanks to : http://stackoverflow.com/questions/3518504/regular-expression-for-matching-latitude-longitude-coordinates
    density.regex <- "^(\\+|-)?(?:90(?:(?:\\.0{1,6})?)|(?:[0-9]|[1-8][0-9])(?:(?:\\.[0-9]{1,6})?))$"
    density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
    myArgs.Data1 <- sum(density.grepl > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)
    
    # Regex match of anything that looks like longitude or latitude
    myArgs.InfoDetail <- "REGEX_GEO_LONGITUDE"
    # Thanks to : http://stackoverflow.com/questions/3518504/regular-expression-for-matching-latitude-longitude-coordinates
    density.regex <- "^(\\+|-)?(?:180(?:(?:\\.0{1,6})?)|(?:[0-9]|[1-9][0-9]|1[0-7][0-9])(?:(?:\\.[0-9]{1,6})?))$"
    density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
    myArgs.Data1 <- sum(density.grepl > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)
    
    # http://stackoverflow.com/questions/164979/uk-postcode-regex-comprehensive
    myArgs.InfoDetail <- "REGEX_GEO_POSTCODE"
    data.regex = "^(([gG][iI][rR] {0,}0[aA]{2})|((([a-pr-uwyzA-PR-UWYZ][a-hk-yA-HK-Y]?[0-9][0-9]?)|(([a-pr-uwyzA-PR-UWYZ][0-9][a-hjkstuwA-HJKSTUW])|([a-pr-uwyzA-PR-UWYZ][a-hk-yA-HK-Y][0-9][abehmnprv-yABEHMNPRV-Y]))) {0,}[0-9][abd-hjlnp-uw-zABD-HJLNP-UW-Z]{2}))$"
    density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
    myArgs.Data1 <- sum(density.grepl > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)
    
    # ====================
    myArgs.InfoDetail <- "DENSITY_WHITESPACE"
    density.space <- str_count(FieldAsChar, regex("[:blank:]"))
    #myArgs.Data1 <- sum(density.space) / sum(density.lengths)
    myArgs.Data1 <- sum(density.space > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    # ====================
    myArgs.InfoDetail <- "DENSITY_ROW_DIGITS"
    density.digits <- str_count(FieldAsChar, regex("[:digit:]"))
    myArgs.Data1 <- sum(density.digits > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    myArgs.InfoDetail <- "DENSITY_DATASET_DIGITS"
    myArgs.Data1 <- sum(density.digits) / sum(density.lengths)
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    # ====================
    myArgs.InfoDetail <- "DENSITY_ROW_ALPHAS"
    density.alpha <- str_count(FieldAsChar, regex("[:alpha:]"))
    myArgs.Data1 <- sum(density.alpha > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    myArgs.InfoDetail <- "DENSITY_DATASET_ALPHAS"
    myArgs.Data1 <- sum(density.alpha) / sum(density.lengths)
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    # ====================
    myArgs.InfoDetail <- "DENSITY_ROW_PUNCTUATION"
    density.punct <- str_count(FieldAsChar, regex("^[:blank:,:alpha:,:digit:]"))
    myArgs.Data1 <- sum(density.punct > 0) / nvalids
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    myArgs.InfoDetail <- "DENSITY_DATASET_PUNCTUATION"
    myArgs.Data1 <- sum(density.punct) / sum(density.lengths)
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)

    return(df.burrow)  
}


# ==============================================================
# Write a line to the burrow.csv
# ==============================================================
myContentLine <- function(df.burrow,
                          InfoLevel="", InfoType="", InfoDetail="",
                          Variable1="", myData1="", 
                          Variable2="", myData2="", myNotes="") {
  
  library(stringr)
  # InfoLevel : HEADER/FILE/FIELD : determines what gets written
  # InfoType : details/contents etc
  # InfoDetail : columns/rows etc
  
  # Variable1 : Main variable (or field)
  # myData1 : The data for the variable
  # Variable2 : where there is a comparison this is the comparator
  # myData2 : The data for the comparator
  # myNotes : R code, notes etc
  
  VarLevel <- "UNI"
  if (Variable2 != "") VarLevel <- "BI"
  if (InfoLevel == "FILE") VarLevel <- ""
  
  newrow <- data.frame("InfoLevel"=InfoLevel, "InfoType"=InfoType, "InfoDetail"=InfoDetail, 
                       "VarLevel"=VarLevel, 
                       "Variable1"=Variable1, "myData1"=myData1, "Variable2"=Variable2, "myData2"=myData2, 
                       "myNotes"=myNotes, stringsAsFactors = FALSE)
  
  df.burrow <- rbind(df.burrow, newrow)
  return(df.burrow)
}
