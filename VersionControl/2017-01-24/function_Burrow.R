

# This is the Input function
# myInput can be a file, dataframe, table, timeseries
BurrowData <- function(myInput, myInputName="") {
  
  diagnostics <- 0
  # =============================================================
  # Step 1. Initialise a blank dataframe to hold the output data
  # =============================================================
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
  
  # =============================================================
  # Step 2. Determine what sort of input has been passed
  #         Whatever the input, this step creates a dataframe
  #
  #         if dataframe has been input, just make a copy
  #         if a table, convert to dataframe first
  #         otherwise assume a file and read into a dataframe
  #
  #         NOTE : this step can be extended to accomodate other
  #         types of input as long as they can be converted
  #         to a dataframe
  # =============================================================
  if (diagnostics==1) {print(paste("... Input variable has class",class(myInput)))}
  
  if (class(myInput)=="data.frame") {
    if (diagnostics==1) print("df")
    myData <- myInput
    if (myInputName=="") {
      myInput <- c("DATA.FRAME",deparse(substitute(myInput)))
    } else {
      myInput <- c("NAMED DF",myInputName)
    }
    
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
  
  # =============================================================
  # Step 3. Now that we have the input in a standard dataframe
  #         We can analyse it to create the output data
  # =============================================================
  if (diagnostics==1) print("... Step 3. writing field information")
  
  # Start time
  tstart <- proc.time()
  
  # call write_Fileinfo : write Source details
  df.burrow <- write_Fileinfo(myInput, myData, df.burrow)  
  
  # call write_Fieldinfo : write Field details  
  for(i in names(myData)) {                                
    df.burrow <- write_Fieldinfo(myData, i, df.burrow)
  }
  
  # =============================================================
  # Step 4. Write some run stats
  # =============================================================
  if (diagnostics==1) print("... Step 4. run stats")
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
  
  # =============================================================
  # Step 5. Return the output dataframe
  # =============================================================
  if (diagnostics==1) print("... Step 5. return results")
  rownames(df.burrow) <- NULL
  return(df.burrow)
}

# ==============================================================
# INTERNAL SUPPORTING FUNCTIONS FOR STEP 3
# Write the file level details
# ==============================================================
write_Fileinfo <- function(myInput, myData, df.burrow, diagnostics=0) {
  
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
  
  # ==========================================================================
  # File-level details for each field
  # ==========================================================================
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
# INTERNAL SUPPORTING FUNCTIONS FOR STEP 3
# Write the field level details
# ==============================================================
write_Fieldinfo <- function(myData, Field, df.burrow, diagnostics=0) {
  
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
  
  myArgs.InfoDetail <- "DENSITY_NA"
  myArgs.Data1 <- nblanks / nrows
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
  
  myArgs.InfoDetail <- "DENSITY_UNIQUES_EXNA"
  myArgs.Data1 <- nuniques / (nrows - nblanks)
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # ============================================
  # write stats appropriate to numeric variables
  # ============================================
  myArgs.InfoType <- "FEATURE"
  myArgs.InfoDetail <- "MAX"
  myArgs.Data1 <- 0
  
  if (varclass == "numeric" |
      varclass == "double" | varclass == "integer") {
    myArgs.Data1 <- max(myData[,Field], na.rm = TRUE)
  }
  df.burrow = myContentLine(
    df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail,
    myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
    myArgs.Notes
  )
  
  myArgs.InfoDetail <- "MIN"
  myArgs.Data1 <- 0
  if (varclass == "numeric" |
      varclass == "double" | varclass == "integer") {
    myArgs.Data1 <- min(myData[,Field], na.rm = TRUE)
  }
  df.burrow = myContentLine(
    df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail,
    myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
    myArgs.Notes
  )
  
  myArgs.InfoDetail <- "MEAN"
  myArgs.Data1 <- 0
  if (varclass == "numeric" |
      varclass == "double" | varclass == "integer") {
    myArgs.Data1 <- mean(myData[,Field], na.rm = TRUE)
  }
  df.burrow = myContentLine(
    df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail,
    myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
    myArgs.Notes
  )
  
  myArgs.InfoDetail <- "STDDEV"
  myArgs.Data1 <- 0
  if (varclass == "numeric" |
      varclass == "double" | varclass == "integer") {
    myArgs.Data1 <- sd(myData[,Field], na.rm = TRUE)
  }
  df.burrow = myContentLine(
    df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail,
    myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
    myArgs.Notes
  )
  
  myArgs.InfoDetail <- "MEDIAN"
  myArgs.Data1 <- 0
  if (varclass 
      == "numeric" |
      varclass == "double" | varclass == "integer") {
    myArgs.Data1 <- median(myData[,Field], na.rm = TRUE)
  }
  df.burrow = myContentLine(
    df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail,
    myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
    myArgs.Notes
  )
  
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
  
  # Regex : anything that looks like an integer (whole number)
  myArgs.InfoDetail <- "REGEX_NUMBER"
  density.regex <- "^(-?|\\+?)([0-9]+$)"
  density.grepl <- grepl(density.regex, FieldAsChar.NoSpaces, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : anything that look like a number with decimal places (float)
  myArgs.InfoDetail <- "REGEX_DECIMALS"
  density.regex <- "^(-?|\\+?)([0-9]+)([\\.]{1})([0-9]+)"
  density.grepl <- grepl(density.regex, FieldAsChar.NoSpaces, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : anything that looks like a percent
  myArgs.InfoDetail <- "REGEX_PERCENT"
  density.regex <- "^(-?|\\+?)([0-9]+)(\\.)?([0-9])?([\\%])$"
  density.grepl <- grepl(density.regex, FieldAsChar.NoSpaces, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : anything that looks like scientific notation
  myArgs.InfoDetail <- "REGEX_SCIENTIFIC"
  density.regex <- "^(-?|\\+?)([0-9]+)(\\.[0-9]+)?(E)(-|\\+)([0-9]+)"
  density.grepl <- grepl(density.regex, FieldAsChar.NoSpaces, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  
  # Regex : anything that looks like it has 10x ...
  myArgs.InfoDetail <- "REGEX_TIMES10"
  density.regex <- "^(-?|\\+?)([0-9]+)(\\.[0-9]+)?(x10)(-|\\+)?([0-9]+)"
  density.grepl <- grepl(density.regex, FieldAsChar.NoSpaces, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : anything that looks like UK currency
  myArgs.InfoDetail <- "REGEX_CURRENCY_UK"
  density.regex <- "^([£])([0-9+)(\\.[0-9]+)?"
  density.grepl <- grepl(density.regex, FieldAsChar.NoSpaces, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : anything that looks like USA currency
  myArgs.InfoDetail <- "REGEX_CURRENCY_USA"
  density.regex <- "^([\\$])([0-9+)(\\.[0-9]+)?"
  density.grepl <- grepl(density.regex, FieldAsChar.NoSpaces, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : logical 1/0    
  myArgs.InfoDetail <- "REGEX_BIT_ONEZERO"
  density.regex <- "^[1]$|^[0]$"
  density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : logical true/false (case insensitive)
  myArgs.InfoDetail <- "REGEX_BIT_TRUEFALSE"
  density.regex <- "^[true]*$|^[false]*$"
  density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : logical t/f (case insensitive)
  myArgs.InfoDetail <- "REGEX_BIT_TF"
  density.regex <- "^[t]*$|^[f]*$"
  density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : logical y/n (case insensitive)
  myArgs.InfoDetail <- "REGEX_BIT_YN"
  density.regex <- "^[y]*$|^[n]*$"
  density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : logical yes/no (case insensitive)
  myArgs.InfoDetail <- "REGEX_BIT_YESNO"
  density.regex <- "^[yes]*$|^[no]*$"
  density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : anything in date dd-mm-yyyy format
  # data.date <- c("1-1-2016","04-09-2013","30-12-2014")
  myArgs.InfoDetail <- "REGEX_DATE_DDMMYYYY_DASHES"
  
  regex.day = "^(0?[1-9]|1[0-9]|2[0-9]|3[0-1])" # 1-31, with optional leading zero
  regex.month = "(0?[1-9]|1[1-2])" # 1-12, with optional leading zero
  regex.year = "([1-2][7890][0-9][0-9])" # matches 1000 to 2999
  regex.date = paste(regex.day,"(-)",regex.month,"(-)",regex.year,sep="")
  density.regex <- regex.date
  
  density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : anything in date dd/mm/yyyy format
  # data.date <- c("1/1/2016","04/09/2013","30/12/2014")
  myArgs.InfoDetail <- "REGEX_DATE_DDMMYYYY_SLASHES"
  
  regex.day = "^(0?[1-9]|1[0-9]|2[0-9]|3[0-1])" # 1-31, with optional leading zero
  regex.month = "(0?[1-9]|1[1-2])" # 1-12, with optional leading zero
  regex.year = "([1-2][7890][0-9][0-9])" # matches 1000 to 2999
  regex.date = paste(regex.day,"(/)",regex.month,"(/)",regex.year,sep="")
  density.regex <- regex.date
  
  density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : anything Likert scale type question +2+10-1-20
  # data.scale <- c("Very likely","Quite likely","Neither way","Quite unlikely","Very unlikely","DK","Don't know","N/A","NA","Not answered")
  density.regex <- "^(very.)|(quite.)|(neither.)|(neutral.)|(DK)|(don't .)|(na)|(Not answered)"
  myArgs.InfoDetail <- "REGEX_SCALE_210120"
  density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : anything that looks like any sort of url
  myArgs.InfoDetail <- "REGEX_WEB_URL"
  density.regex = "^(https://)|^(http://)|^(www.)"
  density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : anything that looks like an email
  # General Email Regex (RFC 5322 Official Standard)
  # http://emailregex.com/
  myArgs.InfoDetail <- "REGEX_WEB_EMAIL"
  data.regex = "[A-Z0-9a-z._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,6}"
  density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  
  # Regex : anything that looks like longitude or latitude
  myArgs.InfoDetail <- "REGEX_GEO_LATITUDE"
  # Thanks to : http://stackoverflow.com/questions/3518504/regular-expression-for-matching-latitude-longitude-coordinates
  density.regex <- "^(\\+|-)?(?:90(?:(?:\\.0{1,6})?)|(?:[0-9]|[1-8][0-9])(?:(?:\\.[0-9]{1,6})?))$"
  density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : anything that looks like longitude or latitude
  myArgs.InfoDetail <- "REGEX_GEO_LONGITUDE"
  # Thanks to : http://stackoverflow.com/questions/3518504/regular-expression-for-matching-latitude-longitude-coordinates
  density.regex <- "^(\\+|-)?(?:180(?:(?:\\.0{1,6})?)|(?:[0-9]|[1-9][0-9]|1[0-7][0-9])(?:(?:\\.[0-9]{1,6})?))$"
  density.grepl <- grepl(density.regex, FieldAsChar, perl=TRUE, ignore.case = TRUE)
  myArgs.Data1 <- sum(density.grepl > 0) / nvalids
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # Regex : anything that looks like postcode
  # http://regexlib.com/REDetails.aspx?regexp_id=260&AspxAutoDetectCookieSupport=1
  myArgs.InfoDetail <- "REGEX_GEO_POSTCODE"
  density.regex = "^([A-PR-UWYZ0-9][A-HK-Y0-9][AEHMNPRTVXY0-9]?[ABEHMNPRVWXY0-9]? {1,2}[0-9][ABD-HJLN-UW-Z]{2}|GIR 0AA)$"
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
# INTERNAL SUPPORTING FUNCTIONS FOR STEP 3
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


# ==============================================================================
# 27 November 2016
# INPUT : A dataframe in the format created by the BurrowData function
#
# OUTPUT : A dataframe with the following structure -
#          - a row per "InfoDetail==COLUMN_NAME" in the BurrowData dataframe 
#            (ie variable in the source data). 
#          - a column per "InfoType==FEATURE" in the BurrowData dataframe
#            if more features are added to the myBurrow they will be picked
#            up automatically so there is a variable number of Columns
#          - a column with the "BEST_GUESS" at the data type of each variable
#            and a "GUESS_SOURCE" column with reason why this guess was the best
# ==============================================================================
BurrowDataFeatures <- function(myBurrowData) {
  
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
BurrowBestGuess <- function(myData, myDataName="") {
  if (myDataName=="") {
    myDataName <- as.character(c(deparse(substitute(myData))))
  }
  guess.burrow <- BurrowData(myData, myDataName)
  guess.features <- BurrowDataFeatures(guess.burrow)
  #names.3 <- c("FILE_NAME","COLUMN_NAME","BEST_GUESS","GUESS_SOURCE")
  #return(subset(guess.features,,names.3))
  guess.features <- guess.features[c("FILE_NAME","COLUMN_NAME","BEST_GUESS","GUESS_SOURCE")]
  return (guess.features)
  
}  

# ==============================================================================
# 29 November 2016
# INPUT : A source dataset
#
# OUTPUT : A dataframe in the format for generating a word markdown report
# ==============================================================================
BurrowReport <- function(myData) {
  myBurrow.Data <- BurrowData(myData)
  
  # get unique value density
  report.na <- subset(myBurrow.Data, InfoDetail=="DENSITY_NA",c("Variable1","myData1"))
  rownames(report.na) <- NULL
  colnames(report.na) <- c("VariableName","Missing")
  report.na$Missing <- as.numeric(report.na$Missing) * 100
  
  # get missing density
  report.unique <- subset(myBurrow.Data, InfoDetail=="DENSITY_UNIQUES",c("myData1"))
  rownames(report.unique) <- NULL
  colnames(report.unique) <- c("Unique")
  report.unique$Unique <- as.numeric(report.unique$Unique) * 100
  
  # get bestguess
  report.bestguess <- BurrowBestGuess(myData)

  # stitch it all together 
  report.detail <- cbind2(report.na,report.unique)
  report.detail <- cbind2(report.detail,report.bestguess[c("BEST_GUESS")])

  return(report.detail)
}

# ====================================
# Histogram with 20 bins, showing mean
# ====================================
myPlot_Int <- function(myData, Field, diagnostics=0) {
  
  #rnorm <- data.frame(x = c(rnorm(length(myData[,Field]))))
  plotData <- myData[!is.na(myData[c(Field)]),]
  if (diagnostics==1) print(paste("1. Plotting ",names(plotData)))
  
  myBinWidth = (max(plotData[,1]) - min(plotData[,1])) / 20
  if (diagnostics==1) print(paste("binwidth is ",myBinWidth))
  
  maxy <- max(hist(plotData[,1], breaks=seq(min(plotData[,1]), max(plotData[,1]), by=myBinWidth), plot=FALSE)$counts)
  if (diagnostics==1) print(paste("maxy is ",maxy))
  
  myMean = mean(plotData[,1])
  myMedian = median(plotData[,1])
  myQuantile = quantile(plotData[,1])
  
  ColorMean <- "blue"
  
  myPlot <- ggplot(plotData, 
                   aes_string(x=Field)) + 
    geom_histogram(binwidth=myBinWidth, colour="black", fill="#EBEDFA") +
    
    geom_vline(aes_string(xintercept=myMean), color=ColorMean, linetype="solid", size=1) +
    annotate("text", x = myMean, y = maxy * 0.9, label = "Mean", color=ColorMean) +
    
    geom_vline(aes_string(xintercept=myQuantile[2]), color="sienna1", linetype="solid", size=.5) +
    annotate("text", x = myQuantile[2], y = maxy * 0.7, label = "Q1", color="sienna1") +
    
    geom_vline(aes_string(xintercept=myQuantile[3]), color="sienna3", linetype="solid", size=1) +
    annotate("text", x = myQuantile[3], y = maxy, label = "Median", color="sienna3") +
    
    geom_vline(aes_string(xintercept=myQuantile[4]), color="sienna1", linetype="solid", size=.5) +
    annotate("text", x = myQuantile[4], y = maxy * 0.7, label = "Q3", color="sienna1") +
    
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none")
  
  return(myPlot)
  
}
testPlot_Int <- function(myData, Field) {
  #rnorm <- data.frame(x = c(rnorm(length(myData[,Field]))))
  
  plotData <- subset(myData,,select=c((Field)))
  myBinWidth = (max(myData[,Field]) - min(myData[,Field])) / 20
  maxy <- max(hist(myData[,Field], breaks=seq(min(myData[,Field]), max(myData[,Field]), by=myBinWidth), plot=FALSE)$counts)
  
  myMean = mean(myData[,Field])
  myMedian = median(myData[,Field])
  myQuantile = quantile(myData[,Field])
  
  ColorMean <- "blue"
  
  myPlot <- ggplot(myData, 
                   aes_string(x=Field)) + 
    geom_histogram(binwidth=myBinWidth, colour="black", fill="#EBEDFA") +
    
    geom_vline(aes_string(xintercept=myMean), color=ColorMean, linetype="solid", size=1) +
    annotate("text", x = myMean, y = maxy * 0.9, label = "Mean", color=ColorMean) +
    
    geom_vline(aes_string(xintercept=myQuantile[2]), color="sienna1", linetype="solid", size=.5) +
    annotate("text", x = myQuantile[2], y = maxy * 0.7, label = "Q1", color="sienna1") +
    
    geom_vline(aes_string(xintercept=myQuantile[3]), color="sienna3", linetype="solid", size=1) +
    annotate("text", x = myQuantile[3], y = maxy, label = "Median", color="sienna3") +
    
    geom_vline(aes_string(xintercept=myQuantile[4]), color="sienna1", linetype="solid", size=.5) +
    annotate("text", x = myQuantile[4], y = maxy * 0.7, label = "Q3", color="sienna1") +
    
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none")
  
  return(myPlot)
  
}
