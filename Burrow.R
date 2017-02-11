
# ==============================================
# create an S3 class
# ==============================================
Burrow <- function(sourcedata, sourcedescription, diagnostics=FALSE) {

  # =================================================================
  # INPUT VALIDITY CHECKING
  # =================================================================
  # check sourcedata is a dataframe
  if(class(sourcedata) != "data.frame")  {
    stop("First argument must be the source data supplied as a dataframe")
  }
  
  # check sourcedescription is a character
  if(class(sourcedescription) != "character")  {
    stop("Second argument must be a character variable describing the data")
  }

  # check diagnostics is logical
  if(class(diagnostics) != "logical")  {
    stop("Third argument must be a logical variable")
  }
  
  # =================================================================
  # MAKE SURE THERE ARE NO FACTOR VARIABLES
  # =================================================================
  sourcedataname <- deparse(substitute(sourcedata))
  cleandata <- data.frame(lapply(sourcedata, function(x) if(is.factor(x)) as.character(x) else x), stringsAsFactors = FALSE)
  sourcedata <- cleandata
  
  # =================================================================
  # REPORT THE RUN START
  # =================================================================
  if (diagnostics==TRUE) {
    print(paste("BURROW","... Run started :",format(Sys.time(), "%a %b %d %Y, %X")))
  }
  
  # =============================================================
  # INITIALISE BLANK DATAFRAME FOR "LONG" FORMAT DATA
  # =============================================================
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
  # Start time
  tstart <- proc.time()
  
  # write some header details
  myArgs.InfoLevel <- "HEADER"
  myArgs.InfoType <- "RUN DATE"
  myArgs.InfoDetail <- format(Sys.time(), "%a %b %d %Y, %X")
  myArgs.Variable1 <- ""
  myArgs.Data1 <- ""
  myArgs.Variable2 <- ""
  myArgs.Data2 <- ""
  myArgs.Notes <- ""
  
  df.burrow = myContentLine(df.burrow,
                            myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)

  myArgs.InfoLevel <- "DATASET"
  myArgs.InfoType <- "SOURCEDATA"
  myArgs.InfoDetail <- sourcedataname

  df.burrow = myContentLine(df.burrow,
                            myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail,
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)

  # =================================================================
  # COLLECT DATASET DETAILS
  # =================================================================
  
  myData <- sourcedata
  
  if (diagnostics==TRUE) {
    print(paste("BURROW","... Analysing dataset"))
  }
  df.burrow <- write_Datasetinfo(myData, sourcedescription, df.burrow, diagnostics) 
  
  # =================================================================
  # CALCULATE CORRELATIONS FOR NUMERIC FIELDS
  # =================================================================
  if (diagnostics==TRUE) {
    print(paste("BURROW","... Calculating correlations"))
  }
  df.correlations <- calculate_Correlations(myData, diagnostics)

  # =================================================================
  # COLLECT DETAILS FOR EACH FIELD AND GUESS THE DATATYPE
  # =================================================================
  for(i in names(myData)) {
    if (diagnostics==TRUE) {
      print(paste("BURROW","... Analysing field :",i))
    }

    # Field features
    df.burrow <- write_Fieldinfo(myData, i, df.burrow)
    # Best guess at datatype
    df.burrow <- write_BestGuess(myData, i, df.burrow)
    
    # Previous calculations
    i.gotcorrelation <- nrow(subset(df.correlations, Var1==i))
    if (i.gotcorrelation > 0) {
      if (diagnostics==TRUE) {
        print(paste("BURROW","... Adding Correlation :",i))
      }
      df.burrow <- write_Calculations(df.correlations,i,df.burrow,"CORRELATION")
    }

    if (diagnostics==TRUE) {
      bg <- subset(df.burrow, InfoType=="BEST GUESS" & Variable1==i)
      print(paste("BURROW","... Best Guess is :",bg[,c("myData1")]))
    }
  }
  
  # =================================================================
  # ASSEMBLE THE LIST OF OBJECTS TO RETURN TO THE CALLER
  # =================================================================
  
  # collect time taken
  tused <- proc.time() - tstart
  if (diagnostics==TRUE) {
    print(paste("BURROW","... Finished. Processing time :",tused[1]))
  }
  
  value <- list(sourcedata = sourcedata, 
                sourcedescription = sourcedescription, 
                data = df.burrow,
                runStatsProcTime = tused[1])

  attr(value, "class") <- "Burrow"
  value
}

# ==============================================================
# INTERNAL SUPPORTING FUNCTIONS
# ==============================================================
# -------------------------------------------------------------
# determine features of the dataset
# -------------------------------------------------------------
write_Datasetinfo <- function(myData, myDescription, df.burrow, diagnostics=FALSE) {
  
  if (diagnostics==TRUE) {
    print("... write_Datasetinfo ... Writing dataset details")
  }
  
  # write the data source details
  myArgs.InfoLevel <- "DATASET"
  myArgs.InfoType <- "SOURCE"
  myArgs.InfoDetail <- myDescription
  myArgs.Variable1 <- ""
  myArgs.Data1 <- ""
  myArgs.Variable2 <- ""
  myArgs.Data2 <- ""
  myArgs.Notes <- ""
  
  df.burrow = myContentLine(df.burrow,
                            myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  # number of cols
  myArgs.InfoType <- "FEATURE"
  myArgs.InfoDetail <- "COUNT_COLUMNS"
  myArgs.Variable1 <- "NUMCOLS"
  myArgs.Data1 <- ncol(myData)
  myArgs.Notes <- "ncol(myData)"
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  
  # number of rows
  myArgs.InfoDetail <- "COUNT_ROWS"
  myArgs.Variable1 <- "NUMROWS"
  myArgs.Data1 <- nrow(myData)
  myArgs.Notes <- "nrow(myData)"
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)

  # dataset-level details for each field
  icount <- 0
  for (i in names(myData)) {
    icount <- icount + 1
    
    myArgs.InfoType <- "DETAIL"
    myArgs.InfoDetail <- "ORDINAL_POSITION"
    myArgs.Variable1 <- i
    myArgs.Data1 <- icount
    myArgs.Notes <- ""
    df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                              myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                              myArgs.Notes)
    
    myArgs.InfoType <- "FEATURE"
    myArgs.InfoDetail <- "COLUMN_NAME"
    myArgs.Variable1 <- "COLNAME"
    myArgs.Data1 <- i
    myArgs.Notes <- "names(myData)"
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
  
  if (diagnostics==TRUE) {
    print("... write_Datasetinfo ... Finished.")
  }
  
  return(df.burrow)
}

# -------------------------------------------------------------
# determine features of a field in the dataset
# -------------------------------------------------------------
write_Fieldinfo <- function(myData, Field, df.burrow, diagnostics=FALSE) {
  
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

# -------------------------------------------------------------
# determine the best guess of the data type
# -------------------------------------------------------------
write_BestGuess <- function(myData, Field, df.burrow, diagnostics=FALSE) {
  
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
  
  test.variable <- Field
  test.bestguess <- "DK"
  test.guesssource <- "DK"
  test.features <- subset(df.burrow, Variable1 == test.variable & myData1 > 0)
  
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
  
  # write the Best Guess details
  myArgs.InfoLevel <- "FIELD"
  myArgs.InfoType <- "BEST GUESS"
  myArgs.InfoDetail <- test.guesssource
  myArgs.Variable1 <- test.variable
  myArgs.Data1 <- test.bestguess
  myArgs.Variable2 <- ""
  myArgs.Data2 <- ""
  myArgs.Notes <- "Guess by Burrow function, revision 0.0.9000"
  
  
  df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                            myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                            myArgs.Notes)
  
  return(df.burrow)  
  
}

# -------------------------------------------------------------
# determine correlations for numeric fields
# -------------------------------------------------------------
calculate_Correlations <- function(myData, diagnostics=FALSE) {

  # identify all the variables suitable for numeric stats
  myData.mean <- as.data.frame(suppressWarnings(sapply(myData, mean, na.rm=TRUE)))
  names(myData.mean) <- c("MeanVal")
  # non numeric variables give NA for means so remove them
  myData.mean <- subset(myData.mean, !is.na(MeanVal))
  
  if (diagnostics==TRUE) {
    print(paste("... calculate_Correlations ... number of variables",nrow(myData.mean)))
  }
  
  # make empty dataframe
  myData.correlations <- data.frame(Var1=character(),
                                    Var2=character(),
                                    cor=double())
  
  # we need at least 2 variables to get correlations
  if (nrow(myData.mean) > 1) {
    colnames(myData.mean) <- "Value"
    
    # get the numerical variables
    myData.numericols <- row.names(subset(myData.mean))
    if (diagnostics==TRUE) {
      print(paste("... calculate_Correlations ... numeric cols are",myData.numericols))
    }
    
    # calculate correlations
    try(myData.cor <- as.data.frame(cor(myData[,c(myData.numericols)], use="complete.obs")))
    
    if (exists("myData.cor")) {
      for (i in names(myData.cor)) {
        temp.Var1 <- myData.numericols
        temp.Var2 <- rep_len(i, length(temp.Var1))
        temp.Var3 <- myData.cor[,c(i)]
        temp.df = data.frame("Var1" = temp.Var1, "Var2" = temp.Var2, "Value" = temp.Var3)
        myData.correlations <- rbind2(myData.correlations, temp.df)
      }
    }
  }
  
  if (diagnostics==TRUE) {
    print(paste("... calculate_Correlations ... Finished"))
  }
  return(myData.correlations)
}  

# -------------------------------------------------------------
# Add pre calulated values to the burrow data
# -------------------------------------------------------------
write_Calculations <- function(df.correlations, Field, df.burrow, calcType) {

  if (calcType=="CORRELATION") {

    # Corelations for this field
    my.corr <- subset(df.correlations, Var1==Field)
    
    for(i in 1:nrow(my.corr)){
      # write the Field details
      myArgs.InfoLevel <- "FIELD"
      myArgs.InfoType <- "ANALYSIS"
      myArgs.InfoDetail <- "CORRELATION"
      myArgs.Variable1 <- Field
      myArgs.Data1 <- my.corr[i,"Value"]
      myArgs.Variable2 <- my.corr[i,"Var2"]
      myArgs.Data2 <- ""
      myArgs.Notes <- "NAs excluded, Pearsons calculation"
      
      df.burrow = myContentLine(df.burrow, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                                myArgs.Notes)
    }
  }
  
  return (df.burrow)
}

# -------------------------------------------------------------
# write a record to the long format burrow dataframe
# -------------------------------------------------------------
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
  
  VarLevel <- "UNIVARIATE"
  if (Variable2 != "") VarLevel <- "BIVARIATE"
  if (InfoLevel == "FILE") VarLevel <- ""
  
  # remove very small numbers
  newrow <- data.frame("InfoLevel"=InfoLevel, "InfoType"=InfoType, "InfoDetail"=InfoDetail, 
                       "VarLevel"=VarLevel, 
                       "Variable1"=Variable1, "myData1"=myData1, "Variable2"=Variable2, "myData2"=myData2, 
                       "myNotes"=myNotes, stringsAsFactors = FALSE)
  
  df.burrow <- rbind(df.burrow, newrow)
  return(df.burrow)
}

# ======================================================================
# draw a QQ plot of a numeric distribution to show normality
# show outliers (outside 1.5IQR) calculated using boxplot.stats in red
#
# overlay with -
#      solid green lines showing q1, median, q3 levels as for boxplots
#      dashed blue line for mean 
#      median line is wider than q1/q3, and opaque so that mean is
#      not obscured if mean and median overlap
# ======================================================================
plot_nums <- function(data.orig, data.title) {
  
  names(data.orig) <- c(data.title)
  data.plot <- as.data.frame(stack(data.orig))
  names(data.plot) <- c("Values","Variable")
  
  # get stats
  data.mean <- mean(data.plot$Values, na.rm = TRUE)
  data.median <- median(data.plot$Values, na.rm = TRUE)
  
  # identify outliers
  data.outliers <- boxplot.stats(data.plot$Values)
  data.outlierindex <- data.plot$Values %in% data.outliers$out
  data.plot$outliers <- "NOT"
  data.plot$outliers[data.outlierindex] <- "IS"
  data.colours.outliers <- c(IS="firebrick3", NOT="darkgrey")
  data.colours.mean <- "royalblue3"
  data.colours.median <- "olivedrab"
  
  plot_num <- ggplot(data.plot)
  plot_num <- plot_num + scale_colour_manual(values = data.colours.outliers)   # outliers colours
  plot_num <- plot_num + stat_qq(aes(sample=Values, colour = factor(outliers)))  
  plot_num <- plot_num + geom_hline(yintercept = data.median, color=data.colours.median, size=2, alpha=0.4)
  plot_num <- plot_num + geom_hline(yintercept = data.mean, color=data.colours.mean, linetype = "dotdash", size=1.25)
  plot_num <- plot_num + geom_hline(yintercept = data.outliers$stats[2], color=data.colours.median)
  plot_num <- plot_num + geom_hline(yintercept = data.outliers$stats[4], color=data.colours.median)
  plot_num <- plot_num + theme(legend.position="none")
  plot_num <- plot_num + ggtitle(paste("QQ plot showing normality, outliers, mean and median for",data.title))
  
  return(plot_num)
}
