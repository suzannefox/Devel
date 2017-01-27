

diagnostics <- 0 # Set this to 1 for diagnostic messages
writeheader <- 1 # Set this to 1 to write header

# This is the Input function
# myInput can be a file or a dataframe
# myOutput is the name of the file to write the burrow data to
myBurrow <- function(myInput, myOutput) {
  
  if (diagnostics==1) {print(paste("... Input variable is",class(myInput)))}
  
  # =============================================================
  # if dataframe has been input, just make a copy
  # otherwise assume it's a file and read it into a dataframe
  # =============================================================
  if (class(myInput)=="data.frame") {
    myData <- myInput
    myInput <- c("DATA.FRAME",deparse(substitute(myInput)))
  } else {
    myData <- read.csv(myInput, stringsAsFactors = FALSE)
    myInput <- c("FILE", myInput)
  }
  
  # ==========================
  # Write to the output stream
  # ==========================
  if (diagnostics==1) {print(paste("... Writing output to",FileOut))}

  if (writeheader==1) {
    write_Header(FileOut)                     # Write header
    write_Fileinfo(myInput, myData, FileOut)  # write source details
  }

  for(i in names(myData)) {                 # write Field details
    write_Fieldinfo(myData, i, FileOut)
  }
  
  # ==========================
  # return the dataframe
  # ==========================
  return(myData)
}


# ==============================================================
# Write a line to the burrow.csv
# ==============================================================
myContentLine <- function(FileOut, 
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
  
  outTemplate <- "%s,%s,%s,%s,%s,%s,%s,%s,%s"
  VarLevel <- "UNI"
  if (Variable2 != "") VarLevel <- "BI"
  if (InfoLevel == "FILE") VarLevel <- ""
  
  outString <- sprintf(outTemplate, InfoLevel, InfoType, InfoDetail,
                       VarLevel,
                       Variable1, myData1, Variable2, myData2,
                       myNotes)
  
  write.table(outString, FileOut, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)
}

# ==============================================================
# Write the header, which creates a new file
# ==============================================================
write_Header <- function(FileOut) {
  
  if (diagnostics==1) {print("... Writing header")}
  write.table("InfoLevel, InfoType, InfoDetail, VariableLevel, Variable1, Data1, Variable2, Data2, Notes", 
              FileOut, append=FALSE, row.names=FALSE,col.names=FALSE, quote=FALSE)
  
}

# ==============================================================
# Write the file level details
# ==============================================================
write_Fileinfo <- function(myInput, myData, FileOut) {
  
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
  myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                         myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                         myArgs.Notes)
  
  # number of cols
  myArgs.InfoType <- "DETAIL"
  myArgs.InfoDetail <- "COUNT_COLUMNS"
  myArgs.Variable1 <- "NUMCOLS"
  myArgs.Data1 <- ncol(myData)
  myArgs.Notes <- "ncol(myData)"
  myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                myArgs.Notes)
  

  # number of rows
  myArgs.InfoType <- "DETAIL"
  myArgs.InfoDetail <- "COUNT_ROWS"
  myArgs.Variable1 <- "NUMROWS"
  myArgs.Data1 <- nrow(myData)
  myArgs.Notes <- "nrow(myData)"
  myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                myArgs.Notes)

  # Column names
  if (ncol(myData) < 100) {
    nameTemplate = "Field%02d"
  } else {
    nameTemplate = "Field%03d"
  }

  nameClass <- sapply(myData,class)
  nameTypeof <- sapply(myData,typeof)

  icount <- 0
  for (i in names(myData)) {
    icount <- icount + 1
    Ftext = sprintf(nameTemplate,icount)
    
    myArgs.InfoDetail <- "COLUMN_NAME"
    myArgs.Variable1 <- "COLNAME"
    myArgs.Data1 <- i
    myArgs.Notes <- "names(myData)"
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)


    myArgs.InfoDetail <- "ORDINAL_POSITION"
    myArgs.Variable1 <- i
    myArgs.Data1 <- icount
    myArgs.Notes <- ""
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    myArgs.InfoDetail <- "DATA_CLASS_R"
    myArgs.Variable1 <- i
    myArgs.Data1 <- nameClass[icount]
    myArgs.Notes <- "sapply(myData,class)"
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)

    myArgs.InfoDetail <- "DATA_TYPE_R"
    myArgs.Variable1 <- i
    myArgs.Data1 <- nameTypeof[icount]
    myArgs.Notes <- "sapply(myData,typeof)"
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
  }
}

# ==============================================================
# Write the field level details
# ==============================================================
write_Fieldinfo <- function(myData, Field, FileOut) {
  
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
  myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                myArgs.Notes)
  
  myArgs.InfoDetail <- "NA"
  myArgs.Data1 <- nblanks
  myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                myArgs.Notes)
  
  myArgs.InfoDetail <- "VALID_ROWS"
  myArgs.Data1 <- nvalids
  myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                myArgs.Notes)

    myArgs.InfoDetail <- "UNIQUES"
  myArgs.Data1 <- nuniques
  myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                myArgs.Notes)
  
  myArgs.InfoType <- "DENSITY_ROW"
  myArgs.InfoDetail <- "NA"
  myArgs.Data1 <- nblanks / nrows
  myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                myArgs.Notes)

  myArgs.InfoDetail <- "UNIQUES"
  myArgs.Data1 <- nuniques / nrows
  myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                myArgs.Notes)
  
  myArgs.InfoDetail <- "UNIQUES_EXNA"
  myArgs.Data1 <- nuniques / (nrows - nblanks)
  myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
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
     myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                   myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                   myArgs.Notes)
     
     myArgs.InfoType <- "PARAM"
     myArgs.InfoDetail <- "MIN"
     myArgs.Data1 <- min(myData[,Field], na.rm=TRUE)
     myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                   myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                   myArgs.Notes)

     myArgs.InfoType <- "PARAM"
     myArgs.InfoDetail <- "MEAN"
     myArgs.Data1 <- mean(myData[,Field], na.rm=TRUE)
     myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                   myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                   myArgs.Notes)

     myArgs.InfoType <- "PARAM"
     myArgs.InfoDetail <- "STDDEV"
     myArgs.Data1 <- sd(myData[,Field], na.rm=TRUE)
     myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                   myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                   myArgs.Notes)

     myArgs.InfoType <- "PARAM"
     myArgs.InfoDetail <- "MEDIAN"
     myArgs.Data1 <- median(myData[,Field], na.rm=TRUE)
     myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                   myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                   myArgs.Notes)
   }  
  
   # Now, change all variable types to character
   FieldAsCharNA <- lapply(myData[,Field], as.character)
   FieldAsChar <- FieldAsCharNA[!is.na(FieldAsCharNA)]
   
    # change factor columns of contents to character type
    FieldCounts[,1] <- sapply(FieldCounts[,1], as.character)
    
    # add a column (#3)  which is the length of the character
    FieldCounts$Lengths <- nchar(FieldCounts[,1])

    myArgs.InfoType <- "PARAM"
    myArgs.InfoDetail <- "WIDTH_MAX"
    myArgs.Data1 <- max(FieldCounts[,3])
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    myArgs.InfoType <- "PARAM"
    myArgs.InfoDetail <- "WIDTH_MIN"
    myArgs.Data1 <- min(FieldCounts[,3])
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    myArgs.InfoType <- "PARAM"
    myArgs.InfoDetail <- "WIDTH_MEAN"
    myArgs.Data1 <- sum(FieldCounts[,2] * FieldCounts[,3]) / sum(FieldCounts[,2])
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    

    # ================================================================
    # Densities
    # ================================================================
    density.lengths <- nchar(FieldAsChar)

    # ====================
    myArgs.InfoDetail <- "@"
    myArgs.InfoType <- "DENSITY_ROW"
    density.atsign <- str_count(FieldAsChar, regex("@"))
    myArgs.Data1 <- sum(density.atsign > 0) / nvalids
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)

    myArgs.InfoType <- "DENSITY_TOT"
    myArgs.Data1 <- sum(density.atsign) / sum(density.lengths)
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    # ====================
    myArgs.InfoDetail <- "DOT"
    myArgs.InfoType <- "DENSITY_ROW"
    density.dot <- str_count(FieldAsChar, regex("\\."))
    #myArgs.Data1 <- sum(grepl("\\.", FieldAsChar)) / nvalids
    myArgs.Data1 <- sum(density.dot > 0) / nvalids
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    myArgs.InfoType <- "DENSITY_TOT"
    myArgs.Data1 <- sum(density.dot) / sum(density.lengths)
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    # ====================
    myArgs.InfoDetail <- "COMMA"
    myArgs.InfoType <- "DENSITY_ROW"
    density.comma <- str_count(FieldAsChar, regex(","))
    #myArgs.Data1 <- sum(grepl(",", FieldAsChar)) / nvalids
    myArgs.Data1 <- sum(density.comma > 0) / nvalids
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    myArgs.InfoType <- "DENSITY_TOT"
    myArgs.Data1 <- sum(density.comma) / sum(density.lengths)
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    # ====================
    myArgs.InfoDetail <- "WHITESPACE"
    myArgs.InfoType <- "DENSITY_ROW"
    density.space <- str_count(FieldAsChar, regex("[:blank:]"))
    #myArgs.Data1 <- sum(density.space) / sum(density.lengths)
    myArgs.Data1 <- sum(density.space > 0) / nvalids
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    myArgs.InfoType <- "DENSITY_TOT"
    myArgs.Data1 <- sum(density.space) / sum(density.lengths)
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    # ====================
    myArgs.InfoDetail <- "URL"
    myArgs.InfoType <- "DENSITY_ROW"
    density.https <- grepl("https:", FieldAsChar)
    density.http <- grepl("http:", FieldAsChar)
    density.www <- grepl("www.", FieldAsChar)
    density.url <- density.https | density.http | density.www
    myArgs.Data1 <- sum(density.url > 0) / nvalids
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    # ====================
    myArgs.InfoDetail <- "DIGITS"
    myArgs.InfoType <- "DENSITY_ROW"
    density.digits <- str_count(FieldAsChar, regex("[:digit:]"))
    myArgs.Data1 <- sum(density.digits > 0) / nvalids
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    myArgs.InfoType <- "DENSITY_TOT"
    myArgs.Data1 <- sum(density.digits) / sum(density.lengths)
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    # ====================
    myArgs.InfoDetail <- "ALPHAS"
    myArgs.InfoType <- "DENSITY_ROW"
    density.alpha <- str_count(FieldAsChar, regex("[:alpha:]"))
    myArgs.Data1 <- sum(density.alpha > 0) / nvalids
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    myArgs.InfoType <- "DENSITY_TOT"
    myArgs.Data1 <- sum(density.alpha) / sum(density.lengths)
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    # ====================
    myArgs.InfoDetail <- "PUNCTUATION"
    myArgs.InfoType <- "DENSITY_ROW"
    density.punct <- str_count(FieldAsChar, regex("^[:blank:,:alpha:,:digit:]"))
    myArgs.Data1 <- sum(density.punct > 0) / nvalids
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
    myArgs.InfoType <- "DENSITY_TOT"
    myArgs.Data1 <- sum(density.punct) / sum(density.lengths)
    myContentLine(FileOut, myArgs.InfoLevel, myArgs.InfoType, myArgs.InfoDetail, 
                  myArgs.Variable1, myArgs.Data1, myArgs.Variable2, myArgs.Data2,
                  myArgs.Notes)
    
}

