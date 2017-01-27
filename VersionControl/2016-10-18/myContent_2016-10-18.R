
# Set this to 1 for diagnostic messages
diagnostics <- 1

# ==============================================================
# Write a line to the burrow.csv
# ==============================================================
myContentLine <- function(FileName, InfoLevel="",myNotes="",InfoType="",InfoDetail="",
                          Variable1="",myData1="", 
                          Variable2="", myData2="") {
  
  library(stringr)
  # InfoLevel : HEADER/FILE/FIELD : determines what gets written
  # myNotes : R code, notes etc
  # InfoType : details/contents etc
  # InfoDetail : columns/rows etc
  # Variable1 : Main variable (or field)
  # myData1 : The data for the variable
  # Variable2 : where there is a comparison this is the comparator
  # myData2 : The data for the comparator
  
  InfoLevel <- toupper(InfoLevel)
  outTemplate <- "%s,%s,%s,%s,%s,%s,%s,%s,%s"
  
  if (InfoLevel[1]=="HEADER") {
    if (diagnostics==1) {print("Writing header text")}
    
    write.table("InfoLevel, InfoType, InfoDetail, VariableLevel, Variable1, Data1, Variable2, Data2, Notes", 
                FileName, append=FALSE, row.names=FALSE,col.names=FALSE, quote=FALSE)
    
  } else if (InfoLevel[1]=="FILE") {
    if (diagnostics==1) {print("Writing FILE details")}

        outString <- sprintf(outTemplate,InfoLevel[1],InfoType,InfoDetail,"",Variable1,myData1,Variable2,myData2,myNotes)
    write.table(outString, FileName, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)
    
  } else {
    if (diagnostics==1) {print("Writing variable details")}

    VarLevel <- "Univariate"
    if (Variable2 != "") VarLevel <- "Bivariate"
    outString <- sprintf(outTemplate,"FIELD",InfoType,InfoDetail,VarLevel,Variable1,myData1,Variable2,myData2,myNotes)
    write.table(outString, FileName, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)
  }
}


# ==============================================================
# Analyse the content 
#
# FileName is always the output stream
#
# If myData is the string "HEADER" then Field will be the
# filename, in which case analyse and return a datatable
# of the file
#
# otherwise, myData is the datatable, field is the Field
# within it to analyse
# ==============================================================
myContent <- function(myData, Field, FileName) {
  
  # ================================================================================================
  # See if this is the header command for an input CSV file
  # ================================================================================================
  if (class(myData) == "character" && toupper(myData)=="HEADER" && class(Field) == "character") {
    
    # write column headers to the output stream
    myContentLine(FileName,"HEADER")
    
    # write the File name
    myContentLine(FileName,"FILE","File details","DETAIL","FILENAME",Field, Field)

    # Read data into a dataframe
    myData <- read.csv(Field)
    
    # write number of columns
    myContentLine(FileName,"FILE","ncol(myData)","DETAIL","COUNT_COLUMNS",Field,ncol(myData))
    # write number of rows
    myContentLine(FileName,"FILE","nrow(myData)","DETAIL","COUNT_ROWS",Field,nrow(myData))

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
      myContentLine(FileName,"FILE","","DETAIL","COLUMN_NAME",i)
      myContentLine(FileName,"FILE","","COLUMNS","ORDINAL_POSITION",i,icount)
      myContentLine(FileName,"FILE","\"sapply(myData,class)\"","COLUMNS","DATA_CLASS",i,nameClass[icount])
      myContentLine(FileName,"FILE","\"sapply(myData,typeof)\"","COLUMNS","DATA_TYPE",i,nameTypeof[icount])
    }
    
    # =================================
    # correlations
    # =================================
    
    
    return (myData)
  }

  # ================================================================================================
  # See if this is the header command for an input dataframe
  # ================================================================================================
  if (class(myData) == "character" && toupper(myData)=="HEADER" && class(Field) == "data.frame") {
    
    if (diagnostics == 1) {print("input is dataframe")}

        # write column headers to the output stream
    myContentLine(FileName,"HEADER")

    # write the File name
    #myContentLine(FileName,"FILE","dataframe details","DETAIL","DATAFRAME","Field", "Field")
    
    # Read data into a dataframe
    myData <- Field
    
    # write number of columns
    #myContentLine(FileName,"FILE","ncol(myData)","DETAIL","COUNT_COLUMNS",Field,ncol(myData))
    # write number of rows
    #myContentLine(FileName,"FILE","nrow(myData)","DETAIL","COUNT_ROWS",Field,nrow(myData))
    
    return(myData)
  }  
    
  # ================================================================================================
  # This must be the Field, and myData will be a dataframe
  # ================================================================================================
  if (diagnostics > 0) {
    print(sprintf("... Analysing %s", Field)) 
    print("")
  }
  
  # Find the class of data as analysis will be different for different datatype
  varclass <- class(myData[,Field])
  if (diagnostics > 0) {print(sprintf("... varclass is %s", varclass))}
  
  # ===============================================================
  # make a table of occurances and their counts
  FieldCounts <- data.frame(table(myData[,Field], dnn=Field))
  
  # coerce dataype back to numeric
  if (varclass[1] == "numeric") {
    FieldCounts[,1] = as.numeric(as.character(FieldCounts[,1]))
  }
  
  # ===============================================================
  if (diagnostics > 0) {print(sprintf("... uniques", nrow(FieldCounts[,1])))}  
  if (diagnostics > 0) {print(sprintf("... nrows/uniques", Field))}
  
  nrows <- nrow(myData[Field])
  nuniques <- nrow(FieldCounts)
  outTemplate <- "Field,%s,%s,%s,%s"
  
  # Datatype
  myContentLine(FileName, "FIELD","sapply(class)","PARAMS","DATATYPE",Field,varclass[1])
  # Length
  myContentLine(FileName, "FIELD","nrows","COUNT","RECORDS",Field,nrows)
  # write number of uniques
  noteText = "\"nrows of data.frame(table(myData[,Field], dnn=Field))\""
  myContentLine(FileName, "FIELD",noteText,"COUNT","UNIQUES",Field,nuniques)
  # Density
  noteText = "nuniques/nrows"
  myContentLine(FileName, "FIELD",noteText,"PARAMS","DENSITY",Field,nuniques/nrows)
  
  
  
  # =================================
  # print stats for numeric variables
  # =================================
  if (varclass[1]=="numeric") {
    
    # Max value
    noteText = "\"max of data.frame(table(myData[,Field], dnn=Field))\""
    myContentLine(FileName, "FIELD",noteText,"PARAMS","MAX",Field,max(FieldCounts[,1]))
    # Min value
    noteText = "\"min of data.frame(table(myData[,Field], dnn=Field))\""
    myContentLine(FileName, "FIELD",noteText,"PARAMS","MIN",Field,min(FieldCounts[,1]))
    # Mean value
    noteText = "\"mean(myData[,Field])\""
    myContentLine(FileName, "FIELD",noteText,"PARAMS","MEAN",Field,mean(myData[,Field]))
    # Std Dev
    noteText = "\"sd(myData[,Field])\""
    myContentLine(FileName, "FIELD",noteText,"PARAMS","STDDEV",Field,sd(myData[,Field]))
    # Var
    noteText = "\"var(myData[,Field])\""
    myContentLine(FileName, "FIELD",noteText,"PARAMS","VAR",Field,var(myData[,Field]))
    # Median
    noteText = "\"median(myData[,Field])\""
    myContentLine(FileName, "FIELD",noteText,"PARAMS","MEDIAN",Field,median(myData[,Field]))
    
  }  
  
  # =================================
  # print stats for factor variables
  # =================================
  if (varclass[1]=="factor") {
    
    # change factor columns of contents to character type
    FieldCounts[,1] <- sapply(FieldCounts[,1], as.character)
    # add a column (#3)  which is the length of the strings
    FieldCounts$Lengths <- nchar(FieldCounts[,1])

    # Longest string
    noteText = "\"max(FieldCounts[,3])\""
    myContentLine(FileName, "FIELD",noteText,"PARAMS","STRING_MAX",Field,max(FieldCounts[,3]))
    # Shortest string
    noteText = "\"min(FieldCounts[,3])\""
    myContentLine(FileName, "FIELD",noteText,"PARAMS","STRING_MIN",Field,min(FieldCounts[,3]))
    # Mean string
    noteText = "\"sum(FieldCounts[,2] * FieldCounts[,3]) / sum(FieldCounts[,2])\""
    myContentLine(FileName, "FIELD",noteText,"PARAMS","STRING_MEAN",Field,sum(FieldCounts[,2] * FieldCounts[,3]) / sum(FieldCounts[,2]))
    # # std dev string
    # myContentLine(FileName, Field,"PARAMS","STRING_STDDEV",sd(FieldCounts[,3]))
    # # var string
    # myContentLine(FileName, Field,"PARAMS","STRING_VAR",var(FieldCounts[,3]))
    # median string
    # myContentLine(FileName, Field,"PARAMS","STRING_MEDIAN",median(FieldCounts[,3]))
  }
  
  # convert field to character list so we can get some features
  myRows <- nrow(myData)
  FieldAsChar <- lapply(myData[,Field], as.character)

  # ================================================================
  # might be an email address  
  # ================================================================
  density.AtSign <- sum(grepl("@", FieldAsChar))
  if (density.AtSign > 0 ) {
    density.AtSign <- density.AtSign / myRows
  }
  noteText <- "sum(grepl(\"@\", FieldAsChar)) / myRows"
  myContentLine(FileName, "FIELD",noteText,"DENSITY","@",Field,density.AtSign)

  # ================================================================
  # might be a numeric
  # ================================================================
  density.dot <- sum(grepl(".", FieldAsChar))
  if (density.dot > 0 ) {
    density.dot <- density.dot / myRows
  }
  noteText <- "sum(grepl(\".\", FieldAsChar)) / myRows"
  myContentLine(FileName, "FIELD",noteText,"DENSITY","Dot",Field,density.dot)
  
  # ================================================================
  # might be a foreign numeric (They use ,)
  # ================================================================
  density.comma <- sum(grepl(",", FieldAsChar))
  if (density.comma > 0 ) {
    density.comma <- density.comma / myRows
  }
  noteText <- "sum(grepl(\",\", FieldAsChar)) / myRows"
  myContentLine(FileName, "FIELD",noteText,"DENSITY","Comma",Field,density.comma)

  # ================================================================
  # might be a url
  # ================================================================
  density.https <- grepl("https:", FieldAsChar)
  density.http <- grepl("http:", FieldAsChar)
  density.www <- grepl("www.", FieldAsChar)
  density.url <- density.https | density.http | density.www
  
  density.url <- sum(density.url)
  myContentLine(FileName, "FIELD",noteText,"DENSITY","Urlcount",Field,sum(density.url))
  if (density.url > 0 ) {
    density.url <- density.url / myRows
  }
  
  # ================================================================
  # density of numerics
  # ================================================================
  density.nums <- str_count(FieldAsChar, regex("[:digit:]"))
  density.lengths <- nchar(FieldAsChar)
  density.numdens <- density.nums / density.lengths
  
  noteText <- "TOO COMPLICATED : Mean of numeric density per case"
  myContentLine(FileName, "FIELD",noteText,"DENSITY","Numerics",Field,mean(density.numdens))

  # ================================================================
  # density of [A-Z][a-z]
  # ================================================================
  density.alphas <- str_count(FieldAsChar, regex("[:alpha:]"))
  density.numalphas <- density.alphas / density.lengths

  noteText <- "TOO COMPLICATED : Mean of alphas density per case"
  myContentLine(FileName, "FIELD",noteText,"DENSITY","Alphas",Field,mean(density.numalphas))
  
  # ================================================================
  # density of whitespace
  # ================================================================
  density.space <- str_count(FieldAsChar, regex("[:blank:]"))
  density.numspaces <- density.space / density.lengths

  noteText <- "TOO COMPLICATED : Mean of whitespace density per case"
  myContentLine(FileName, "FIELD",noteText,"DENSITY","Spaces",Field,mean(density.numspaces))

  # ================================================================
  # density of punctuation
  # ================================================================
  density.punct <- str_count(FieldAsChar, regex("[^:blank:,:alpha:,:digit:]"))
  density.numpunct <- density.punct / density.lengths

  noteText <- "TOO COMPLICATED : Mean of punctuation density per case"
  myContentLine(FileName, "FIELD",noteText,"DENSITY","Punctuation",Field,mean(density.numpunct))

} # end of the function

# ==============================================================
# Returns a Count of the occurances of searchFor in within
# ==============================================================
contains <- function(searchFor, within) {
  # Substitute target string for blanks
  without <- gsub(searchFor,"",within)
  # Get the difference in length before and after
  # Divide by length of target string
  return ((nchar(within) - nchar(without)) / nchar(searchFor))
}

