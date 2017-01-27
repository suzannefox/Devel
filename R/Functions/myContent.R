
# ==============================================================
myContentLine <- function(FileName, var1,var2,var3,var4) {
  
  var1 <- toupper(var1)
  
  if (var1[1]=="HEADER") {
    print("Var1")
    write.table("InfoType, Id, Param, Info1, Info1Value, Info2, Info2Value", 
                FileName, append=FALSE, row.names=FALSE,col.names=FALSE, quote=FALSE)
    
  } else if (var1[1]=="FILE") {
    outTemplate <- "File,%s,%s,%s,%s"
    outString <- sprintf(outTemplate,var1,var2,var3,var4)
    write.table(outString, FileName, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)
    
  } else {
    outTemplate <- "Field,%s,%s,%s,%s"
    outString <- sprintf(outTemplate,var1,var2,var3,var4)
    write.table(outString, FileName, append=TRUE, row.names=FALSE,col.names=FALSE, quote=FALSE)
  }
}

# ==============================================================
myContent <- function(myData, Field, FileName) {
  
  diagnostics <- 0
  
  # ================================================================================================
  # See if this is the header command
  # ================================================================================================
  if (toupper(Field)=="HEADER") {
    myContentLine(FileName,"HEADER","","","")
    return()
  }

  # ================================================================================================
  # See if this is to be data about the file
  # ================================================================================================
  if (toupper(Field)=="FILE") {
    # number of columns
    myContentLine(FileName,"FILE","COUNT","COLUMNS",ncol(myData))
    # number of rows
    myContentLine(FileName,"FILE","COUNT","ROWS",nrow(myData))
    
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
      myContentLine(FileName,"FILE","COLUMN_NAME",Ftext,i)
      myContentLine(FileName,"FILE","COLUMN_CLASS",Ftext,nameClass[icount])
      myContentLine(FileName,"FILE","COLUMN_TYPE",Ftext,nameTypeof[icount])
    }
    
    return()
  }

  # ================================================================================================
  # This must be the Field
  # ================================================================================================
  # Get unique values and their count
  library(plyr)
  if (diagnostics > 0) {print(sprintf("Analysing %s", Field))}

  if (diagnostics > 0) {print(sprintf("... FieldCounts", Field))}
  FieldCounts <- count(myData[Field])
  
  if (diagnostics > 0) {print(sprintf("... varclass", Field))}
  varclass <- sapply(FieldCounts,class)
  
  if (diagnostics > 0) {print(sprintf("... nrows/uniques", Field))}
  nrows <- nrow(myData[Field])
  nuniques <- nrow(FieldCounts)
  outTemplate <- "Field,%s,%s,%s,%s"
  
  # ===========================================
  # Datatype
  myContentLine(FileName, Field,"PARAMS","DATATYPE",varclass[1])
  # Length
  myContentLine(FileName, Field,"COUNT","RECORDS",nrows)
  # write number of uniques
  myContentLine(FileName, Field,"COUNT","UNIQUES",nuniques)
  # Density
  myContentLine(FileName, Field,"PARAMS","DENSITY",nuniques/nrows)

  # ---------------------------------
  if (diagnostics > 0) {
    print(sprintf("Variables type is %s", varclass[1]))
  }

    # print stats for numeric variables
  if (varclass[1]=="numeric") {
    
    # Max value
    myContentLine(FileName, Field,"PARAMS","MAX",max(FieldCounts[,1]))
    # Min value
    myContentLine(FileName, Field,"PARAMS","MIN",min(FieldCounts[,1]))
    # Mean value
    myContentLine(FileName, Field,"PARAMS","MEAN",mean(FieldCounts[,1]))
    # Std Dev
    myContentLine(FileName, Field,"PARAMS","STDDEV",sd(FieldCounts[,1]))
    # Var
    myContentLine(FileName, Field,"PARAMS","VAR",var(FieldCounts[,1]))
    # Median
    myContentLine(FileName, Field,"PARAMS","MEDIAN",median(FieldCounts[,1]))

    # ---------------------------------
    # print stats for factor variables
  }  else if (varclass[1]=="factor") {
    
    # Make a vector of the columns which are factors
    i <- sapply(FieldCounts, is.factor)
    # change any factor columns to character type
    FieldCounts[i] <- lapply(FieldCounts[i], as.character)
    # add a column (#3)  which is the length of the strings
    FieldCounts$Lengths <- nchar(FieldCounts[,1])
    # add various column which count the occurances of string features
    FieldCounts$AtSigns <- sapply(FieldCounts[,1], function(x) contains("@",x))

    # Longest string
    myContentLine(FileName, Field,"PARAMS","STRING_MAX",max(FieldCounts[,3]))
    # Shortest string
    myContentLine(FileName, Field,"PARAMS","STRING_MIN",min(FieldCounts[,3]))
    # Mean string
    myContentLine(FileName, Field,"PARAMS","STRING_MEAN",mean(FieldCounts[,3]))
    # std dev string
    myContentLine(FileName, Field,"PARAMS","STRING_STDDEV",sd(FieldCounts[,3]))
    # var string
    myContentLine(FileName, Field,"PARAMS","STRING_VAR",var(FieldCounts[,3]))
    # median string
    myContentLine(FileName, Field,"PARAMS","STRING_MEDIAN",median(FieldCounts[,3]))
  }
}

# ==============================================================
# Count the number of occurances of searchFor in within
contains <- function(searchFor, within) {
  # Substitute target string for blanks
  without <- gsub(searchFor,"",within)
  # Get the difference in length before and after
  # Divide by length of target string
  return ((nchar(within) - nchar(without)) / nchar(searchFor))
}
