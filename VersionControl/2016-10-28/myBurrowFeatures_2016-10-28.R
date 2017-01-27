
myBurrowFeatures <- function(myBurrowData) {
  
  # get the source filename
  myData.1 <- subset(myBurrowData, InfoDetail=="FILE_NAME", 
                                   select=c("InfoDetail", "myData1"))
  
  # get list of variables
  myData.Variables <- subset(myBurrowData, InfoDetail=="COLUMN_NAME", 
                                           select=c("InfoDetail", "myData1"))

  regex.numerics <- "^[0-9\\.]*$"
  
  # iterate round the variables. i is the counter of the variable
  for(i in 1:nrow(myData.Variables)) { 
  
    # myData.Variables[i, "myData1"] : will be the name of the variable
    
    # get all the data for this variable
    myData.VariableData <- subset(myBurrowData, Variable1==myData.Variables[i,"myData1"])
    
    # make a df for the name
    myData.2 <- subset(myBurrowData, Variable1=="COLNAME" & myData1==myData.Variables[i,"myData1"], 
                                     select=(c("InfoDetail","myData1")))
    
    # get the r type
    myData.3 <- subset(myData.VariableData, InfoDetail=="DATA_TYPE_R", 
                                            select=(c("InfoDetail","myData1")))

    # get the features
    myData.4 <- subset(myData.VariableData, InfoType=="FEATURE", 
                                            select=(c("InfoDetail","myData1")))
    
    # Make a combined df of all those elements
    #myData.4 <- sapply(myData.4, as.numeric)
    mtcars.4a <- as.data.frame(sapply(myData.4, as.numeric))
    myData.RawFeatures = rbind.data.frame(myData.1, myData.2, myData.3, myData.4a)
    
    # Transpose the dataframe
    myData.5 <- t(myData.RawFeatures[,c(2)])
    colnames(myData.5)<- myData.RawFeatures[,c(1)]
  
    if (i==1) {
      myFeatures <- as.data.frame(myData.5)
    } else {
      myFeatures <- rbind(myFeatures, myData.5)
    }
  }
  
  # convert columns from factors to numerics
  # indx <- sapply(myFeatures, is.numeric)
  # myFeatures[indx] <- lapply(myFeatures[indx], function(x) as.numeric(as.character(x)))
  
  return(myFeatures)
}