
myBurrowFeatures <- function(myBurrowData) {
  
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
    
    if (i==1) {
      myFeatures <- as.data.frame(myData.6)
    } else {
      myFeatures <- rbind(myFeatures, myData.6)
    }
  }
  
  return(myFeatures)
}