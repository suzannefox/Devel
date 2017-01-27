

myData <- myBurrow(iris)

# get the filename
myData.1 <- subset(myData, InfoDetail=="FILE_NAME", 
                           select=c("InfoDetail", "myData1"))

# get each variable
myData.Variables <- subset(myData, InfoDetail=="COLUMN_NAME", 
                                   select=c("InfoDetail", "myData1"))

i <- 5

# get all the data for this variable
myData.VariableData <- subset(myData, Variable1==myData.Variables[i,"myData1"])

# make a df for the name
myData.2 <- subset(myData, Variable1=="COLNAME" & myData1==myData.Variables[i,"myData1"],
                           select=(c("InfoDetail","myData1")))

# get the r type
myData.3 <- subset(myData.VariableData, InfoDetail=="DATA_TYPE_R",
                                        select=(c("InfoDetail","myData1")))

# get the features
myData.4 <- subset(myData.VariableData, InfoType=="FEATURE",
                   select=(c("InfoDetail","myData1")))

# Make a combined df
myData.RawFeatures = rbind.data.frame(myData.1, myData.2, myData.3, myData.4)

myData.Features <- t(myData.RawFeatures[,c(2)])
colnames(myData.Features)<- myData.RawFeatures[,c(1)]

# transpose it
# Get features for each variable
# for(i in 1:nrow(myData.Variables)) { 
#   # get all the data for this variable
#   myData.VariableData <- subset(myData, Variable1==myData.Variables[i,"myData1"])
#   
