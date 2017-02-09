
workdir <- "C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/"
source("C:/Users/Suzanne/OneDrive - Suzanne Fox/Dissertation/R/Burrow.R")

#Data.choose = "x"
#Data.choose <- "Hair"
#Data.choose <- "iris"
#Data.choose <- "mtcars"
Data.choose <- "Arth"


if (Data.choose=="iris") {
  Data.source <- iris
  Data.title <- "iris"

} else if (Data.choose=="mtcars") {
  Data.source <- mtcars
  Data.title <- "mtcars"
  
} else if (Data.choose=="Hair") {
  Data.source <- as.data.frame(HairEyeColor)
  Data.title <- "HairEyeColor"
  
} else if (Data.choose=="Arth") {
  library(vcd)
  Data.source <- as.data.frame(Arthritis)
  Data.title <- "Arthritis"
  
} else {
  Data.source <- read.csv(paste(workdir,"TestData.csv",sep=""))
  Data.title <- "synthetic dataset"
}

Data.temp <- Burrow(Data.source, Data.title, TRUE)
Data.burrow <- Data.temp$data

# Get the best guesses
Data.vars.guess <- subset(Data.burrow, 
                           InfoType=="BEST GUESS")

# Get all the variables which are number fields
Data.vars.category <- subset(Data.burrow, 
                           InfoType=="BEST GUESS" 
                           & (myData1=="CATEGORICAL"))

# if (nrow(Data.vars.category)==0) {
#   print("No categorical variables in this dataset")
#   
# } else {
#   Data.list.categories <- as.list(Data.vars.category[c("Variable1")])
#   Data.categorical <- as.data.frame(Data.source[,c(Data.list.categories$Variable1)])
#   names(Data.categorical) <- Data.list.categories$Variable1
  
#  for (i in 1:nrow(Data.vars.category)) {
    # Generate data
    #Data.col <- Data.vars.category[i,c("Variable1")]

    #x1 <- as.data.frame(table(Data.categorical[,c(Data.col)]), stringsAsFactors = FALSE)
    #sumx1 <- sum(x1$Freq)
    
    #names(x1) <- c(Data.col,"Count")
    #x1$Pcent <- x1$Count / sumx1 * 100
    
    #print(".... 1")
    # myPlot <- ggplot(data=x1, 
    #        aes_string(x=Data.col,  y="Pcent")) +
    #   geom_bar(colour="black", fill="#4C1E9C",
    #            width=.8, stat="identity") +
    #   xlab("Categories") + ylab("% of Total Sample") +
    #   ggtitle(paste("Distribution of Categories for Variable :",Data.col)) +
    #   theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    
    #print(myPlot)
#  }

#}

# ==================================



