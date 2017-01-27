
library(ggplot2)
library(cowplot)

xx(iris,"Sepal.Width")

xx <- function(myData, Field) {
  plotData <- myData[c(Field)]
  plotData <- as.data.frame(plotData[complete.cases(plotData),])
  print(plotData)
  myBinWidth = (max(plotData[,1]) - min(plotData[,1])) / 20
  print(myBinWidth)
}

testPlot_Int <- function(myData, Field) {
  #rnorm <- data.frame(x = c(rnorm(length(myData[,Field]))))
  
  plotData <- myData[c(Field)]
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
# 
# 
# plot1 <- qplot(1)
# plot2 <- qplot(1)
# #plot_grid(plot1, plot2, align='h', labels=c('A', 'B'))
# plot_grid(plot1, plot2, align='h')
# 
# 
# dat <- data.frame(
#   time = factor(c("Lunch","Dinner"), levels=c("Lunch","Dinner")),
#   total_bill = c(14.89, 17.23)
# )
# dat
# #>     time total_bill
# #> 1  Lunch      14.89
# #> 2 Dinner      17.23
# 
# # Load the ggplot2 package
# # Very basic bar graph
# ggplot(data=dat, aes(x=time, y=total_bill)) +
#   geom_bar(stat="identity")
# 
# 
# 
# set.seed(124)
# norm <- as.data.frame(rnorm(1000))
# names(norm) <-c("vals")
# 
# #myData <- iris
# #f1 <- "Petal.Length"
# #f2 <- "Sepal.Width"
# 
# #Field <- f2
# 
# p1 <- myPlot_Int(norm, "vals")
# p1
# 
# 
# 
# # ====================================
# # Histogram with 20 bins, showing mean
# # ====================================
# myPlot_Int <- function(myData, Field) {
# #rnorm <- data.frame(x = c(rnorm(length(myData[,Field]))))
# 
# myBinWidth = (max(myData[,Field]) - min(myData[,Field])) / 20
# maxy <- max(hist(myData[,Field], breaks=seq(min(myData[,Field]), max(myData[,Field]), by=myBinWidth), plot=FALSE)$counts)
# 
# myMean = mean(myData[,Field])
# myMedian = median(myData[,Field])
# myQuantile = quantile(myData[,Field])
# 
# ColorMean <- "blue"
# 
# myPlot <- ggplot(myData, 
#        aes_string(x=Field)) + 
#   geom_histogram(binwidth=myBinWidth, colour="black", fill="#EBEDFA") +
#   
#   geom_vline(aes_string(xintercept=myMean), color=ColorMean, linetype="solid", size=1) +
#   annotate("text", x = myMean, y = maxy * 0.9, label = "Mean", color=ColorMean) +
#   
#   geom_vline(aes_string(xintercept=myQuantile[2]), color="sienna1", linetype="solid", size=.5) +
#   annotate("text", x = myQuantile[2], y = maxy * 0.7, label = "Q1", color="sienna1") +
#   
#   geom_vline(aes_string(xintercept=myQuantile[3]), color="sienna3", linetype="solid", size=1) +
#   annotate("text", x = myQuantile[3], y = maxy, label = "Median", color="sienna3") +
#   
#   geom_vline(aes_string(xintercept=myQuantile[4]), color="sienna1", linetype="solid", size=.5) +
#   annotate("text", x = myQuantile[4], y = maxy * 0.7, label = "Q3", color="sienna1") +
# 
#   theme(panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank(), 
#         axis.line=element_blank(),
#         axis.text.x=element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks=element_blank(),
#         axis.title.x=element_blank(),
#         axis.title.y=element_blank(),
#         legend.position="none")
# 
# return(myPlot)
# 
# }
