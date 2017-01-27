
# Source : http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
library(ggplot2)
# scatter plot
myData <- iris
f1 <- "Petal.Length"
f2 <- "Sepal.Width"

Field <- f2
#ggplot(myData) + geom_point(aes_string(f1, f2))


# ====================================
# Histogram with 20 bins, showing mean
# ====================================
rnorm <- data.frame(x = c(rnorm(length(myData[,Field]))))

myBinWidth = (max(myData[,Field]) - min(myData[,Field])) / 20
maxy <- max(hist(myData[,Field], breaks=seq(min(myData[,Field]), max(myData[,Field]), by=myBinWidth), plot=FALSE)$counts)

myMean = mean(myData[,Field])
myMedian = median(myData[,Field])
myQuantile = quantile(myData[,Field])

ggplot(myData, 
      aes_string(x=Field)) + 
      geom_histogram(binwidth=myBinWidth, colour="black", fill="grey") +

      geom_vline(aes_string(xintercept=myMean), color="dodgerblue", linetype="solid", size=1) +
      annotate("text", x = myMean, y = maxy * 0.9, label = "Mean", color="dodgerblue") +
  
      geom_vline(aes_string(xintercept=myQuantile[2]), color="sienna1", linetype="solid", size=.5) +
      annotate("text", x = myQuantile[2], y = maxy * 0.7, label = "Q1", color="sienna1") +

      geom_vline(aes_string(xintercept=myQuantile[3]), color="sienna3", linetype="solid", size=1) +
      annotate("text", x = myQuantile[3], y = maxy, label = "Median", color="sienna3") +

      geom_vline(aes_string(xintercept=myQuantile[4]), color="sienna1", linetype="solid", size=.5) +
      annotate("text", x = myQuantile[4], y = maxy * 0.7, label = "Q3", color="sienna1") 
  

# ggplot(rnorm, 
      #        aes(x)) +
      # geom_density()

# ====================================
# Density curve
# ====================================
ggplot(myData, 
       aes_string(x=Field)) + 
       geom_density()

# ====================================
# CDF
# ====================================
# make a df with the test data, a normal distribution 
normData <- data.frame(x = c(myData[,Field], 
                             rnorm(length(myData[,Field]))),
                       
                       g = gl(2, length(myData[,Field])))


normData <- data.frame(x = c(rnorm(length(myData[,Field])), 
                             rnorm(length(myData[,Field]))),
                       
                       g = gl(2, length(myData[,Field])))

ggplot(normData,
       aes(x, colour = g)) +
       stat_ecdf()


# ====================================
# qq plot
# ====================================
# see http://onlinestatbook.com/2/advanced_graphs/q-q_plots.html
ggplot(myData, 
       aes_string(sample = Field)) +
       stat_qq()

# ====================================
# Sorted index plot
# ====================================
Field = "Petal.Width"
sortdata  <- data.frame(sort(myData[,Field]))
ggplot() +
geom_point(aes_string(x=seq_along(sortdata[,1]), 
                      y=sortdata[,1]))