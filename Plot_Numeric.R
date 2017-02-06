
library(ggplot2)
library(gridExtra)

# Generate data
data.orig <- subset(iris,,select=c("Sepal.Width"))
data.plot <- as.data.frame(stack(data.orig))
names(data.plot) <- c("Values","Variable")

# get stats
data.mean <- mean(data.plot$Values)
data.median <- median(data.plot$Values)

# identify outliers
data.outliers <- boxplot.stats(data.plot$Values)
data.outlierindex <- data.plot$Values %in% data.outliers$out
data.plot$outliers <- "NOT"
data.plot$outliers[data.outlierindex] <- "IS"


x3 <- ggplot(data.plot, aes(sample=Values)) 
x3 <- x3 + stat_qq()

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
scale_colour_manual(values=cbPalette)
scale_fill_manual(values=cbPalette)

# scale_fill_brewer()

colss <- c(IS="firebrick3", NOT="mediumseagreen")

x3 <- ggplot(data.plot)
x3 <- x3 + scale_colour_manual(values = colss) +   # outliers colours
  scale_fill_manual(values = colss)       # boxes colours
x3 <- x3 + stat_qq(aes(sample=Values, colour = factor(outliers)))  
x3 <- x3 + geom_hline(yintercept = data.mean, color="blue")
x3 <- x3 + geom_hline(yintercept = data.median, color="blue")
x3 <- x3 + geom_hline(yintercept = data.outliers$stats[2], color="red")
x3 <- x3 + geom_hline(yintercept = data.outliers$stats[4], color="red")
x3 <- x3 + theme(legend.position="none")
#x3 <- x3 + geom_point(aes(colour = outliers)) + scale_colour_gradient(low = "blue")
x3

