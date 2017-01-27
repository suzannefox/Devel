# =============================================================================
# normality test

#Invoking the "nortest" package into the active R session
library(nortest)

#Generating a sample of 100 random numbers from a Gaussian / normal distribution
test.norm <- rnorm(100,10,1)

#Generating a sample of 100 numbers from a non-normal data set
test.normnot <- rweibull(100,1,5)

test.n1 <- ad.test(test.norm)
test.n2 <- ad.test(test.normnot)

ad.test(psych.cdata$A3)

test.n1$p.value
test.n2$p.value

test.norm <- as.data.frame(test.norm)
test.normnot <- as.data.frame(test.normnot)
ggplot(test.norm, aes(sample=test.norm))+stat_qq()
ggplot(test.normnot, aes(sample=test.normnot))+stat_qq()

# =============================================================================
# violin plots/boxplots

# make a typical  "wide" format dataset
violin.data = data.frame(AAA=rnorm(100,1,1),
                BBB=rnorm(100,2,1.5),
                CCC=rnorm(100,1.5,1.2))

# reshape to "long" format
violin.m <- reshape2::melt(violin.data, id.vars = NULL)

# make plot
library(ggplot2)
ggplot(violin.m, aes(x = variable, y = value)) + geom_violin()

ggplot(data = violin.m, aes(x=variable, y=value)) +
     geom_boxplot() #+
#     facet_wrap( ~ variable, scales="free")

# =============================================================================
# psych library plots
library(psych)
data(bfi)
psych.cdata <- na.omit(bfi) # listwise deletion of missing

# scatterplot. Use jitter to show densities at intersections
library(ggplot2)
ggplot(psych.cdata, aes(x=E2, y=E4)) +
  geom_jitter() +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)

# get column means as a vector
psych.means <- as.data.frame(colMeans(psych.cdata))

# cluster analysis
psych.sdata <- scale(psych.cdata) # standardize variables 
# K-Means Cluster Analysis
psych.fit <- kmeans(psych.sdata, 6) # 5 cluster solution
library(fpc)
plotcluster(psych.cdata, psych.fit$cluster) 

# Boxplots
psych.melt <- reshape2::melt(psych.data[,c(1:25)], id.vars = NULL)

# box plot
ggplot(data = psych.melt, 
       aes(x=variable, y=value)) +
       geom_boxplot() 

# violin plot
ggplot(data = psych.melt, 
       aes(x=variable, y=value)) +
       geom_violin() 

psych.norm <- data.frame(
  x = rnorm(100)
)

ggplot(psych.data, aes(x="Age")) + 
      geom_density() +
      stat_function(fun = dnorm, colour = "red")

# =============================================================================
# plots for burrowdata

# library(ggplot2)

plotvars <- c("DENSITY_UNIQUES","Density of Unique Occurances")
#plotvars <- c("REGEX_NUMMBER","Density of Regex Number Matches")
#plotvars <- c("COUNT_NA","Density of Regex Number Matches")

features <- subset(features.all, FILE_NAME=="iris",)

dat <- subset(features,,select=c("COLUMN_NAME",plotvars[1]))
names(dat) <- c("Cols","Count")

ggplot(dat,aes(Cols, Count))+
  geom_bar(stat="identity", fill="white") +
  geom_text(dat,mapping=aes(x=Cols, y=0.1), label=dat$Cols, angle=90, colour="blue") +
  ggtitle(plotvars[2]) +
  labs(x = "Columns", y = "Density") +
  ylim(0,1) +
  theme(
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
