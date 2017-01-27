


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
