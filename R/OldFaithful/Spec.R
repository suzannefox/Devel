# use the old faithful dataset
attach(faithful)
# get the structure
str(faithful)
# get the eruptions
duration <- faithful$eruptions
# find out what sort of object duration is
cltyped <- class(duration)
# find the range
range(duration)
# frequency table
table(duration)
table(waiting)
# make bins
bins <- seq(1.5, 5.5, by=0.5)
duration.cut <- cut(duration, bins, right=FALSE)
table(duration.cut)
cbind(table(duration.cut))
#histogram
hist(duration)
# This won't work as duration.cut is a factor
# hist(duration.cut)
# cumulative frequency
distcumsum <- cumsum(duration) 
# this is meaningless
hist(distcumsum)


