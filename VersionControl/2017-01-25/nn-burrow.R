

nn.Data <- myFeatures

nn.Data$FILE_NAME <- NULL
nn.Data$COLUMN_NAME <- NULL
nn.Data$WIDTH_MAX <- NULL
nn.Data$WIDTH_MIN <- NULL
nn.Data$WIDTH_MEAN <- NULL

nn.Data$UNIQUES <- as.numeric(as.character(nn.Data$UNIQUES))
x <- table(nn.Data$DATA_TYPE_R)
x

# make training data
itrain <- nn.Data[sample(1:58, 25),]

# recode target as t/f
itrain$outdouble <- c(itrain$DATA_TYPE_R == "double")
itrain$outinteger <- c(itrain$DATA_TYPE_R == "integer")
itrain$outcharacter <- c(itrain$DATA_TYPE_R == "character")

# remove target col
itrain$DATA_TYPE_R <- NULL

library(neuralnet)

n <- names(itrain)
f <- as.formula(paste(
                  paste(n[grepl("out*",n)], collapse = " + "), 
                  "~",
                  paste(n[!grepl("out*",n)], collapse = " + ")))
f

#nn <- neuralnet(f, data=itrain, hidden=c(3), lifesign="full")

nn <- neuralnet("outdouble + outinteger + outcharacter ~ UNIQUES + DOT_ROW", data=itrain, hidden=c(3), lifesign="full")

plot(inet, rep="best")

predict <- compute(inet, iris[1:4])

result<-0

for (i in 1:150) { result[i] <- which.max(predict$net.result[i,]) }
for (i in 1:150) { if (result[i]==1) {result[i] = "setosa"} }
for (i in 1:150) { if (result[i]==2) {result[i] = "versicolor"} }
for (i in 1:150) { if (result[i]==3) {result[i] = "virginica"} }

comparison <- iris
comparison$Predicted <- result
