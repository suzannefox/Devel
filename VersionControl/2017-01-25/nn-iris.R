
# make training data
itrain <- iris[sample(1:150, 50),]

# recode target as t/f
itrain$outsetosa <- c(itrain$Species == "setosa")
itrain$outversicolor <- c(itrain$Species == "versicolor")
itrain$outvirginica <- c(itrain$Species == "virginica")

# remove target col
itrain$Species <- NULL

library(neuralnet)

n <- names(itrain)
f <- as.formula(paste(
                  paste(n[grepl("out*",n)], collapse = " + "), 
                  "~",
                  paste(n[!grepl("out*",n)], collapse = " + ")))

nn <- neuralnet(f, data=itrain, hidden=c(3), lifesign="full")

plot(inet, rep="best")

predict <- compute(inet, iris[1:4])

result<-0

for (i in 1:150) { result[i] <- which.max(predict$net.result[i,]) }
for (i in 1:150) { if (result[i]==1) {result[i] = "setosa"} }
for (i in 1:150) { if (result[i]==2) {result[i] = "versicolor"} }
for (i in 1:150) { if (result[i]==3) {result[i] = "virginica"} }

comparison <- iris
comparison$Predicted <- result
