
# make training data
itrain <- iris[sample(1:150, 50),]

# recode target as t/f
itrain$setosa <- c(itrain$Species == "setosa")
itrain$versicolor <- c(itrain$Species == "versicolor")
itrain$virginica <- c(itrain$Species == "virginica")

# remove target col
itrain$Species <- NULL

library(neuralnet)
inet <- neuralnet(setosa + versicolor + virginica ~ 
                  Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, itrain, hidden=3, lifesign="full")

plot(inet, rep="best")

predict <- compute(inet, iris[1:4])

result<-0

for (i in 1:150) { result[i] <- which.max(predict$net.result[i,]) }
for (i in 1:150) { if (result[i]==1) {result[i] = "setosa"} }
for (i in 1:150) { if (result[i]==2) {result[i] = "versicolor"} }
for (i in 1:150) { if (result[i]==3) {result[i] = "virginica"} }

comparison <- iris
comparison$Predicted <- result
