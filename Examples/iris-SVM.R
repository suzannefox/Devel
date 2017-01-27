#install.packages("e1071")

library("e1071")
data(iris)
attach(iris)

## classification mode
# default with factor response:
model_1 <- svm(Species ~ ., data = iris)

# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- Species
model_2 <- svm(x, y) 
print(model_2)
summary(model_2)

# test with train data
pred <- predict(model_2, x)

# (same as:)
pred <- fitted(model_2)

# Check accuracy:
accuracy <- table(pred, y)

# compute decision values and probabilities:
pred <- predict(model_2, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model_1$index + 1],
     main="Model 1")

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model_2$index + 1],
     main="Model 2")

# test from http://machinomics.blogspot.co.uk/2012/03/multiclass-svm-with-e1071_20.html
#data(iris) 
#attach(iris) 
#model <- svm(Species ~ ., data = iris) 
#x <- subset(iris, select = -Species) 
#y <- Species 
model_3 <- svm(x, y, probability = TRUE)

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model_3$index + 1],
     main="Model 3")

pred_3 <- predict(model_3, x)
pred_3b <- predict(model_3, x, decision.values = TRUE, probability = TRUE)

# =======================================================
# function to classify new data from the model
# =======================================================
predsvm<-function(model, newdata)
{
  prob<-attr(predict(model, newdata, probability = TRUE),"probabilities")
  n<-dim(prob)[1]
  m<-dim(prob)[2]
  
  me<-which(prob==apply(prob,1,max))
  return(as.factor(model$labels[floor((me-1)/n)+1]))
} 

# =======================================================
# One might also program the following function, that deals 
# with the way the support vector coefficients are stored in 
# the model object, in model$coefs and model$rho:
# http://machinomics.blogspot.co.uk/2012/03/multiclass-svm-with-e1071_20.html
# =======================================================
  
## Linear Kernel function
K <- function(i,j) crossprod(i,j)

predsvm <- function(object, newdata) {
  ## compute start-index
  start <- c(1, cumsum(object$nSV)+1)
  start <- start[-length(start)]
  
  ## compute kernel values
  kernel <- sapply (1:object$tot.nSV,
                    function (x) K(object$SV[x,], newdata))
  
  ## compute raw prediction for classifier (i,j)
  predone <- function (i,j) {
    ## ranges for class i and j:
    ri <- start[i] : (start[i] + object$nSV[i] - 1)
    rj <- start[j] : (start[j] + object$nSV[j] - 1)
    
    ## coefs for (i,j):
    coef1 <- object$coefs[ri, j-1]
    coef2 <- object$coefs[rj, i]
    
    ## return raw values:
    crossprod(coef1, kernel[ri]) + crossprod(coef2, kernel[rj])
  }
  
  ## compute votes for all classifiers
  votes <- rep(0,object$nclasses)
  c <- 0 # rho counter
  for (i in 1 : (object$nclasses - 1))
    for (j in (i + 1) : object$nclasses)
      if (predone(i,j) > object$rho[c <- c + 1])
        votes[i] <- votes[i] + 1
  else
    votes[j] <- votes[j] + 1
  
  ## return winner (index with max. votes)
  object$levels[which(votes %in% max(votes))[1]]
}


