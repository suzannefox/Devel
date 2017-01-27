loglik <- function(param)
{
  mu <-param[1]
  sigma = param[2]
  
  loglike <- -0.5N*log(2*pi)-N*log(sigma)-sum(0.5*(x-mu)^2/sigma^2)
  loglike
}

# library(maxlike)
x <- rnorm(1000,1,2)
N <- length(x)
res = maxLik(loglik, start = c(0,1))
print(res)
# coeff(res)
?maxLik