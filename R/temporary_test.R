require(lmls)
require(ggplot2)
require(Rcpp)
require(mvtnorm)
source("R/estimation.R")
source("R/helper.R")
source("R/init.R")
source("R/spline.R")
source("R/methods.R")
source("R/mcmc.spline.R")


set.seed(1)
#x = seq(0,20, length.out = 600)
x = sort(runif(500, 0,20))
y =  (x-10) * rnorm(500,0, 1) + x + (x-10)^2/100 *  rnorm(500,0, 1) #da funktioniert es nicht... backward un und forward, sind dann sehr stark voneinander verschieden. Daher wird nie angenommen
#y =  sin(x) + rnorm(500,0, 1) + x
m = lmls(y~x, scale = ~x, light = F)
m = spline(m, c(50,50), order = c(2,2), p_order = c(2,2), smooth = c(1,1))
plot(m, sd = 1.96)

n = 100
lol = mcmc.spline(m, it = n)




n = length(lol$tau)
seq1 = seq(1, n, length.out = n)
plot(seq1, lol$tau, type = "l")
plot(seq1, lol$epsilon, type = "l")
plot(seq1, lol$beta[, 1], type = "l")
plot(seq1, lol$gamma[, 5], type = "l")


























bmean = colMeans(lol$beta)
gmean = colMeans(lol$gamma)
length(gmean)

fit.spline = function(beta, gamma, X)
{
  location = X %*% beta
  scale = exp(X %*% gamma)
  return(list(location = location, scale =  scale))
}


pred = fit.spline(bmean, gmean, m$loc$X)

plot(x, pred$location)
points(x, y)
lines(x, pred$location + 1.96 * pred$scale)
lines(x, pred$location - 1.96 * pred$scale)
