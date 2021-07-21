#source necessary files and generate model object

source("R/spline/print_function.R")
require(lmls)
source("R/spline/estimation.R")
source("R/spline/init.R")
source("R/spline/spline.R")
source("R/spline/mcmc_framework_jonathan.R")



set.seed(1111)
x = seq(0,10, length.out = 500)
y =  x + rnorm(500, 0, 1 + sin(x))
m = lmls(y ~ x, light = F)
m = spline_user_function(m = m, kn =  10, order =  2, p_order =  2,lambda =  100)


print.spl(m, sd = 1.96)


m = m$spline



# evaluate code performance with profvis and rprof (set its to 1 for this purpose) #####
library(profvis)
profvis({
  mcmc(m)
})

Rprof("mcmc_prof.log", line.profiling = TRUE)
mcmc(m)
Rprof(NULL)
summaryRprof("mcmc_prof.log", lines = "show")

#test without code performance evaluation ######
lol = mcmc(m, 1000)


bmean = colMeans(lol$beta)
gmean = colMeans(lol$gamma)

fit.spline = function(beta, gamma, X)
{
  location = X %*% beta
  scale = exp(X %*% gamma)
  return(list(location = location, scale =  scale))
}

lol
prediction = fit.spline(bmean, lol$gamma[40,], m$x)

plot(x, prediction$location)
plot(x, prediction$scale)







#Hier müssen noch x und y richtig definiert werden für den scatterplot
print.spl <- function(prediction , sd = 1.96)
{
  require(ggplot2)
  ggplot(mapping = aes(x = x)) +
    geom_point(aes(y = y)) +
    geom_line(aes(y = prediction$location), colour = "green", size = 2)+
    geom_line(aes(y = prediction$location + sd * prediction$scale ), colour = "blue", size = 1)+
    geom_line(aes(y = prediction$location - sd * prediction$scale ), colour = "blue", size = 1)+
    ylab("dependent variable")+
    xlab("explaining variable")
}
print.spl(prediction)

m$coefficients$scale





