require(asp21bpspline)

set.seed(1)
n = 1000
x = seq(0,20, length.out = n)
mean = -0.04*x^2 + 0.5* x 
y =  mean + rnorm(n, 0, 0.05 *x + 0.3)

plot(x,y)
m1 = list(x = x, z = x, y = y)
m = spline(m1, kn = c(50,50), order = c(3,3), p_order = c(2,2), smooth = c(0,0))

plot(m, sd = 1.96)


n = 200

a = Sys.time()
lol = mcmc.spline(m, it = n, burning = 100, thinning = 10, taustart = 1)
b = Sys.time()
b-a


plot(lol, m)

n = length(lol$tau)
seq1 = seq(1, n, length.out = n)
plot(seq1, lol$gamma[, 2], type = "l")

####second data generating process####
# data generating process and generating data

require(asp21bpspline)

set.seed(1)
n = 1000
x = seq(0,20, length.out = n)
y = 5*sin(x) + rnorm(n, 0, sd = 1 + (sin(x)))

plot(x,y)
m1 = list(x = x, z = x, y = y)
# #so funktioniert es nicht
# m = spline(m1, kn = c(40,40), order = c(3,3), p_order = c(1,1), smooth = c(0,0))
# #so ist es schon viel besser
# m = spline(m1, kn = c(40,40), order = c(3,3), p_order = c(3,3), smooth = c(0,0))
#noch besser
m = spline(m1, kn = c(40,40), order = c(3,3), p_order = c(3, 3), smooth = c(0,0))


plot(m, sd = 1.96)
lol = mcmc.spline(m, it = 1200, burning = 200, thinning = 10)

plot(lol, m)

##############################################

require(asp21bpspline)

set.seed(1)
n = 1500
x = seq(0,20, length.out = n)
mean = -0.0004*x^4 + 0.005* x^3 - 0.05*x^2 + 2*x + 6*sin(x)
y =  mean + rnorm(n, 0, 0.5*(1 + x - (1/20) * x^2 + abs(3 - 1/10 * x)))

plot(x,y)
m1 = list(x = x, z = x, y = y)
# #so funktioniert es nicht
# m = spline(m1, kn = c(40,40), order = c(3,3), p_order = c(1,1), smooth = c(0,0))
# #so ist es schon viel besser
# m = spline(m1, kn = c(40,40), order = c(3,3), p_order = c(3,3), smooth = c(0,0))
#noch besser
m = spline(m1, kn = c(40,40), order = c(3,3), p_order = c(3, 3), smooth = c(0,0))


plot(m, sd = 1.96)
lol = mcmc.spline(m, it = 1200, burning = 200, thinning = 10)

plot(lol, m)
###################################################################################################################

library(lmls)
####first data generating process (easy to fit with low order splines)####
# data generating process and generating data
set.seed(10)
x <- runif(1000, 0 , 10)
e <- rnorm(1000,sd = (3*sin(x) + 0.1*x^2- 0.01*x^3+ 0.0005* x^4)/3+3)
y <- 3*sin(x) + 0.1*x^2- 0.01*x^3+ 0.001* x^4 + e

# fit spline and run mcmc
m = lmls(y ~ x, scale = ~x, light = FALSE)
m_spline = spline(m, kn = c(15,15), order = c(3,3), p_order = c(2,2), smooth = c(0,0))
plot(m_spline)
mcmc_m_spline = mcmc.spline(m_spline, it = 500, burning = 100, thinning = 10)

#plot beta and gamma components against time
layout(matrix(c(1,2), 1, 2))
ind = seq(1, length(mcmc_m_spline$beta[,1]))
beta_first_component = mcmc_m_spline$beta[,1]
plot(ind, beta_first_component, type = "l")

ind = seq(1, length(mcmc_m_spline$gamma[,1]))
gamma_first_component = mcmc_m_spline$gamma[,1]
plot(ind, gamma_first_component, type = "l")
layout(1)

####second data generating process####
# data generating process and generating data
set.seed(10)
x = seq(0,20, length.out = 500)
y = 5*sin(x) + rnorm(500, 0,sd =1 + (sin(x)))

# fit spline and run mcmc
m = lmls(y ~ x, scale = ~x, light = FALSE)
m_spline = spline(m, kn = c(50,50), order = c(3,3), p_order = c(3,3), smooth = c(0,0))
plot(m_spline)
mcmc_m_spline = mcmc.spline(m_spline, it = 1500, burning = 400, thinning = 10)

#plot beta and gamma components against time
layout(matrix(c(1,2), 1, 2))
ind = seq(1, length(mcmc_m_spline$beta[,1]))
beta_first_component = mcmc_m_spline$beta[,1]
plot(ind, beta_first_component, type = "l")

ind = seq(1, length(mcmc_m_spline$gamma[,1]))
gamma_first_component = mcmc_m_spline$gamma[,1]
plot(ind, gamma_first_component, type = "l")
layout(1)

length(unique(gamma_first_component))/length(gamma_first_component)

####third data generating process (large step size?)####
# data generating process and generating data
set.seed(10)
#x = seq(0,20, length.out = 600)
x = sort(runif(500, 0,20))
y =  (x-10) * rnorm(500,0, 1) + x + (x-10)^2/100 *  rnorm(500,0, 1) #da funktioniert es nicht... backward un und forward, sind dann sehr stark voneinander verschieden. Daher wird nie angenommen
#y =  sin(x) + rnorm(500,0, 1) + x

# fit spline and run mcmc
m = lmls(y ~ x, scale = ~x, light = FALSE)
m_spline = spline(m, kn = c(15,15), order = c(3,3), p_order = c(1,1), smooth = c(0,0))
plot(m_spline)
mcmc_m_spline = mcmc.spline(m_spline, it = 1000, burning = 200, thinning = 1)

#plot beta and gamma components against time
layout(matrix(c(1,2), 1, 2))
ind = seq(1, length(mcmc_m_spline$beta[,1]))
beta_first_component = mcmc_m_spline$beta[,1]
plot(ind, beta_first_component, type = "l")

ind = seq(1, length(mcmc_m_spline$gamma[,1]))
gamma_first_component = mcmc_m_spline$gamma[,1]
plot(ind, gamma_first_component, type = "l")
layout(1)









bmean = colMeans(mcmc_m_spline$beta)
gmean = colMeans(mcmc_m_spline$gamma)

fit.spline = function(beta, gamma, X)
{
  location = X %*% beta
  scale = exp(X %*% gamma)
  return(list(location = location, scale =  scale))
}


pred = fit.spline(bmean, gmean, m_spline$loc$X)
plot(sort(x), pred$location[order(x)], xlim = range(x), ylim = range(y), type = "l", col = "red")
points(x, y, cex = 0.3)
lines(sort(x), (pred$location)[order(x)] + 1.96 * pred$scale[order(x)])
lines(sort(x), (pred$location)[order(x)] - 1.96 * pred$scale[order(x)])







