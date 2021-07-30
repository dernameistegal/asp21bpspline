require(asp21bpspline)

set.seed(1)
n = 1000
x = seq(0,20, length.out = n)
mean = -0.04*x^2 + 0.5* x 
y =  mean + rnorm(n, 0, 0.05 *x + 0.3)

plot(x,y)
m1 = list(x = x, z = x, y = y)
m = spline(m1, kn = c(10,10), order = c(3,3), p_order = c(2,2), smooth = c(0,0))

plot(m, sd = 1.96)


n = 100

a = Sys.time()
lol = mcmc.spline(m, it = n, burning = 1, thinning = 1)
b = Sys.time()
b-a
class(m) = "mcmcspline"



plot(lol, m)

n = length(lol$tau)
seq1 = seq(1, n, length.out = n)
plot(seq1, lol$gamma[, 2], type = "l")

####second data generating process####
# data generating process and generating data

require(asp21bpspline)

set.seed(1)
n = 1000
x = seq(0,20, length.out = 1500)
y = 5*sin(x) + rnorm(1500, 0, sd = 1 + (sin(x)))

plot(x,y)
m1 = list(x = x, z = x, y = y)
# #so funktioniert es nicht
# m = spline(m1, kn = c(40,40), order = c(3,3), p_order = c(1,1), smooth = c(0,0))
# #so ist es schon viel besser
# m = spline(m1, kn = c(40,40), order = c(3,3), p_order = c(3,3), smooth = c(0,0))
#noch besser
m = spline(m1, kn = c(40,40), order = c(3,3), p_order = c(3,3), smooth = c(0,0))


plot(m, sd = 1.96)
lol = mcmc.spline(m, it = 1200, burning = 200, thinning = 10)


####second data generating process####
# data generating process and generating data
bmean = colMeans(lol$beta)
gmean = colMeans(lol$gamma)


fit.spline = function(beta, gamma, X)
{
  location = X %*% beta
  scale = exp(X %*% gamma)
  return(list(location = location, scale =  scale))
}
pred = fit.spline(bmean, gmean, m$loc$X)

plot(sort(x), pred$location[order(x)], xlim = range(x), ylim = range(y), type = "l", col = "red")
points(x, y, cex = 0.3)
lines(sort(x), (pred$location)[order(x)] + 1.96 * pred$scale[order(x)])
lines(sort(x), (pred$location)[order(x)] - 1.96 * pred$scale[order(x)])














