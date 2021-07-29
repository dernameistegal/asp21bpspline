require(lmls)
require(asp21bpspline)

set.seed(1)
n = 100
x = seq(0,20, length.out = n)
mean = -0.04*x^2 + 0.5* x 
y =  mean + rnorm(n, 0, 0.05 *x + 0.3)


m1 = list(x = x, z = x, y = y)
m = spline(m1, c(15,15), order = c(3,3), p_order = c(0,0), smooth = c(0,0))

plot(m, sd = 1.96)


n = 1000

a = Sys.time()
lol = mcmc.spline(m, it = n, burning = 1, thinning = 1)
b = Sys.time()
b-a

summary(lol$tau)
summary(lol$epsilon)

hist(lol$epsilon, breaks = 50)
hist(lol$tau, breaks = 50)
plot.mcmcspline(m, lol)

n = length(lol$tau)
seq1 = seq(1, n, length.out = n)
plot(seq1, lol$gamma[, 4], type = "l")

