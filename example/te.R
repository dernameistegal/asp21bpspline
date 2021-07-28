require(lmls)
require(asp21bpspline)

set.seed(1)
n = 1000
x = seq(0,20, length.out = n)
y =  rnorm(n,x, sin(x) + 2)
m1 = lmls(y~x, scale = ~x, light = F)
m = spline(m1, c(50,50), order = c(2,2), p_order = c(2,2), smooth = c(1,1))

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

