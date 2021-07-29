# generate spline values for simulation study

require(lmls)
require(asp21bpspline)

set.seed(1)
n = 1000
x = seq(0,20, length.out = n)
mean = -0.04*x^2 + 0.5* x 
y =  mean + rnorm(n, 0, 0.05 *x + 0.3)
m1 = lmls(y~x, scale = ~x, light = F)
m = spline(m1, c(15,15), order = c(3,3), p_order = c(2,2), smooth = c(1,1))

plot(m, sd = 1.96)


beta = round(m$coefficients$location, 3)
gamma = round(m$coefficients$scale, 3)

write.csv(as.vector(beta), file = "simulation/beta_sim1", row.names = F)
write.csv(as.vector(gamma), file = "simulation/gamma_sim1", row.names = F)
