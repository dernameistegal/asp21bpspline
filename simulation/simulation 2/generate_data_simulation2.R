# generate observations out of an observation

require(lmls)
require(asp21bpspline)
set.seed(1)
n = 1000

rsimu2data <- function(n){
  x = seq(0,20, length.out = n)
  mean = -0.0004*x^4 + 0.005* x^3 - 0.05*x^2 + 2*x + 6*sin(x)
  y =  mean + rnorm(n, 0, 0.5*(1 + x - (1/20) * x^2 + abs(3 - 1/10 * x)))
  return(list(x = x , y = y))
}
data <- rsimu2data(1000)

x <- data[["x"]]
y <- data[["y"]]
plot(x,y)


m1 = lmls(y~x, scale = ~x, light = F)
m = spline(m1, c(15,15), order = c(3,3), p_order = c(2,2), smooth = c(1,1))

plot(m, sd = 1.96)
plot(x,y)


# beta = round(m$coefficients$location, 3)
# gamma = round(m$coefficients$scale, 3)
# 
# write.csv(as.vector(beta), file = "simulation/beta_sim1", row.names = F)
# write.csv(as.vector(gamma), file = "simulation/gamma_sim1", row.names = F)
