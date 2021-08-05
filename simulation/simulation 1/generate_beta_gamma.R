# generate spline values for simulation study

require(lmls)
require(asp21bpspline)

set.seed(2)
n = 100
x = c(runif(100, 0, 20))
mean = -0.04*x^2 + 0.5* x 
y =  mean + rnorm(n, 0, 0.2 *x + 0.3)
m1 = lmls(y~x, scale = ~x, light = F)
m = spline(m1, c(15,15), order = c(3,3), p_order = c(0,0), smooth = c(0,0))

plot(m, sd = 1.96)


beta = round(m$coefficients$location, 3)
gamma = round(m$coefficients$scale, 3)


x = seq(0, 20, length.out = 1000)
temp = predict(m, x, x, isDesignmatrix = F)
m[["fitted.values"]]$location = temp[[1]]
m[["fitted.values"]]$scale = temp[[2]]


require(ggplot2)
sd = 1.96
data = data.frame(x = x,
                  ypred = m[["fitted.values"]]$location,
                  scalepred = m[["fitted.values"]]$scale)
ggplot2::ggplot(data, mapping = aes(x = x)) +
  geom_line(aes(y = ypred), colour = "green", size = 2)+
  geom_line(aes(y = ypred + sd * scalepred), colour = "blue", size = 1)+
  geom_line(aes(y = ypred - sd * scalepred), colour = "blue", size = 1)+
  ylab("dependent variable")+
  xlab("explaining variable")

#write.csv(as.vector(beta), file = "simulation/simulation 1/beta_sim1", row.names = F)
#write.csv(as.vector(gamma), file = "simulation/simulation 1/gamma_sim1", row.names = F)
