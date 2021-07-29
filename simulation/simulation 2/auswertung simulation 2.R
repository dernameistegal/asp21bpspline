library(simsalapar)

simulation1 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 1000),
  n = list(type = "grid", value = 1000),
  beta = list(type = "frozen", expr = quote(beta), value = beta),
  gamma = list(type = "frozen", expr = quote(gamma), value = gamma),
  knots = list(type = "frozen", value = c(15, 15)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(0,0)),
  smooth =  list(type = "frozen", value = c(0,0)))



res10 = maybeRead("simulation/simulation 1/take_1")
beta = read.csv("simulation/simulation 1/beta_sim1")
gamma = read.csv("simulation/simulation 1/gamma_sim1")

n = get.n.sim(simulation1)

betares = matrix(0, nrow = 17, ncol = n)
gammares = matrix(0, nrow = 17, ncol = n)
for (i in 1:n)
{
  betares[,i] = res10[[i]]$value[,1]
  gammares[,i] = res10[[i]]$value[,2]
}
meanbeta = rowSums(betares) / n
meangamma = rowSums(gammares) / n

# Estimation of bias
biasbeta = meanbeta - beta
biasgamma = meangamma - gamma


# Estimation of SE of bias

sup = matrix(0, nrow = 17, ncol = n)
sop = matrix(0, nrow = 17, ncol = n)

for (i in 1:n)
{
 sup[,i] = (betares[,i] - meanbeta)^2
 sop[,i] = (gammares[,i] - meangamma)^2
}
SEbeta = sqrt(rowSums(sup) / (n * n -1))
SEgamma = sqrt(rowSums(sop) / (n * n -1))

data.frame(bias = biasbeta, SE = SEbeta)
data.frame(bias = biasgamma, SE = SEgamma)
colMeans(biasbeta)
colMeans(biasgamma)
mean(SEbeta)
mean(SEgamma)
##### ab hier hat Valentin was gemacht#####
m = list()
class(m) = "spline"
m$coefficients$location = meanbeta
m$coefficients$scale = meangamma
m$loc$knots = simulation1[["knots"]]$value[1]
m$loc$order = simulation1[["order"]]$value[1]
m$scale$knots= simulation1[["knots"]]$value[2]
m$scale$order = simulation1[["order"]]$value[2]
x <- seq(0,20, length.out = 1000)
spline_Werte <- predict(m, x, x)

# wie weit liegen die Werte von den wahren Werten weg?
plot(x,type = "l", -0.04*x^2 + 0.5* x, col = "green")
lines(x,type = "l", -0.04*x^2 + 0.5* x + 2*(0.05 *x + 0.3), col = "blue")
lines(x,type = "l", -0.04*x^2 + 0.5* x - 2*(0.05 *x + 0.3), col = "blue")
lines(x, spline_Werte[[1]])
lines(x, spline_Werte[[1]] + 2 * spline_Werte[[2]], col = "red")
lines(x, spline_Werte[[1]] - 2 * spline_Werte[[2]], col = "red")
