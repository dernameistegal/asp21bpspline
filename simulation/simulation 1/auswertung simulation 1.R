library(simsalapar)

simulation1 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 10),
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

biasSE_parameters(beta, gamma, get.n.sim(simulation1), res10)



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
