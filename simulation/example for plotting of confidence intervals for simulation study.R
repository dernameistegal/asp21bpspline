source("simulation/simulation functions.R")


library(simsalapar)
res20 = maybeRead("simulation/simulation 2/t_4")
val = getArray(res20)

simulation2 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 5),
  n = list(type = "grid", value = c(1000)),
  it = list(type = "frozen", value = 1000),
  knots = list(type = "frozen", value = c(50,50)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(3,3)),
  smooth =  list(type = "frozen", value = c(0,0)))

x = seq(0,20, length.out = 100)
est = getEstimateSplines(val, simulation2, x)

quant = getQuantiles(est)

plot(x, quant[1,1,], type = "l")
lines(x, quant[2,1,])
