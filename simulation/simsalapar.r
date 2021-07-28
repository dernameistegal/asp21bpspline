require(simsalapar)
require(asp21bpspline)


beta = read.csv("simulation/beta_sim1")
gamma = read.csv("simulation/gamma_sim1")

simulation1 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 10),
  n = list(type = "frozen", value = 1000),
  its = list(type = "frozen", value = 1000),
  beta = list(type = "frozen", value = beta),
  gamma = list(type = "frozen", value = gamma),
  knots = list(type = "frozen", value = 15),
  order = list(type = "grid", value = rbind(c(3, 3), c(2, 2))),
  p_order = list(type = "frozen", value = c(0, 0)),
  smooth =  list(type = "frozen", value = c(1, 1))
  
  )

doOne = function(n, its, beta, gamma, knots, order, p_order, smooth)
{
  x = runif(n, 0, 10)
  y = predict.spline(m)
  m = list(x = x, z = x, y = y)
  model = spline(m, kn = knots, order = order, p_order = p_order, smooth = smooth)
  dloc = sum((model$coefficients$location - beta)^2)
  dscale = sum((model$coefficients$scale - gamma)^2)
  ls = list(loc = dloc, scale = dscale)
  return(ls)
}

doOne(1000, its = 1, beta, gamma, c(15,15), c(3,3), c(0,0), c(1,1))

res = doLapply(simulation1, sfile = "simulation/simtest", doOne = doOne)
