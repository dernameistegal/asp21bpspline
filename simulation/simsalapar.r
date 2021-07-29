require(simsalapar)
require(asp21bpspline)


beta = read.csv("simulation/beta_sim1")
gamma = read.csv("simulation/gamma_sim1")

simulation1 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 10),
  n = list(type = "frozen", value = 1000),
  its = list(type = "frozen", value = 1000),
  beta = list(type = "frozen", expr = quote(beta), value = beta),
  gamma = list(type = "frozen", expr = quote(beta), value = gamma),
  knots = list(type = "frozen", value = 14),
  order = list(type = "grid", value = c(3,2)),
  p_order = list(type = "frozen", value = 0),
  smooth =  list(type = "frozen", value = 0)
  )

toLatex(simulation1)
mkGrid(simulation1)

doOne = function(n, its, beta, gamma, knots, order, p_order, smooth)
{
  x = runif(n, 0, 10)
  y = x + rnorm(n)
  m = lmls(y~x, scale = ~x, light = F)
  model = spline(m, kn = c(knots, knots), order = c(order, order), 
                 p_order = c(p_order, p_order), smooth = c(smooth, smooth))
  m$coefficients$location = beta
  m$coefficients$scale = gamma
  y = predict(m, model$X, model$X)
  dloc = sum((model$coefficients$location - beta)^2)
  dscale = sum((model$coefficients$scale - gamma)^2)
  ls = list(loc = dloc, scale = dscale)
  return(ls)
}

#doOne(1000, its = 1, beta, gamma, c(15,15), c(3,3), c(0,0), c(1,1))

res = doLapply(simulation1, sfile = "simulation/simtest", doOne = doOne)

