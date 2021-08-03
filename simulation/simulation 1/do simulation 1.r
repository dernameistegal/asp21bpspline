require(simsalapar)
require(asp21bpspline)
source("simulation/simulation 1/simulation functions.R")

beta = read.csv("simulation/simulation 1/beta_sim1")
gamma = read.csv("simulation/simulation 1/gamma_sim1")

simulation1 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 10),
  n = list(type = "grid", value = 1000),
  it = list(type = "frozen", value = 1000),
  beta = list(type = "frozen", expr = quote(beta), value = beta),
  gamma = list(type = "frozen", expr = quote(gamma), value = gamma),
  knots = list(type = "frozen", value = c(15, 15)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(0,0)),
  smooth =  list(type = "frozen", value = c(0,0)))

doOne = function(n, beta, gamma, knots, order, p_order, smooth)
{ 
  # generate sample
  x = runif(n, 0, 10)
  x = sort(x)
  
  pred = predict_simulation(beta, knots, order, x)
  y = pred$location + rnorm(n, 0, pred$scale)
  m1 = list(x = x, z = x, y = y)
  
  
  # estimate parameters
  model = spline(m1, knots, order = order, p_order = p_order, smooth = smooth)
  loc = model$coefficients$location 
  scale = model$coefficients$scale 
  model = mcmc.spline(model, it = it, burning = 1, thinning = 1)
  locmcmc = colMeans(model$beta)
  scalemcmc = colMeans(model$gamma)
  return(cbind(loc, scale))
}

doOne(n = 1000, beta, gamma, knots = c(15, 15), order = c(3, 3), 
      p_order = c(0,0), smooth = c(0,0))

a = Sys.time()
res10 = doLapply(simulation1, doOne = doOne)
b = Sys.time()
b-a

