require(simsalapar)
require(asp21bpspline)
source("simulation/simulation 1/simulation1_functions.R")

beta = read.csv("simulation/simulation 1/beta_sim1")
gamma = read.csv("simulation/simulation 1/gamma_sim1")

simulation1 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 500),
  n = list(type = "grid", value = 1000),
  it = list(type = "frozen", value = 1500),
  beta = list(type = "frozen", expr = quote(beta), value = beta),
  gamma = list(type = "frozen", expr = quote(gamma), value = gamma),
  knots = list(type = "frozen", value = c(15, 15)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(0,0)),
  smooth =  list(type = "frozen", value = c(0,0)),
  burning =  list(type = "frozen", value = 500),
  thinning =  list(type = "frozen", value = 10))

doOne = function(n, beta, gamma, it, knots, order, p_order, smooth, burning,
                 thinning)
{ 
  # generate sample
  x = runif(n, 0, 20)
  x = sort(x)
  
  pred = predict_simulation(beta, gamma, knots, order, x)
  y = pred$location + rnorm(n, 0, pred$scale)
  m1 = list(x = x, z = x, y = y)
  
  
  # estimate ML parameters
  model = spline(m1, knots, order = order, p_order = p_order, smooth = smooth)
  loc = model$coefficients$location 
  scale = model$coefficients$scale 
  
  # estimate mcmc parameters
  model = mcmc.spline(model, it = it, burning = burning, thinning = thinning)
  locmcmc = colMeans(model$beta)
  scalemcmc = colMeans(model$gamma)
  return(cbind(loc, scale, locmcmc, scalemcmc))
}

doOne(n = 1000, beta, gamma, it = 1500, knots = c(15, 15), order = c(3, 3), 
      p_order = c(0,0), smooth = c(0,0), burning = 500, thinning = 10)


set.seed(1)
a = Sys.time()
res10 = doLapply(simulation1, sfile = "simulation/simulation 1/500samples", doOne = doOne)
b = Sys.time()
b-a

