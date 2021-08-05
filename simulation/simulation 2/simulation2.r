source("simulation/simulation 2/simulation2_functions.R")
require(simsalapar)
require(asp21bpspline)

simulation2 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 5),
  n = list(type = "grid", value = c(1000)),
  it = list(type = "frozen", value = 1500),
  knots = list(type = "frozen", value = c(40,40)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(3,3)),
  smooth =  list(type = "frozen", value = c(0,0)))


doOne1 = function(n,it, knots, order, p_order, smooth)
{ 
  data <- rsimu2.1data(n)
  m1 = list(x = data$x, z = data$x, y = data$y)
  
  # estimate parameters
  model = spline(m = m1, kn = knots, order = order, p_order = p_order, smooth = smooth)
  loc = model$coefficients$location 
  scale = model$coefficients$scale 
  
  model = mcmc.spline(model, it = it, burning = 500, thinning = 10)
  locmcmc = colMeans(model$beta)
  scalemcmc = colMeans(model$gamma)
  return(cbind(loc, scale, locmcmc, scalemcmc))
}

doOne2 = function(n,it, knots, order, p_order, smooth)
{ 
  data <- rsimu2.2data(n)
  m1 = list(x = data$x, z = data$x, y = data$y)
  
  # estimate parameters
  model = spline(m = m1, kn = knots, order = order, p_order = p_order, smooth = smooth)
  loc = model$coefficients$location 
  scale = model$coefficients$scale 
  
  model = mcmc.spline(model, it = it, burning = 500, thinning = 10)
  locmcmc = colMeans(model$beta)
  scalemcmc = colMeans(model$gamma)
  return(cbind(loc, scale, locmcmc, scalemcmc))
}

doOne3 = function(n,it, knots, order, p_order, smooth)
{ 
  data <- rsimu2.3data(n)
  m1 = list(x = data$x, z = data$x, y = data$y)
  
  # estimate parameters
  model = spline(m = m1, kn = knots, order = order, p_order = p_order, smooth = smooth)
  loc = model$coefficients$location 
  scale = model$coefficients$scale 
  
  model = mcmc.spline(model, it = it, burning = 500, thinning = 10)
  locmcmc = colMeans(model$beta)
  scalemcmc = colMeans(model$gamma)
  return(cbind(loc, scale, locmcmc, scalemcmc))
}

doLapply(simulation2, sfile = "simulation/simulation 2/test111", doOne = doOne1, monitor = T)
doLapply(simulation2, sfile = "simulation/simulation 2/test112", doOne = doOne2, monitor = T)
doLapply(simulation2, sfile = "simulation/simulation 2/test114", doOne = doOne3, monitor = T)















