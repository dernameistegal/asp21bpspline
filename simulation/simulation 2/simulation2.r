source("simulation/simulation 2/simulation2_functions")
require(simsalapar)
require(asp21bpspline)

simulation2 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 200),
  n = list(type = "grid", value = c(1500)),
  it = list(type = "frozen", value = 2000),
  knots = list(type = "frozen", value = c(40,40)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(3,3)),
  smooth =  list(type = "frozen", value = c(0,0)))


doOne = function(n,it, knots, order, p_order, smooth)
{ 
  data <- rsimu2data(n)
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
# 
# doOne(n = 100, it = 1000, knots = c(15, 15), order = c(3, 3),
#       p_order = c(2,2), smooth = c(0,0))


res10 = doLapply(simulation2, sfile = "simulation/simulation 2/test111", doOne = doOne, monitor = T)

