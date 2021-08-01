require(simsalapar)
require(asp21bpspline)

rsimu2data <- function(n){
  x = seq(0,20, length.out = n)
  y = 5*sin(x) + rnorm(n, 0, sd = 1 + (sin(x)))
  return(list(x = x , y = y))
}

simulation2 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 4),
  n = list(type = "grid", value = 1000),
  it = list(type = "frozen", value = 1200),
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
  
  
  model = mcmc.spline(model, it = it, burning = 200, thinning = 10)
  locmcmc = colMeans(model$beta)
  scalemcmc = colMeans(model$gamma)
  return_stuff = cbind(loc, scale, locmcmc, scalemcmc)
  colnames(return_stuff) = c("loc", "scale", "locmcmc", "scalemcmc")
  return(return_stuff)
}

doOne(n = 500, it = 1200, knots = c(40,40), order = c(3, 3),
     p_order = c(3,3), smooth = c(0,0))


res10 = doClusterApply(simulation2, sfile = "simulation/simulation 2/take_11", doOne = doOne, monitor = T)


doClusterApply()