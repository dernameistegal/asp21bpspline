require(simsalapar)
require(asp21bpspline)


rsimu2data <- function(n){
  x = seq(0,20, length.out = n)
  mean = -0.0004*x^4 + 0.005* x^3 - 0.05*x^2 + 2*x + 6*sin(x)
  y =  mean + rnorm(n, 0, 0.5*(1 + x - (1/20) * x^2 + abs(3 - 1/10 * x)))
  return(list(x = x , y = y))
}

simulation2 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 10),
  n = list(type = "grid", value = 500),
  it = list(type = "frozen", value = 100),
  knots = list(type = "frozen", value = c(20,20)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(2,2)),
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

doOne(n = 100, it = 1000, knots = c(15, 15), order = c(3, 3),
      p_order = c(2,2), smooth = c(0,0))


res10 = doLapply(simulation2, sfile = "simulation/simulation 2/take_22222", doOne = doOne, monitor = T)


