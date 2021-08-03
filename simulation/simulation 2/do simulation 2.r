require(simsalapar)
require(asp21bpspline)


loc_sim2 = function(x) {
  return(-0.0004*x^4 + 0.005* x^3 - 0.05*x^2 + 2*x + 4*sin(x))
}

scale_sim2 = function(x) {
  return(2.1 + 2*sin(x) + x^2/200)
}
  
rsimu2data <- function(n){
  x = runif(n,0,20)
  mean = loc_sim2(x)
  scale = scale_sim2(x)
  y =  mean + rnorm(n, sd = scale)
  return(list(x = x , y = y))
}


simulation2 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 50),
  n = list(type = "grid", value = c(250,500,1000)),
  it = list(type = "frozen", value = 1500),
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
  #locmcmc = colMeans(model$beta)
  #scalemcmc = colMeans(model$gamma)
  # return_stuff = cbind(loc, scale, locmcmc, scalemcmc)
  # colnames(return_stuff) = c("loc", "scale", "locmcmc", "scalemcmc")
  return_stuff = cbind(loc, scale, t(model$beta), t(model$gamma))
  colnames(return_stuff) = c("loc", "scale", rep("locmcmc",nrow(model$beta)),rep("scalemcmc",nrow(model$beta)))
  return(return_stuff)
}
# 
# doOne(n = 100, it = 1000, knots = c(15, 15), order = c(3, 3),
#       p_order = c(2,2), smooth = c(0,0))


res10 = doLapply(simulation2, sfile = "simulation/simulation 2/simulation2_test1", doOne = doOne, monitor = T)


