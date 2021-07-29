require(simsalapar)
require(asp21bpspline)


beta = read.csv("simulation/simulation 1/beta_sim1")
gamma = read.csv("simulation/simulation 1/gamma_sim1")

simulation1 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 1000),
  n = list(type = "grid", value = 1000),
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
  y = x + rnorm(n)
  
  #build Designmatrices
  m1 = list(x = x, z = x, y = y)
  m = spline(m1, kn = knots, order = order, p_order = p_order, smooth = smooth)  
  
  # sample from the spline model
  m$coefficients$location = beta
  m$coefficients$scale = gamma
  pred = predict(m, m$loc$X, m$loc$X)
  y = pred$location + rnorm(n, 0, pred$scale)
  m1 = list(x = x, z = x, y = y)
  
  
  # estimate parameters
  model = spline(m1, knots, order = order, p_order = p_order, smooth = smooth)
  loc = model$coefficients$location 
  scale = model$coefficients$scale 
  return(cbind(loc, scale))
}

doOne(n = 1000, beta, gamma, knots = c(15, 15), order = c(3, 3), 
      p_order = c(0,0), smooth = c(0,0))

a = Sys.time()
res10 = doLapply(simulation1, sfile = "simulation/simulation1/take 1", doOne = doOne)
b = Sys.time()
b-a

