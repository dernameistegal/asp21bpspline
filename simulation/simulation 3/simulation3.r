require(simsalapar)
require(asp21bpspline)
source("simulation/simulation 1/simulation1_functions.R")

beta = read.csv("simulation/simulation 1/beta_sim1")
gamma = read.csv("simulation/simulation 1/gamma_sim1")

simulation3 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 4),
  init = list(type = "grid", value = c("normal", "random" ,"error")),
  n = list(type = "frozen", value = 1000),
  it = list(type = "frozen", value = 1500),
  beta = list(type = "frozen", expr = quote(beta), value = beta),
  gamma = list(type = "frozen", expr = quote(gamma), value = gamma),
  knots = list(type = "frozen", value = c(15, 15)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(0,0)),
  smooth =  list(type = "frozen", value = c(0,0)),
  burning =  list(type = "frozen", value = 500),
  thinning =  list(type = "frozen", value = 10))
set.seed(1633)
doOne = function(init, n, beta, gamma, it, knots, order, p_order, smooth, burning,
                 thinning)
{ 
  # generate sample
  x = runif(n, 0, 10)
  x = sort(x)
  
  pred = predict_simulation(beta, gamma, knots, order, x)
  y = pred$location + rnorm(n, 0, pred$scale)
  m1 = list(x = x, z = x, y = y)
  
  
  # estimate ML parameters
  model = spline(m1, knots, order = order, p_order = p_order, smooth = smooth)
  loc = model$coefficients$location 
  scale = model$coefficients$scale 
  
  #randomize coefficents
  if (init == "normal"){
    #do nothing
  }
  else if(init == "random") {
    model$coefficients$location = rnorm(length(loc),0,1)
    model$coefficients$scale =  rnorm(length(loc),0,1)
  }
  else if(init == "error") {
  model$coefficients$location = loc + rnorm(length(loc),0,1)
  model$coefficients$scale = scale + rnorm(length(loc),0,1)
  }

  # estimate mcmc parameters
  model = mcmc.spline(model, it = it, burning = burning, thinning = thinning)
  
  
  return_stuff = cbind(loc, scale, t(model$beta), t(model$gamma))
  colnames(return_stuff) = c("loc", "scale", 
                             rep("locmcmc",nrow(model$beta)),
                             rep("scalemcmc",nrow(model$beta)))
  
  
  return(return_stuff)
}

 # doOne(init = "normal", n = 1000, beta, gamma, it = 1500, knots = c(15, 15), order = c(3, 3), 
 #       p_order = c(0,0), smooth = c(0,0), burning = 500, thinning = 10)

res3 = doLapply(simulation3, sfile = "simulation/simulation 3/simulation3_test1"
                , monitor = T, doOne = doOne)
