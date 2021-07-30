
# comparing actual predictions instead of parameters


doOne = function(n,it, beta, gamma, knots, order, p_order, smooth)
{ 
  # generate sample
  x = seq(0, 10, length.out = n)
  y = x + rnorm(n)
  
  m = list()
  class(m) = "spline"
  
  # sample from the spline model
  m$coefficients$location = beta
  m$coefficients$scale = gamma
  m$loc$knots = knots[1]
  m$loc$order = order[1]
  m$scale$knots= knots[2]
  m$scale$order = order[1]
  
  pred = predict(m, x, x)
  y = pred$location + rnorm(n, 0, pred$scale)
  m1 = list(x = x, z = x, y = y)
  
  
  # estimate parameters
  model = spline(m1, knots, order = order, p_order = p_order, smooth = smooth)
  pred =  predict(model, model$loc$X, model$loc$X, isDesignmatrix = T)
  
  
  return(cbind(pred$location, pred$scale))
}

doOne(n = 1000, it = 1000, beta, gamma, knots = c(15, 15), order = c(3, 3), 
      p_order = c(0,0), smooth = c(0,0))

a = Sys.time()
res10 = doLapply(simulation1, sfile = "simulation/simulation 1/take_3", doOne = doOne)
b = Sys.time()
b-a



beta = read.csv("simulation/simulation 1/beta_sim1")
gamma = read.csv("simulation/simulation 1/gamma_sim1")

n = get.n.sim(simulation1)

betares = matrix(0, nrow = 1000, ncol = n)
gammares = matrix(0, nrow = 1000, ncol = n)
for (i in 1:n)
{
  betares[,i] = res10[[i]]$value[,1]
  gammares[,i] = res10[[i]]$value[,2]
}
meanbeta = rowSums(betares) / n
meangamma = rowSums(gammares) / n


# actual values
m = list()
class(m) = "spline"

# sample from the spline model
m$coefficients$location = beta
m$coefficients$scale = gamma
m$loc$knots = 15
m$loc$order = 3
m$scale$knots= 15
m$scale$order = 3
x = seq(0, 10, length.out = 1000)
pr


# actual values
m = list()
class(m) = "spline"

# sample from the spline model
m$coefficients$location = beta
m$coefficients$scale = gamma
m$loc$knots = 15
m$loc$order = 3
m$scale$knots= 15
m$scale$order = 3
x = seq(0, 10, length.out = 1000)
pred = predict(m, x, x)



# Estimation of bias
biasbeta = meanbeta - pred$location
biasgamma = meangamma - pred$scale
mean(biasbeta)
mean(biasgamma)
