require(asp21bpspline)


### Only for usage in Simulation 1, 1.2 and 3 ### 

# done
predict_simulation = function(beta, gamma, knots, order, x)
{
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
  return(pred)
}


beta = read.csv("simulation/simulation 1/beta_sim1")
gamma = read.csv("simulation/simulation 1/gamma_sim1")

  n  = 1000
  knots = c(20, 20)
  order = c(3, 3)
  p_order = c(3,3)

  x = runif(n, 0, 20)
  x = sort(x)
  
  pred = predict_simulation(beta, gamma, c(knots, knots), order, x)
  y = pred$location + rnorm(n, 0, pred$scale)
  m1 = list(x = x, z = x, y = y)
  
  
  
alpha = seq(0, 100, length.out = 2)

# do gridsearch before doing the simulation study separately for both models
gridsearch = function(alpha, knots)
{
  len = length(alpha)
  models = array(list(matrix), dim = c(len, len))
  # location
  for (i in 1:len)
  {
    # scale
    for (j in 1:len)
    {
      temp = spline(m1, knots, order = order, p_order = p_order, 
                            smooth = c(i, j))
      models[[i, j]] = list(temp$coefficients$location, temp$coefficients$scale)
    }
  }
  return(models)
}
gridsearch(alpha, knots)
