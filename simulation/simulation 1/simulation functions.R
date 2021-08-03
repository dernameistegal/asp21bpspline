require(simsalapar)

predict_simulation = function(beta, knots, order, x)
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



# uses list form of results of simulation study
biasSE_parameters = function(beta, gamma, n, results)
{
  
  
  len = length(results[[1]]$value[,1])
  
  betares = matrix(0, nrow = len, ncol = n)
  gammares = matrix(0, nrow = len, ncol = n)
  
  for (i in 1:n)
  {
    betares[,i] = res10[[i]]$value[,1]
    gammares[,i] = res10[[i]]$value[,2]
  }
  meanbeta = rowSums(betares) / n
  meangamma = rowSums(gammares) / n
  
  # Estimation of bias
  biasbeta = meanbeta - beta
  biasgamma = meangamma - gamma
  
  
  
  # Estimation of Standard Error
  helper1 = matrix(0, nrow = len, ncol = n)
  helper2 = matrix(0, nrow = len, ncol = n)
  
  for (i in 1:n)
  {
    helper1[,i] = (betares[,i] - meanbeta)^2
    helper2[,i] = (gammares[,i] - meangamma)^2
  }
  SEbeta = sqrt(rowSums(helper1) / (n * n -1))
  SEgamma = sqrt(rowSums(helper2) / (n * n -1))
  
  
  return(list(beta = data.frame(bias = biasbeta, SE = SEbeta),
              gamma = data.frame(bias = biasgamma, SE = SEgamma)))
}

