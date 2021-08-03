require(simsalapar)

plot_simulation = function(predictions, x)
{
  sd = 1.96
  data = data.frame(x = x,
                    ypred = predictions[[1]],
                    scalepred = predictions[[2]])
  ggplot2::ggplot(data, mapping = aes(x = x)) +
    geom_line(aes(y = ypred), colour = "green", size = 2)+
    geom_line(aes(y = ypred + sd * scalepred), colour = "blue", size = 1)+
    geom_line(aes(y = ypred - sd * scalepred), colour = "blue", size = 1)+
    ylab("dependent variable")+
    xlab("explaining variable")
}



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


findmean = function(len, n, res, j)
{
  vector = matrix(0, nrow = len, ncol = n)
  
  for (i in 1:n)
  {
    vector[,i] = res[[i]]$value[,j]
  }
  mean = rowSums(vector) / n
  return(mean)
}


# uses list form of results of simulation study
biasSE_parameters = function(beta, gamma, n, results, MCMC = F)
{
  # chooses appropriate columns of results
  j = 1
  if (MCMC == T)
  {
    j = 3
  }
  len = length(results[[1]]$value[,1])
  
  meanbeta = findmean(len, n, results, j)
  meangamma = findmean(len, n, results, j + 1)
  
  # Estimation of bias
  biasbeta = meanbeta - beta
  biasgamma = meangamma - gamma
  
  
  
  # Estimation of Standard Error
  helper1 = matrix(0, nrow = len, ncol = n)
  helper2 = matrix(0, nrow = len, ncol = n)
  
  betares = matrix(0, nrow = len, ncol = n)
  gammares = matrix(0, nrow = len, ncol = n)
  
  for (i in 1:n)
  {
    betares[,i] = res10[[i]]$value[,j]
    gammares[,i] = res10[[i]]$value[,j + 1]
  }
  
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


# uses list form of results of simulation study
biasSE_predictions = function(beta, gamma, n, results, seq, MCMC = F)
{
  # chooses appropriate columns of results
  j = 1
  if (MCMC == T)
  {
    j = 3
  }
  len = length(results[[1]]$value[,1])
  
  betares = matrix(0, nrow = len, ncol = n)
  gammares = matrix(0, nrow = len, ncol = n)
  
  for (i in 1:n)
  {
    betares[,i] = res10[[i]]$value[,j]
    gammares[,i] = res10[[i]]$value[,j + 1]
  }
  
  # do predictions for result
  for (i in 1:n)
  {
    predict_simulation(betares[,i], gammares[,i], knots, order)
  }
 
  
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

