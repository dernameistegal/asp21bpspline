source("simulation/simulation functions_mainfolder")

# done 
findmean = function(result, j)
{
  n = length(result)
  len = length(result[[1]]$value[,1])
  vector = matrix(0, nrow = len, ncol = n)
  
  for (i in 1:n)
  {
    vector[,i] = result[[i]]$value[,j]
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
    betares[,i] = results[[i]]$value[,j]
    gammares[,i] = results[[i]]$value[,j + 1]
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
MSE_predictions = function(beta, gamma, n, results, seq, knots, order, MCMC = F)
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
  
  
  # find true mean and scale values
  truth = predict_simulation(beta, gamma, knots, order, seq)
  truth = cbind(truth[[1]], truth[[2]])
  
  pred = array(0, dim = c(length(seq), 2, n))
  
  # do predictions for result
  for (i in 1:n)
  {
    temp =predict_simulation(betares[,i], gammares[,i], knots, order, seq)
    pred[,,i] = cbind(temp[[1]], temp[[2]])
  }
  
  deviation = array(0, dim = c(length(seq), 2, n))
  # MSE 
  for (i in 1:n)
  {
    deviation[,,i] = (pred[,,i] - truth)^2
  }
  
  deviation = apply(deviation, FUN = mean, MARGIN = c(1,2)) / n
  return(deviation)
}



# uses list form of results of simulation study
bias_predictions = function(beta, gamma, n, results, seq, knots, order, MCMC = F)
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
  
  
  # find true mean and scale values
  truth = predict_simulation(beta, gamma, knots, order, seq)
  truth = cbind(truth[[1]], truth[[2]])
  
  pred = array(0, dim = c(length(seq), 2, n))
  
  # do predictions for result
  for (i in 1:n)
  {
    temp =predict_simulation(betares[,i], gammares[,i], knots, order, seq)
    pred[,,i] = cbind(temp[[1]], temp[[2]])
  }
  
  deviation = array(0, dim = c(length(seq), 2, n))
  
  # MSE 
  for (i in 1:n)
  {
    deviation[,,i] = (pred[,,i] - truth)
  }
  
  deviation = apply(deviation, FUN = mean, MARGIN = c(1,2)) / n
  
  
  # Estimation of Standard Error
  meanpred = apply(pred, FUN = mean, MARGIN = c(1,2))
  
  
  helper1 = matrix(0, nrow = length(seq), ncol = n)
  helper2 = matrix(0, nrow = length(seq), ncol = n)
  for (i in 1:n)
  {
    helper1[,i] = (pred[,1,i] - meanpred[,1])^2
    helper2[,i] = (pred[,2,i] - meanpred[,2])^2
  }
  SEbeta = sqrt(rowSums(helper1) / (n * n -1))
  SEgamma = sqrt(rowSums(helper2) / (n * n -1))
  
  
  return(list(location = data.frame(bias = deviation[,1], SE = SEbeta),
              scale = data.frame(bias = deviation[,2], SE = SEgamma)))
}



