source("simulation/simulation functions_mainfolder.R")

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


# The function expects truth to be a list object with
# two vectors beta and gamma the true spline parameters or the true spline values
# if parameter = F the function expects the same form of truth but for the
# bias the predictions are computed first, also an x needs to be specified
# varlist is the variable list of the simulation

# returns a dataframe with bias and SE for location and scale
biasSE = function(truth, result, MCMC = F, parameter = T, varlist, x = NA)
{
  # chooses appropriate columns of result
  j = 1
  if (MCMC == T)
  {
    j = 3
  }
  len = length(result[[1]]$value[,1])
  n = length(result)
  
  
  meanbeta = findmean(result, j)
  meangamma = findmean(result, j + 1)
  
  # Estimation of bias
  if (parameter == T)
  {
    biasloc = meanbeta - truth[[1]]
    biasscale = meangamma - truth[[2]]
  }
  else if (parameter == F)
  {
    pred = predict_simulation(meanbeta, meangamma, varlist, x)
    biasloc = pred[[1]] - truth[[1]]
    biasscale = pred[[2]] - truth[[2]]
  }
  
  
  # Estimation of Standard Error
  helper1 = matrix(0, nrow = len, ncol = n)
  helper2 = matrix(0, nrow = len, ncol = n)
  
  betares = matrix(0, nrow = len, ncol = n)
  gammares = matrix(0, nrow = len, ncol = n)
  
  for (i in 1:n)
  {
    betares[,i] = result[[i]]$value[,j]
    gammares[,i] = result[[i]]$value[,j + 1]
  }
  
  if (parameter == T)
  {
    for (i in 1:n)
    {
      helper1[,i] = (betares[,i] - meanbeta)^2
      helper2[,i] = (gammares[,i] - meangamma)^2
    }
  }
  
  else if (parameter == F)
  {
    # do predictions for every simulation
    for (i in 1:n)
    {
      temp = predict_simulation(betares[,i], gammares[,i], varlist, x)
    }
    
    for (i in 1:n)
    {
      helper1[,i] = (temp[[1]][,i] - pred[[1]])^2
      helper2[,i] = (temp[[2]][,i] - pred[[2]])^2
    }
    
  }
  
  SEloc = sqrt(rowSums(helper1) / (n * n -1))
  SEscale = sqrt(rowSums(helper2) / (n * n -1))
  
  
  return(list(location = data.frame(bias = biasloc, SE = SEloc),
              scale = data.frame(bias = biasscale, SE = SEscale)))
}



# uses list form of result of simulation study
mean_MSE = function(beta, gamma, n, result, seq, knots, order, MCMC = F)
{
  # chooses appropriate columns of result
  j = 1
  if (MCMC == T)
  {
    j = 3
  }
  len = length(result[[1]]$value[,1])
  
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



