# computes mean of parameters over all simulations 
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
# two vectors beta and gamma the true spline parameters or the true (spline) mean values
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
  betares = matrix(0, nrow = len, ncol = n)
  gammares = matrix(0, nrow = len, ncol = n)
  
  for (i in 1:n)
  {
    betares[,i] = result[[i]]$value[,j]
    gammares[,i] = result[[i]]$value[,j + 1]
  }
  
  if (parameter == T)
  {
    helper1 = matrix(0, nrow = len, ncol = n)
    helper2 = matrix(0, nrow = len, ncol = n)
    for (i in 1:n)
    {
      helper1[,i] = (betares[,i] - meanbeta)^2
      helper2[,i] = (gammares[,i] - meangamma)^2
    }
  }
  
  else if (parameter == F)
  {
    helper1 = matrix(0, nrow = length(x), ncol = n)
    helper2 = matrix(0, nrow = length(x), ncol = n)
    
    predall = list(loc = matrix(0, nrow = length(x),ncol = n), 
                   scale =matrix(0, nrow = length(x),ncol = n))
    # do predictions for every simulation
    for (i in 1:n)
    {
      temp = predict_simulation(betares[,i], gammares[,i], varlist, x)
      predall[[1]][,i] = temp[[1]]
      predall[[2]][,i] = temp[[2]]
    }
    
    for (i in 1:n)
    {
      helper1[,i] = (predall[[1]][,i] - pred[[1]])^2
      helper2[,i] = (predall[[2]][,i] - pred[[2]])^2
    }
    
  }
  
  SEloc = sqrt(rowSums(helper1) / (n * (n -1)))
  SEscale = sqrt(rowSums(helper2) / (n * (n -1)))
  
  
  return(list(location = data.frame(bias = biasloc, SE = SEloc),
              scale = data.frame(bias = biasscale, SE = SEscale)))
}



# same form as in BiasSE
mean_MSE = function(truth, result, MCMC = F, parameter = T, varlist, x = NA)
{
  # chooses appropriate columns of result
  j = 1
  if (MCMC == T)
  {
    j = 3
  }
  len = length(result[[1]]$value[,1])
  n = length(result)
  
  # extract values from result object
  betares = matrix(0, nrow = len, ncol = n)
  gammares = matrix(0, nrow = len, ncol = n)
  
  for (i in 1:n)
  {
    betares[,i] = result[[i]]$value[,j]
    gammares[,i] = result[[i]]$value[,j + 1]
  }
  
  meanbeta = findmean(result, j)
  meangamma = findmean(result, j + 1)
  
  
  if (parameter == T)
  {
    MSE = list(loc = matrix(0, nrow = len, ncol = 1), 
               scale = matrix(0, nrow = len, ncol = 1))
    for (i in 1:n)
    {
      MSE$loc = MSE$loc + (truth[[1]] - betares[,i])^2 / n
      MSE$scale = MSE$scale + (truth[[2]] - gammares[,i])^2 / n
    }
  }
  
  else if (parameter == F)
  {
    MSE = list(loc = matrix(0, nrow = length(x), ncol = 1), 
               scale = matrix(0, nrow = length(x), ncol = 1))
    
    predall = list(loc = matrix(0, nrow = length(x),ncol = n), 
                   scale =matrix(0, nrow = length(x),ncol = n))
    # do predictions for every simulation
    for (i in 1:n)
    {
      temp = predict_simulation(betares[,i], gammares[,i], varlist, x)
      predall[[1]][,i] = temp[[1]]
      predall[[2]][,i] = temp[[2]]
    }
    
    for (i in 1:n)
    {
      MSE$loc = MSE$loc + (predall[[1]][,i] - truth[[1]])^2 / n 
      MSE$scale = MSE$scale + (predall[[2]][,i] - truth[[2]])^2 / n
    }
  }

  return(c(colMeans(MSE$loc), colMeans(MSE$scale)))
}


getEstimateValuesFourData = function(resi, simulation, x, MCMC = F)
{
  mcmc = 1 + MCMC * 2
  n = length(resi)
  results = array(data = NA, dim = c( length(x),2, n))
  for (j in 1:n)
  {
    location_coef = resi[[j]]$value[,mcmc]
    scale_coef = resi[[j]]$value[,mcmc + 1]
    predictions = predict_simulation(location_coef,
                                     scale_coef, simulation, x)
    results[ ,1,j] = predictions$location
    results[ ,2,j] = predictions$scale
  }
  return(results)
}
# spline_values        return der getEstimatesSplines funktion
# quantile             Welche Quantilswerte

# return               für jede simulation quantile der vorhersagen.
getQuantiles = function(spline_values, quantile = c(0.025, 0.975))
{
  
  #dier erste dimension geht wegen dem subsetting verloren
  quantiles = apply(X = spline_values, FUN = quantile, 
                    c(1,2), quantile)
  
  return(quantiles)
}

# est_mean    an object which was created by predict_simulation
#est_quant    an object which was created by get quantiles c(lower,upper) or from estimate_quantile_splines
#x            the values which you want to plot
plot_simulation3 = function(est_mean, est_quant, x ,ylim = c(-25,20))
{
  data = data.frame(x = x, loc_mean = est_mean$location, 
                    loc_quant_lower = est_quant[1,,1],loc_quant_upper = est_quant[2,,1],
                    sc_mean = est_mean$scale,
                    sc_qu_low = est_quant[1,,2], sc_qu_upper = est_quant[2,,2])
  ggplot(data, aes(x = x)) + geom_line(aes(y = loc_mean))+
    geom_ribbon(aes(ymin=loc_quant_lower,ymax=loc_quant_upper),alpha=0.3)+
    geom_line(aes(y= loc_mean + 1.96 * sc_mean))+
    geom_line(aes(y= loc_mean - 1.96 * sc_mean))+
    geom_ribbon(aes(ymin=loc_mean - 1.96 * sc_qu_upper,ymax=loc_mean - 1.96 *sc_qu_low )
                ,alpha=0.3)+
    geom_ribbon(aes(ymin=loc_mean + 1.96 * sc_qu_low,ymax=loc_mean + 1.96 *sc_qu_upper )
                ,alpha=0.3)+ xlim(min(x),max(x)) + ylim(ylim[1],ylim[2])+
    labs(x = "predictor" , y = "mean and credible intevalls")
}
