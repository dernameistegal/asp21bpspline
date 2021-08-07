# Faster Matrix multiplication with C++
`%f*f%` <- function(a, b)
{
  eigenMapMatMult(a, b)
}

# Alternative rmvnorm function to avoid using "solve" 
rmvnorm2 = function(n, mu = 0, chol_sig_inv) 
{
  dim = nrow(chol_sig_inv)
  
  std_norm = matrix(rnorm(dim * n), dim, n)
  scaled = backsolve(chol_sig_inv, std_norm)
  shifted = scaled + mu
  
  return(shifted)
}

# Alternative dmvnorm function to avoid using "solve" 
dmvnorm2 = function(x, mu = 0, chol_sig_inv, log = FALSE) {
  std_norm = drop(chol_sig_inv %*% (x - mu))
  correction = sum(log(diag(chol_sig_inv)))
  
  log_prob = dnorm(std_norm, log = TRUE)
  
  if (is.matrix(log_prob)) 
  {
    log_prob = colSums(log_prob) + correction
  } else 
  {
    log_prob = sum(log_prob) + correction
  }
  
  if (log) 
  {
    return(log_prob)
  } 
  else 
  {
    return(exp(log_prob))
  }
}

# Calculate score of beta
score_beta = function(m)
{
  ups = t(m$residuals$location / m$fitted.values$scale^2) %*% m$loc$X
  return(drop(ups))
}

# Calculate score of gamma
score_gamma = function(m)
{
  ups = (t(m$residuals/ m$fitted.values$scale)^2 - 1) %*% m$scale$Z
  return(drop(ups))
}

# Calculate Fisher information of beta
info_beta = function(m)
{
  crossprod(m$x, diag(as.vector(1/(m$fitted.values$scale^2))) %*% m$loc$X)
}

# Calculate Fisher information of gamma
info_gamma = function(m)
{
  2 * crossprod(m$scale$Z)
}

# Helper function to update gamma in the Fisher scoring algorithm
set_gamma = function(m, gamma)
{
  m$coefficients$scale = gamma
  m$fitted.values$scale = exp(m$scale$Z %*% gamma)
  return(m)
}

# Function for quantile finding in summary for mcmcspline
getEstimateValues2 = function(m)
{
  reslist = cbind(t(m$beta),t(m$gamma))
  # ermittle die anzahl der für mcmc relevanten Spalten
  iteration = (dim(reslist)[2])/2
  # Dummy Matrix um ergebnisse zu speichen
  results = array(data = NA, dim = c( length(m$model$formerx),2, iteration))
  for (i in 1:iteration)
  {
    m$model$coefficients$location = reslist[,i]
    m$model$coefficients$scale = reslist[, iteration + i]
    predictions = predict(m$model, m$model$loc$X, m$model$scale$Z, T)
    results[ ,1,i] = predictions$location
    results[ ,2,i] = predictions$scale
  }
  return(results)
}

# Helper function for quantile finding in summary for mcmcspline
getQuantiles = function(spline_values, quantile = c(0.025, 0.975))
{
  
  #dier erste dimension geht wegen dem subsetting verloren
  quantiles = apply(X = spline_values, FUN = quantile, 
                    c(1,2), quantile)
  
  return(quantiles)
}
