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