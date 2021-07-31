`%f*f%` <- function(a, b)
{
  eigenMapMatMult(a, b)
}

rmvnorm2 = function(n, mu = 0, chol_sig_inv) 
{
  dim = nrow(chol_sig_inv)
  
  std_norm = matrix(rnorm(dim * n), dim, n)
  scaled = backsolve(chol_sig_inv, std_norm)
  shifted = scaled + mu
  
  return(shifted)
}

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