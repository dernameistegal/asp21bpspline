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
#das wäre nötig für den verbleibenden fall, dann wäre es aber eine untere Dreiecksmatrix und das ganze daher nicht wirklich lösbar mit
#der vorhandenen FUnktion, daher (L^t^-1), daher erstmal weiterhin rmtnorm Package genutzt
#cov(t(rmvnorm2(1000,c(1,1),solve(t(chol(a))))))


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