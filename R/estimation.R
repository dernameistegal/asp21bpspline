'Fisher Scoring Algorithm
'
# Main function of Fisher Scoring algorithm
estimation = function(m, maxit = 100, reltol = sqrt(.Machine$double.eps))
{
  m  = init_beta(m)
  m = init_gamma(m)
  m = update_beta(m)

  it = 0
  enough = TRUE

  while (enough)
  {
    it = it + 1

    before = sum(m$coefficients$location)
    m = update_gamma(m)
    m = update_beta(m)
    after =  sum(m$coefficients$location)
    enough = abs(after - before) > reltol * (abs(before) + reltol)

    if (it >= maxit)
    {
      warning("Estimation did not converge after 100 iterations. The Maximum number of iterations was reached.")
      break
    }
  }
  m$iterations = it
  return(m)
}


# Initialization of beta
init_beta = function(m)
{
  fit = leastsquares(m$loc$X, m$loc$K , m$y, m$smooth[1])
  m$coefficients$location = coef(fit)
  m$fitted.values$location = fitted(fit)
  m$residuals$location = residuals(fit)
  return(m)
}

# Initialization of gamma
init_gamma = function(m)
{
  m$unpenalized_info = info_gamma(m)
  m$chol_info_gamma = chol(info_gamma(m) + m$smooth[2] * m$scale$K)
  fit = leastsquares(m$scale$Z, m$scale$K, 0.6351814 + log(abs(m$residuals$location)),
                     m$smooth[2])
  m$coefficients$scale = coef(fit)
  m$fitted.values$scale = exp(fitted(fit))
  return(m)
}

# Update of gamma
update_gamma = function(m)
{
  fwd = forwardsolve(l = m$chol_info_gamma,
                     x = score_gamma(m) - m$smooth[2] * drop(m$scale$K %*% coef(m)$scale),
                     upper.tri = TRUE, transpose = TRUE)
  step = backsolve(r = m$chol_info_gamma, x = fwd )

  m = set_gamma(m, coef(m)$scale + step)
  return(m)
}

# Update of beta
update_beta = function(m)
{
  fit = w_leastsquares(m)
  m$coefficients$location <- coef(fit)
  m$fitted.values$location <- fitted(fit)
  m$residuals <- resid(fit)
  return(m)
}

# Least squares estimation of beta with added ridge
leastsquares = function(X, K, y, lambda)
{

  chol_helper = chol(crossprod(X) + K * lambda + diag(dim(K)[1]))
  fwd = forwardsolve(l = chol_helper,
                     x = t(X) %*% y,
                     upper.tri = TRUE, transpose = TRUE)
  beta_hat = backsolve(r = chol_helper, x = fwd )
  y_hat = X %*% beta_hat

  fit = list()
  fit$fitted.values = y_hat
  fit$residuals = y - y_hat
  fit$coefficients = beta_hat

  return(fit)
}

# Weighted least squares estimation of beta with added ridge
w_leastsquares = function(m)
{
  lambda = m$smooth[1]
  y = m$y
  Z = m$loc$X
  W = diag(as.vector(m$fitted.values$scale))
  chol_helper = chol(t(Z) %*% W %*% Z +  m$loc$K * lambda + diag(dim(m$loc$K)[1]))
  fwd = forwardsolve(l = chol_helper,
                     x = t(Z) %*% W %*% y,
                     upper.tri = TRUE, transpose = TRUE)
  beta_hat = backsolve(r = chol_helper, x = fwd )
  y_hat = Z %*% beta_hat
  fit = list()
  fit$fitted.values = y_hat
  fit$residuals = y - y_hat
  fit$coefficients = beta_hat

  return(fit)
}