# Fisher Scoring Algorithm

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
      warning("Estimation did not converge, maximum number of iterations reached")
      break
    }
  }
  m$iterations = it
  return(m)
}


#### estimation####
init_beta = function(m)
{
  fit = leastsquares(m$loc$X, m$loc$K , m$y, m$smooth[1])
  m$coefficients$location = coef(fit)
  m$fitted.values$location = fitted(fit)
  m$residuals$location = residuals(fit)
  return(m)
}


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

update_gamma = function(m)
{
  fwd = forwardsolve(l = m$chol_info_gamma,
                     x = score_gamma(m) - m$smooth[2] * drop(m$scale$K %*% coef(m)$scale),
                     upper.tri = TRUE, transpose = TRUE)
  step = backsolve(r = m$chol_info_gamma, x = fwd )

  m = set_gamma(m, coef(m)$scale + step)
  return(m)
}


update_beta = function(m)
{
  fit = w_leastsquares(m)
  m$coefficients$location <- coef(fit)
  m$fitted.values$location <- fitted(fit)
  m$residuals <- resid(fit)
  return(m)
}


# own helper

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

# vermutlich ist das auch so nicht ganz richtig, weil sich auch m$K * lambda ändern müsste
w_leastsquares = function(m)
{
  lambda = m$smooth[1]
  y = m$y
  Z = m$loc$X
  W = diag(as.vector(m$fitted.values$scale))
  chol_helper = chol(crossprod(X) + K * lambda + diag(dim(K)[1]))
  fwd = forwardsolve(l = chol_helper,
                     x = t(X) %*% y,
                     upper.tri = TRUE, transpose = TRUE)
  beta_hat = backsolve(r = chol_helper, x = fwd )
  y_hat = Z %*% beta_hat
  fit = list()
  fit$fitted.values = y_hat
  fit$residuals = y - y_hat
  fit$coefficients = beta_hat

  return(fit)
}


#### helpers from lmls

# changed from source lmls
score_beta = function(m)
{
  ups = t(m$residuals$location / m$fitted.values$scale^2) %*% m$loc$X
  return(drop(ups))
}

# changed from source lmls
score_gamma = function(m)
{
  ups = (t(m$residuals/ m$fitted.values$scale)^2 - 1) %*% m$scale$Z
  return(drop(ups))
}

# changed from source lmls
info_beta = function(m)
{
  crossprod(m$x, diag(as.vector(1/(m$fitted.values$scale^2))) %*% m$loc$X)
}

# not changed from source
info_gamma = function(m)
{
  2 * crossprod(m$scale$Z)
}

# not changed from source
set_gamma = function(m, gamma)
{
  m$coefficients$scale = gamma
  m$fitted.values$scale = exp(m$scale$Z %*% gamma)
  return(m)
}

# x = seq(0,10, length.out = 100)
# y = x + rnorm(100, 0, 0.1)
# m = lmls(y ~ x, light = F)
#spline_user_function(m, 10, 2, 2, 0)
