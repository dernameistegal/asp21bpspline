library(lslm)

# Fisher Scoring Algorithm

estimation = function(m, maxit = 100, reltol = sqrt(.Machine$double.eps))
{
  m$spline$z = m$spline$x
  m$spline  = init_beta(m$spline)
  m$spline = init_gamma(m$spline)
  m$spline = update_beta(m$spline)

  it = 0
  enough = TRUE

  while (enough)
  {
    it = it + 1

    before = sum(m$spline$coefficients$location)
    m$spline = update_gamma(m$spline)
    m$spline = update_beta(m$spline)
    after =  sum(m$spline$coefficients$location)

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
  m$chol_info_gamma = chol(info_gamma(m))
  fit = leastsquares(m, m$y, m$lambda)
  m$coefficients$location = coef(fit)
  m$fitted.values$location = fitted(fit)
  m$residuals$location = residuals(fit)
  return(m)
}


init_gamma = function(m)
{
  fit = leastsquares(m, 0.6351814 + log(abs(m$residuals$location)), m$lambda)
  m$coefficients$scale = coef(fit)
  m$fitted.values$scale = exp(fitted(fit))
  return(m)
}

update_gamma = function(m)
{
  fwd = forwardsolve(l = m$chol_info_gamma,x = score_gamma(m),upper.tri = TRUE, transpose = TRUE)
  step = backsolve(r = m$chol_info_gamma, x = fwd )

  m = set_gamma(m, coef(m)$scale + step)
  return(m)
}


update_beta = function(m)
{
  m$coefficients$location = m$coefficients$location + solve(info_beta(m)) %*% score_beta(m)
  m$fitted.values$location = m$x %*% m$coefficients$location
  m$residuals$location = m$y - m$fitted.values$location
  return(m)
}


# own helper

leastsquares = function(m, y, lambda = 0)
{
  Z = m$x
  beta_hat = solve(crossprod(Z) +  m$K * lambda) %*% t(Z) %*% y
  y_hat = Z %*% beta_hat

  fit = list()
  fit$fitted.values = y_hat
  fit$residuals = y - y_hat
  fit$coefficients = beta_hat

  return(fit)
}


#### helpers from lslm

# changed from source lslm
score_beta = function(m)
{
  ups = t(m$residuals$location / m$fitted.values$scale^2) %*% m$x
  return(drop(ups))
}

# changed from source lslm
score_gamma = function(m)
{
  ups = (t(m$residuals$location / m$fitted.values$scale)^2 - 1) %*% m$z
  return(drop(ups))
}

# changed from source lslm
info_beta = function(m)
{
  crossprod(m$x, diag(as.vector(1/(m$fitted.values$scale^2))) %*% m$x)
}

# not changed from source
info_gamma = function(m)
{
  2 * crossprod(m$z)
}

# not changed from source
set_gamma = function(m, gamma)
{
  m$coefficients$scale = gamma
  m$fitted.values$scale = exp(drop(m$z %*% gamma))
  return(m)
}

x = seq(0,10, length.out = 100)
y = x + rnorm(100, 0, 0.1)
m = lslm(y ~ x, light = F)
#spline_user_function(m, 10, 2, 2, 0)
