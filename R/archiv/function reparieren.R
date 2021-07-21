source("R/spline/print_function.R")
require(lmls)
source("R/spline/estimation.R")
source("R/spline/init.R")
source("R/spline/spline.R")
x = seq(0,10, length.out = 100)
y = x + rnorm(100)
m = lmls(y~x, light = F)
m = spline_user_function(m, 10, order = 2, p_order = 2, lambda = 1)
m = m$spline



sample.gamma = function(list, X, y, K, i, ngamma, cov)
{
  gamma = matrix(list$gamma[i, ], ncol = 1)
  beta = matrix(list$beta[i, ], ncol = 1)
  
  proposal = gamma + rmvnorm(ngamma, mean = 0, cov = diag(cov))
  
  
  
  log_full_cond = function(gamma) {
    temp1 = -sum(z %*% gamma)
    temp2_helper = drop((y - x %*% beta))/exp(drop(z %*% gamma))
    temp2 = -.5 * (t(temp2_helper) %*% temp2_helper)
    temp3 = -1/(2 * eps2) * t(gamma) %*% K %*% gamma
    return(temp1 + temp2 + temp3)
  }
  
  logp = drop(log_full_cond(gamma_proposed) - log_full_cond(gamma)) +
    sum(dnorm(gamma, mean = gamma_proposed, sd = .2, log = TRUE)) -
    sum(dnorm(gamma_proposed, mean = gamma, sd = .2, log = TRUE))
  logp = logLik(y, beta, gamma) - 1/
    (2 * list$epsilon[i]) * list$gamma[i] %*% K2 %*% matrix(list$gamma[i], ncol = 1)
  
  
  # accept whole vector
  accept = logp > log(runif(1))
  if (accept == T)
  {
    return(proposal)
  }
  else
  {
    return(list$gamma[i, ])
  }
}
gamma = matrix(NA, ncol = 11, nrow = 10)
gamma[1, ] = m$coefficients$scale
beta = matrix(NA, ncol = 11, nrow = 10)
beta[1, ] = m$coefficients$location
list = list(gamma = gamma, beta = beta)
list 
sample.gamma(list, m$x, m$y, m$K, i = 1, ngamma = 11, cov = 0.2, "method")



