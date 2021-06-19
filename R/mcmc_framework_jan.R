"
Lets build a general framework for mcmc sampling

mcmc - main function with
m       lslm spline object
it      integer   iterations
burning integer   no of iterations to be burned
thinninginteger   no of iterations to be ignored between two consecutive iterations
cov - variance for random walk proposals for gamma
"
require(mvtnorm)



mcmc = function(m, it, burning, thinning, cov)
{
  
  
  nbeta = length(m$coefficients$location)
  ngamma = length(m$coefficients$scale)
  sample_tau = numeric(it)
  sample_epsilon = numeric(it)
  sample_beta = matrix(NA, nrow = it, ncol = nbeta) 
  sample_gamma = matrix(NA, nrow = it, ncol = ngamma) 

  list = list(tau = sample_tau,
                     beta = sample_beta,
                     epsilon = sample_epsilon,
                     gamma = sample_gamma)

  # extract elements
  list$beta[1, ] = m$coefficients$location
  list$gamma[1, ] = m$coefficients$scale
  
  X = m$x
  y = m$y
  z = m$z
  K1 = m$K
  K2 = m$K
  rk_K1 = m$ext_kn - 1
  rk_K2 = m$ext_kn - 1
  
  

  for (i in 1:(it + 1))
  {
    list[[1]][i] =   sample(list, K1, rk_K1, i, method = "tau")
    list[[2]][i, ] = sample(list, X, y, K1, i, method = "beta")
    list[[3]][i] =   sample(list, K2, rk_K2, i, method = "epsilon")
    list[[4]][i, ] = sample(list, X, y, K2, i, ngamma, cov, method = "gamma")
  }

  list = burn(list, burning)
  list = thin(list, thinning)

  return(list)
}


sample = function(list, method, ...)
{
  class(list) = method
  UseMethod("sample", list)
}


sample.tau = function(list, K, rk_K, i, method)
{
  # check for validity 
  beta = matrix(list$beta[i,], ncol = 1)
  
  a = 1 + 1/2 * rk_K
  b = 0.0005 + 1/2 * t(beta) %*% K %*% beta
  
  tau = 1 / rgamma(1, shape = a, scale = b)
  return(tau)
}


sample.beta = function(list, X, y, K, i, method)
{
  # check for validity
  gamma2 = matrix(list$gamma[i, ]^2, ncol = 1)
  
  sigmainv = diag(drop(1 / (X %*% gamma2)))
  
  cov = solve(t(X) %*% sigmainv %*% X + K)
  mean = cov %*% (t(X) %*% sigmainv %*% y)
  
  beta = rmvnorm(1, mean, cov)
  return(beta)
}


sample.epsilon = function(list, K, rk_K, i, method)
{
  # check for validity
  gamma = matrix(list$gamma[i,], ncol = 1)
  
  a = 1 + 1/2 * rk_K
  b = 0.0005 + 1/2 * t(gamma) %*% K %*% gamma
  
  epsilon = 1 / rgamma(1,shape = a, scale = b)
  return(epsilon)
}


sample.gamma = function(list, X, y, K, i, ngamma, cov, method)
{
  proposal = list$gamma[i, ] + rmvnorm(ngamma, mean = 0, cov = diag(cov))
  
  
  
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



burn = function(result, burnfactor = 400)
{
  len = length(result$eps2)
  result$epsilon = result$eps2[burnfactor:len]
  result$tau = result$tau2[burnfactor:len]
  result$beta = result$beta[burnfactor:len,]
  result$gamma = result$gamma[burnfactor:len,]
  return(result)
}


thin = function(result, thinfactor = 4)
{
  len = length(result$eps2)
  s = seq(1, len, by = thinfactor)
  result$epsilon = result$eps2[s]
  result$tau = result$tau2[s]
  result$beta = result$beta[s,]
  result$gamma = result$gamma[s,]
  return(result)
}


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
mcmc(m, 10, 10, 10)





