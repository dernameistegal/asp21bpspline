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
  X = m$x
  y = m$y
  Z = m$z
  K1 = m$K
  K2 = m$K #to do####
  rk_K1 = m$ext_kn - m$p_order2
  rk_K2 = m$ext_kn - m$p_order2

  list$beta[1, ] = m$coefficients$location
  list$gamma[1, ] = m$coefficients$scale
  list$epsilon[1] = sample.epsilon(list, K2, rk_K2, 2)
  list$tau[1] =   sample.tau(list, K1, rk_K1, 2)


  for (i in 2:(it))
  {
    list[[1]][i] =   sample.tau(list, K1, rk_K1, i)
    list[[2]][i, ] = sample.beta(list, X, Z, y, K1, i)
    list[[3]][i] =   sample.epsilon(list, K2, rk_K2, i)
    list[[4]][i, ] = sample.gamma(list, X, Z, y, K2, i, ngamma, cov)
  }

  #list = burn(list, burning)
  #list = thin(list, thinning)

  return(list)
}


sample.tau = function(list, K, rk_K, i)
{
  # check for validity
  beta = matrix(list$beta[i-1,], ncol = 1)

  a = 1 + 1/2 * rk_K
  b = 0.0005 + 1/2 * t(beta) %*% K %*% beta

  tau = 1 / rgamma(1, shape = a, scale = b)
  return(tau)
}


sample.beta = function(list, X, Z, y, K, i)
{
  # check for validity
  gamma = matrix(list$gamma[i-1, ], ncol = 1)
  tau = list$tau[i]

  sigmainv = diag(drop(1 / exp((Z %*% gamma)^2)))

  cov = solve(t(X) %*% sigmainv %*% X + K / tau)
  mean = cov %*% (t(X) %*% sigmainv %*% y)

  beta = rmvnorm(1, mean, cov)
  return(beta)
}


sample.epsilon = function(list, K, rk_K, i)
{
  # check for validity
  gamma = matrix(list$gamma[i-1,], ncol = 1)

  a = 1 + 1/2 * rk_K
  b = 0.0005 + 1/2 * t(gamma) %*% K %*% gamma

  epsilon = 1 / rgamma(1, shape = a, scale = b)
  return(epsilon)
}


sample.gamma = function(list, X, Z, y, K, i, ngamma, cov)
{
  gamma = matrix(list$gamma[i-1,], ncol = 1)
  beta = matrix(list$beta[i,], ncol = 1)
  epsilon = list$epsilon[i]

  proposal = gamma + rnorm(ngamma, mean = 0, sd = cov)



  log_full_cond = function(gamma)
  {
    faktor1 = -sum(Z %*% gamma)
    faktor2_helper = (y - X %*% beta) / exp(Z %*% gamma)
    faktor2 = -.5 * (t(faktor2_helper) %*% faktor2_helper)
    faktor3 = -1/(2 * epsilon) * t(gamma) %*% K %*% gamma * nrow(X)
    return(faktor1 + faktor2 + faktor3)
  }

  dgamma_old = sum(dnorm(gamma, mean = proposal, sd = cov, log = TRUE))
  dgamma_new = sum(dnorm(proposal, mean = gamma, sd = cov, log = TRUE))

  logprop_ratio = dgamma_old - dgamma_new
  logp = log_full_cond(proposal) - log_full_cond(gamma) + logprop_ratio


# accept whole vector
accept = logp > log(runif(1))
if (accept == T)
{
  return(proposal)
}
else
{
  return(gamma)
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
set.seed(1)
x = seq(0,10, length.out = 100)
y = x + rnorm(100)
m = lmls(y~x, light = F)
m = spline_user_function(m, 10, order = 2, p_order = 2, lambda = 1)
m = m$spline
n = 10000
lol = mcmc(m, it = n, burning = 10, thinning = 10, cov = 0.1)



seq1 = seq(1, n, length.out = n)
plot(seq1, lol$tau, type = "l")
plot(seq1, lol$epsilon, type = "l")
plot(seq1, lol$beta[, 1], type = "l")
plot(seq1, lol$gamma[, 1], type = "l")



