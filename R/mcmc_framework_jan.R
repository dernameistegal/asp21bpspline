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



mcmc.spline = function(m, it, burning, thinning, cov)
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
  X = m$loc$X
  y = m$y
  Z = m$scale$Z
  K1 = m$loc$K
  K2 = m$scale$K
  rk_K1 = m$loc$ext_kn - m$p_order[1]
  rk_K2 = m$scale$ext_kn - m$p_order[2]

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

  list = burn(list, burning)
  list = thin(list, thinning)

  return(list)
}


sample.tau = function(list, K, rk_K, i)
{
  # check for validity
  beta = matrix(list$beta[i-1,], ncol = 1)

  a = 1 + 1/2 * rk_K
  b = 0.0005 + 1/2 * t(beta) %f*f% K %f*f% beta

  tau = 1 / rgamma(1, shape = a, scale = b)
  return(tau)
}


sample.beta = function(list, X, Z, y, K, i)
{
  # check for validity
  gamma = matrix(list$gamma[i-1, ], ncol = 1)
  tau = list$tau[i]

  sigmainv = diag(drop(1 / (exp(Z %*% gamma))^2))

  cov = solve(t(X) %f*f% sigmainv %f*f% X + K / tau)
  mean = cov %f*f% (t(X) %f*f% sigmainv %*% y)

  beta = rmvnorm(1, mean, cov)
  return(beta)
}


sample.epsilon = function(list, K, rk_K, i)
{
  # check for validity
  gamma = matrix(list$gamma[i-1,], ncol = 1)

  a = 1 + 1/2 * rk_K
  b = 0.0005 + 1/2 * t(gamma) %f*f% K %f*f% gamma

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
    faktor1 = -sum(Z %f*f% gamma)
    faktor2_helper = (y - X %f*f% beta) / exp(Z %f*f% gamma)
    faktor2 = -.5 * (t(faktor2_helper) %f*f% faktor2_helper)
    faktor3 = -1/(2 * epsilon) * t(gamma) %f*f% K %f*f% gamma * nrow(X)
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



burn = function(result, burning = 1)
{
  len = length(result$epsilon)
  result$epsilon = result$epsilon[burning:len]
  result$tau = result$tau[burning:len]
  result$beta = result$beta[burning:len,]
  result$gamma = result$gamma[burning:len,]
  return(result)
}


thin = function(result, thinning = 1)
{
  len = length(result$epsilon)
  s = seq(1, len, by = thinning)
  result$epsilon = result$epsilon[s]
  result$tau = result$tau[s]
  result$beta = result$beta[s,]
  result$gamma = result$gamma[s,]
  return(result)
}

source("src/RcppExport.R")
source("R/spline/print_function.R")
require(lmls)
source("R/spline/estimation.R")
source("R/spline/init.R")
source("R/spline/spline.R")



set.seed(1)
x = seq(0,20, length.out = 600)
y = sin(x) + rnorm(600,0, 1)
m = lmls(y~x, scale = ~x, light = F)
m = spline_user_function(m, c(30,30), order = c(2,2), p_order = c(3,2), smooth = c(1,1))

print.spl(m, sd = 1.96)


n = 10000

require(microbenchmark)
microbenchmark(mcmc.spline(m, it = n, burning = 50, thinning = 1, cov = 0.02) )

lol = mcmc.spline(m, it = n, burning = 50, thinning = 1, cov = 0.04)


n = length(lol$tau)
seq1 = seq(1, n, length.out = n)
plot(seq1, lol$tau, type = "l")
plot(seq1, lol$epsilon, type = "l")
plot(seq1, lol$beta[, 1], type = "l")
plot(seq1, lol$gamma[, 1], type = "l")

bmean = colMeans(lol$beta)
gmean = colMeans(lol$gamma)
length(gmean)

fit.spline = function(beta, gamma, X)
{


  location = X %*% beta
  scale = exp(X %*% gamma)
  return(list(location = location, scale =  scale))
}


pred = fit.spline(bmean, gmean, m$loc$X)

plot(x, pred$location, ylim = c(-3,3))
points(x, y)
lines(x, pred$location + 1.96 * pred$scale)
lines(x, pred$location - 1.96 * pred$scale)






