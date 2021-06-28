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
require(Matrix)


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
  unpenalized_info = m$unpenalized_info

  list$beta[1, ] = m$coefficients$location
  list$gamma[1, ] = m$coefficients$scale
  list$epsilon[1] = sample.epsilon(list, K2, rk_K2, 2)
  list$tau[1] =   sample.tau(list, K1, rk_K1, 2)


  for (i in 2:(it))
  {
    list[[1]][i] =   sample.tau(list, K1, rk_K1, i)
    list[[2]][i, ] = sample.beta(list, X, Z, y, K1, i)
    list[[3]][i] =   sample.epsilon(list, K2, rk_K2, i)
    list[[4]][i, ] = sample.gamma(list, X, Z, y, K2, i, ngamma, cov, unpenalized_info)
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


sample.gamma = function(list, X, Z, y, K, i, ngamma, cov, unpenalized_info)
{
  gamma = matrix(list$gamma[i-1,], ncol = 1)
  beta = matrix(list$beta[i,], ncol = 1)
  epsilon = list$epsilon[i]
  info_gamma = unpenalized_info + 1/epsilon * K
  chol_info_gamma = chol(info_gamma)

  ####new proposal####
  fitted_values_scale = drop(exp(Z %*% gamma))
  residuals = drop(y - X %*% beta)
  score_gamma = ((residuals/fitted_values_scale)^2 - 1) %*% Z

  fwd = forwardsolve(l = chol_info_gamma,
                       x = t(score_gamma) - 1/epsilon * (K %*% gamma),
                       upper.tri = TRUE, transpose = TRUE)
  step = backsolve(r = chol_info_gamma, x = fwd)

  mean_sampler <- gamma + step
  proposal <- drop(rmvnorm(1, mean_sampler, solve(info_gamma)))

  forward <- dmvnorm(proposal, mean_sampler, solve(info_gamma), log = TRUE)

  ##backward probability
  fitted_values_scale = drop(exp(Z %*% proposal))
  residuals = drop(y - X %*% beta)
  score_proposal = ((residuals/fitted_values_scale)^2 - 1) %*% Z

  fwd = forwardsolve(l = chol_info_gamma,
                     x = t(score_proposal) - 1/epsilon * (K %*% proposal),
                     upper.tri = TRUE, transpose = TRUE)
  step = backsolve(r = chol_info_gamma, x = fwd)

  mean_sampler_proposal <- proposal + step
  backward <- dmvnorm(drop(gamma), mean_sampler_proposal, solve(info_gamma), log = TRUE)
  ########


  log_full_cond = function(gamma)
  {
    faktor1 = -sum(Z %f*f% gamma)
    faktor2_helper = (y - X %f*f% beta) / exp(Z %f*f% gamma)
    faktor2 = -.5 * (t(faktor2_helper) %f*f% faktor2_helper)
    faktor3 = -1/(2 * epsilon) * t(gamma) %f*f% K %f*f% gamma #* nrow(X)
    return(faktor1 + faktor2 + faktor3)
  }


  logp = log_full_cond(proposal) - log_full_cond(gamma) + backward - forward
  print(c(optim(gamma,log_full_cond, control=list(fnscale=-1))$value,log_full_cond(proposal), log_full_cond(gamma)))

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
x = seq(0,20, length.out = 2000)
y = sin(x) + rnorm(2000,0, 1)
m = lmls(y~x, scale = ~x, light = F)
m = spline_user_function(m, c(30,30), order = c(2,2), p_order = c(3,2), smooth = c(1,1))

print.spl(m, sd = 1.96)


n = 100

#require(microbenchmark)
#microbenchmark(mcmc.spline(m, it = n, burning = 50, thinning = 1, cov = 0.02) )

a = Sys.time()
lol = mcmc.spline(m, it = n, burning = 50, thinning = 1, cov = 0.04)
b = Sys.time()
b-a


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





