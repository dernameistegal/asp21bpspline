#' MCMC sampling for location-scale bspline regression
#'
#' @param m list with entries x and z, the predictors for mean and scale 
#'               respectively and y, the response.
#' @param it integer   Number of iterations
#' @param burning integer   Number  of iterations to be burned
#' @param thinning integer   Number of iterations to be ignored between two
#'                           consecutive iterations
#' @param stepsize positive float stepsize for gamma sampler. Standard value is
#'                                sqrt(3) * (df)^(-1/6).
#'
#' @return MCMC sample for epsilon, tau, gamma and beta
#' @export
#'
#'
mcmc.spline = function(m, it, burning, thinning, stepsize = NA, betastart = NA, 
                       gammastart = NA, epsstart = NA, taustart = NA)
{
  pb = txtProgressBar(min = 0, max = it, style = 3)
  
  
  nbeta = length(m$coefficients$location)
  ngamma = length(m$coefficients$scale)
  sample_tau = numeric(it)
  sample_epsilon = numeric(it)
  sample_beta = matrix(NA, nrow = it, ncol = nbeta)
  sample_gamma = matrix(NA, nrow = it, ncol = ngamma)
  if (is.na(stepsize))
  {
    df = ncol(m$loc$X) + ncol(m$scale$Z)
    stepsize = sqrt(3) * (df)^(-1/6)
  }
  
  list = list(tau = sample_tau,
              beta = sample_beta,
              epsilon = sample_epsilon,
              gamma = sample_gamma)
  
  # Extract elements from model object and initialize starting values for MCMC algorithm
  X = m$loc$X
  y = m$y
  Z = m$scale$Z
  K1 = m$loc$K
  K2 = m$scale$K
  rk_K1 = m$loc$ext_kn - m$p_order[1]
  rk_K2 = m$scale$ext_kn - m$p_order[2]
  unpenalized_info = m$unpenalized_info
  
  if (is.na(betastart)) {
    list$beta[1, ] = m$coefficients$location
  } else {
    list$beta[1, ] = betastart
  }
  
  if (is.na(gammastart)) {
    list$gamma[1, ] = m$coefficients$scale
  } else {
    list$gamma[1, ] = gammastart
  }
  
  if (is.na(epsstart)) {
    list$epsilon[1] = 0.001
  } else {
    list$epsilon[1] = epsstart
  }
  
  if (is.na(taustart)) {
    list$tau[1] = 0.001
  } else {
    list$tau[1] = taustart
  }
  
  # Loop for MCMC-algorithm
  for (i in 2:(it))
  {
    list[[1]][i] =   sample.tau(list, K1, rk_K1, i)
    list[[2]][i, ] = sample.beta(list, X, Z, y, K1, i)
    list[[3]][i] =   sample.epsilon(list, K2, rk_K2, i)
    list[[4]][i, ] = sample.gamma(list, X, Z, y, K2, i, ngamma, cov, unpenalized_info, stepsize)
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  list = burn(list, burning)
  list = thin(list, thinning)
  
  class(list) = "mcmcspline"
  list$call = match.call()
  list$model = m
  return(list)
}

# Method to sample from full conditional of Tau
sample.tau = function(list, K, rk_K, i)
{
  beta = matrix(list$beta[i-1,], ncol = 1)
  a = 1 + 1/2 * rk_K
  b = 0.0005 + 1/2 * t(beta) %f*f% K %f*f% beta
  tau = 1 / rgamma(1, shape = a, scale = b)
  return(tau)
}

# Method to sample from full conditional of beta
sample.beta = function(list, X, Z, y, K, i)
{
  gamma = matrix(list$gamma[i-1, ], ncol = 1)
  tau = list$tau[i]
  sigmainv = diag(drop(1 / (exp(Z %f*f% as.matrix(gamma)))^2))
  chol_helper = chol(t(X) %f*f% sigmainv %f*f% X + K / tau)
  
  fwd = forwardsolve(l = chol_helper,
                     x = diag(dim(chol_helper)[1]),
                     upper.tri = TRUE, transpose = TRUE)
  cov = backsolve(r = chol_helper, x = fwd )
  mean = cov %f*f% (t(X) %f*f% sigmainv %f*f% as.matrix(y))
  beta = rmvnorm(1, mean, cov)
  return(beta)
}

# Method to sample from full conditional of epsilon
sample.epsilon = function(list, K, rk_K, i)
{
  gamma = matrix(list$gamma[i-1,], ncol = 1)
  a = 1 + 1/2 * rk_K
  b = 0.0005 + 1/2 * t(gamma) %f*f% K %f*f% gamma
  epsilon = 1 / rgamma(1, shape = a, scale = b)
  return(epsilon)
}

# Method to sample from proposal density of gamma and decide whether to accept or not
sample.gamma = function(list, X, Z, y, K, i, ngamma, cov, unpenalized_info, stepsize)
{
  gamma = matrix(list$gamma[i-1,], ncol = 1)
  beta = matrix(list$beta[i,], ncol = 1)
  epsilon = list$epsilon[i]
  info_gamma = unpenalized_info + 1/epsilon * K
  chol_info_gamma = chol(info_gamma)
  
  # New proposal and forward probability
  fitted_values_scale = drop(exp(Z %f*f% as.matrix(gamma)))
  residuals = drop(y - X %f*f% as.matrix(beta))
  score_gamma = t((residuals/fitted_values_scale)^2 - 1) %f*f% Z
  
  fwd = forwardsolve(l = chol_info_gamma,
                     x = t(score_gamma) - 1/epsilon * (K %f*f% as.matrix(gamma)),
                     upper.tri = TRUE, transpose = TRUE)
  step = backsolve(r = chol_info_gamma, x = fwd)
  mean_sampler = gamma + stepsize^2/2 * step
  
  
  proposal = drop(rmvnorm2(1, mean_sampler, chol_info_gamma/stepsize))
  forward = dmvnorm2(proposal, mean_sampler, chol_info_gamma/stepsize, log = TRUE)
  
  # Backward probability
  fitted_values_scale = drop(exp(Z %f*f% as.matrix(proposal)))
  residuals = drop(y - X %f*f% as.matrix(beta))
  score_proposal = t((residuals/fitted_values_scale)^2 - 1) %f*f% Z
  
  fwd = forwardsolve(l = chol_info_gamma,
                     x = t(score_proposal) - 1/epsilon * (K %f*f% as.matrix(proposal)),
                     upper.tri = TRUE, transpose = TRUE)
  step = backsolve(r = chol_info_gamma, x = fwd)
  
  mean_sampler_proposal = proposal + stepsize^2/2 * step
  backward = dmvnorm2(drop(gamma), mean_sampler_proposal, chol_info_gamma/stepsize, log = TRUE)
  
  # Log full conditional of gamma
  log_full_cond = function(gamma)
  {
    faktor1 = -sum(Z %f*f% gamma)
    faktor2_helper = (y - X %f*f% beta) / exp(Z %f*f% gamma)
    faktor2 = -.5 * (t(faktor2_helper) %f*f% faktor2_helper)
    faktor3 = -1/(2 * epsilon) * t(gamma) %f*f% K %f*f% gamma
    return(faktor1 + faktor2 + faktor3)
  }
  
  # Acceptance probability and decision
  logp = log_full_cond(proposal) - log_full_cond(gamma) + backward - forward
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

# burns prespecified amount of iterations from MCMC-algorithm
burn = function(result, burning = 1)
{
  len = length(result$epsilon)
  result$epsilon = result$epsilon[burning:len]
  result$tau = result$tau[burning:len]
  result$beta = result$beta[burning:len,]
  result$gamma = result$gamma[burning:len,]
  return(result)
}

# thins prespecified prespecified amount of iterations
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