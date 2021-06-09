#feed this to function: m = m$spline

mcmc = function(m, its = 1000, a1 = 1, b1 = 0.0005, a2 = 1, b2 = 0.0005) {
  result = generation_samples(m, its, a1, b1, a2, b2)
  result_thinned_and_burned = burn_thin()  # TODO ####
  return(result)  # TODO ####
}




generation_samples = function(m, its, a1, b1, a2, b2) {
  # initializing hyperparameters and starting values for parameters
  mcmc_m = m
  mcmc_m$coefficients$eps2 = 1/rgamma(1, a1, b1)
  mcmc_m$coefficients$tau2 = 1/rgamma(1, a2, b2)
  mcmc_m$hyperpar$a1 = a1
  mcmc_m$hyperpar$b1 = b1
  mcmc_m$hyperpar$a2 = a2
  mcmc_m$hyperpar$b2 = b2
  mcmc_m$hyperpar$mean_beta = NA
  mcmc_m$hyperpar$var_beta = NA

  # initializing values that stay fixed throughout mcmc-algorithm
  x = mcmc_m$x
  z = mcmc_m$z
  y = mcmc_m$y
  k1 = mcmc_m$K
  k2 = mcmc_m$K
  rank_k1 = m$ext_kn - 1 # TODO here penalization order has to be inserted instead of 1 (not yet saved in object m) ####
  rank_k2 = m$ext_kn -1 # TODO here penalization order has to be inserted instead of 1 (not yet saved in object m) ####

  # initializing matrices to store mcmc-samples
  beta_sampled = matrix(nrow = its, ncol = ncol(m$x))
  gamma_sampled = matrix(nrow = its, ncol = ncol(m$z))
  tau2_sampled = numeric(length = its)
  eps2_sampled = numeric(length = its)

  # generate mcmc-samples
  for (i in 1:its) {
    mcmc_m = mcmc_update(mcmc_m, parameter = "eps2", k = k1, rank_k = rank_k1, x = NA, z = NA, y = NA)
    mcmc_m = mcmc_update(mcmc_m, parameter =  "scale", k = k1, rank_k = NA, x = x, z = z, y = y)
    mcmc_m = mcmc_update(mcmc_m, parameter =  "tau2", k = k2, rank_k = rank_k2, x = NA, z = NA, y = NA)
    mcmc_m = mcmc_update(mcmc_m, parameter =  "location", k = k2, rank_k = NA, x = x, z = z, y = y)

    eps2_sampled[i] = mcmc_m$coefficients$eps2
    gamma_sampled[i,] = mcmc_m$coefficients$scale
    tau2_sampled[i] = mcmc_m$coefficients$tau2
    beta_sampled[i,] = mcmc_m$coefficients$location
  }

  return(list(beta_sampled, gamma_sampled, tau2_sampled, eps2_sampled))
}



# samplers and update step
mcmc_update = function(curr_m, parameter, k = NA, rank_k = NA, x = NA, z = NA, y = NA) {
  if (parameter == "eps2") {

    gamma = curr_m$coefficients$scale
    new_a1 = curr_m$hyperpar$a1 + .5 * rank_k
    new_b1 = curr_m$hyperpar$b1 + .5 * t(gamma) %*% k %*% gamma
    eps2_proposed = 1/rgamma(1, new_a1, new_b1)

    curr_m$coefficients$eps2 = eps2_proposed
    curr_m$hyperpar$a1 = new_a1
    curr_m$hyperpar$b1 = new_b1
    return(curr_m)

  } else if (parameter == "tau2") {

    beta = curr_m$coefficients$location
    new_a2 = curr_m$hyperpar$a2 + .5 * rank_k
    new_b2 = curr_m$hyperpar$b2 + .5 * t(beta) %*% k %*% beta
    tau2_proposed = 1/rgamma(1, new_a2, new_b2)

    curr_m$coefficients$tau2 = tau2_proposed
    curr_m$hyperpar$a2 = new_a2
    curr_m$hyperpar$b2 = new_b2
    return(curr_m)

  } else if (parameter == "location") {
    gamma = curr_m$coefficients$scale
    tau2 = curr_m$coefficients$tau2
    temp1 = chol(t(x) %*% diag(drop(1/exp(z %*% gamma)^2)) %*% x + 1/tau2 * k)
    temp2 = t(x) %*% diag(drop(1/exp(z %*% gamma))) %*% y

    fwd = forwardsolve(l = temp1, x = temp2, upper.tri = TRUE, transpose = TRUE)
    mean = backsolve(r = temp1, x = fwd)
    beta_proposed = mean + backsolve(r = temp1, x = rnorm(ncol(x)))
    curr_m$coefficients$location = beta_proposed
    return(curr_m)

  } else if (parameter == "scale") {
    return(curr_m)
    # TODO (this will be more complicated) ####
  }
}



burn_thin = function() { #TODO ####
  return(NA)
}


# test
test_return = mcmc(m)





