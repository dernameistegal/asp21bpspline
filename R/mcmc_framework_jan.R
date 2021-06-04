"
Lets build a general framework for mcmc sampling

mcmc - main function with
m       lslm spline object
it      integer   iterations
burning integer   no of iterations to be burned
thinninginteger   no of iterations to be ignored between two consecutive iterations

"

mcmc = function(m, it, burning, thinning)
{

  sample_tau = numeric(it)
  sample_epsilon = numeric(it)
  sample_beta = matrix(NA, ncol = it, nrow = 10) # change 10 to number of intervals
  sample_gamma = matrix(NA, ncol = it, nrow = 10) # change 10 to number of intervals

  sample_list = list(tau = sample_tau,
                     beta = sample_beta,
                     epsilon = sample_epsilon,
                     gamma = sample_gamma)

  sample_list = set_start_values(sample_list, m)

  for (i in 1:it)
  {
    sample_list[[1]][i] = sample(sample_list, method = "tau")
    sample_list[[2]][i,] = sample(sample_list, method = "beta")
    sample_list[[3]][i] = sample(sample_list, method = "epsilon")
    sample_list[[4]][i, ] = sample(sample_list, method = "gamma")
  }

  sample_list = burn(sample_list, burning)
  sample_list = thin(sample_list, thinning)

  return(sample_list)
}


sample = function(sample_list, method)
{
  class(sample_list) = method
  UseMethod("sample", sample_list)
}


sample.tau = function(sample_list, method)
{
  #todo

  return(NA)
}


sample.beta = function(sample_list, method)
{
  #todo

  return(rep(NA, times = ncol(sample_list$beta)))
}


sample.epsilon = function(sample_list, method)
{
  #todo

  return(NA)
}


sample.gamma = function(sample_list, method)
{
  #todo

  return(rep(NA, times = ncol(sample_list$beta)))
}



set_start_values = function(sample_list, m)
{
  #todo
  return(sample_list)
}


burn = function(sample_list, burning)
{
  #todo
  return(sample_list)
}


thin = function(sample_list, thinning)
{
  #todo
  return(sample_list)
}

mcmc(2, 10, 10, 10)





