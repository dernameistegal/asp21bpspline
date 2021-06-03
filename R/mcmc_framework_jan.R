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
  i = 0
  sample_tau = numeric(it)
  sample_epsilon = numeric(it)
  sample_beta = matrix(NA, ncol = it, nrow = extknots)
  sample_gamma = matrix(NA, ncol = it, nrow = extknots)

  sample_list = list(sample_tau, sample_epsilon, sample_beta, sample_gamma)

  for (i in 1:it)
  {
    sample_tau[i] = sample(choose_previous_values(sample_list), method = "tau")
    sample_beta[i] = sample(choose_previous_values(sample_list), method = "beta")
    sample_epsilon[i] = sample(choose_previous_values(sample_list), method = "epsilon")
    sample_gamma[i] = sample(choose_previous_values(sample_list), method = "gamma")
  }
}
