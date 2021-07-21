

#m
sample.gamma = function( X = 0, Z = 0, y = 0, K = 0 , unpenalized_info = 0 , gamma = m$coefficients$scale, beta = m$coefficients$location){
  X = m$loc$X
  Z = m$scale$Z
  y = m$y
  K = m$scale$K
  epsilon = 1 / m$smooth[2]
  unpenalized_info = m$unpenalized_info
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
  return(mean_sampler)
}


update_gamma = function(m)
{
  fwd = forwardsolve(l = m$chol_info_gamma,
                     x = score_gamma(m) - m$smooth[2] * drop(m$scale$K %*% coef(m)$scale),
                     upper.tri = TRUE, transpose = TRUE)
  step = backsolve(r = m$chol_info_gamma, x = fwd )

  m = set_gamma(m, coef(m)$scale + step)
  return(m)
}

gam <- sample.gamma()
gam2 <- update_gamma(m)$coef$scale
all(gam == gam2)
cbind(gam,gam2)
