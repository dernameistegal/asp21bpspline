library(lmls)
library(asp21bpspline)
library(mvtnorm)

####first data generating process (easy to fit with low order splines)####
# data generating process and generating data
set.seed(10)
x <- runif(1000, 0 , 10)
e <- rnorm(1000,sd = (3*sin(x) + 0.1*x^2- 0.01*x^3+ 0.0005* x^4)/3+3)
y <- 3*sin(x) + 0.1*x^2- 0.01*x^3+ 0.001* x^4 + e
plot(x,y)

# fit spline and run mcmc
m = lmls(y ~ x, scale = ~x, light = FALSE)
m_spline = spline(m, kn = c(15,15), order = c(3,3), p_order = c(2,2), smooth = c(10,10))
plot(m_spline)
mcmc_m_spline = mcmc.spline(m_spline, it = 500, burning = 100, thinning = 10)

#plot beta and gamma components against time
layout(matrix(c(1,2), 1, 2))
ind = seq(1, length(mcmc_m_spline$beta[,1]))
beta_first_component = mcmc_m_spline$beta[,1]
plot(ind, beta_first_component, type = "l")

ind = seq(1, length(mcmc_m_spline$gamma[,1]))
gamma_first_component = mcmc_m_spline$gamma[,1]
plot(ind, gamma_first_component, type = "l")
layout(1)

####second data generating process (hard to fit with low order splines)####
# data generating process and generating data
set.seed(10)

x = seq(0,20, length.out = 500)
y = 5*sin(x) + rnorm(500, 0,sd =1 + (sin(x)))
plot(x,y)

# fit spline and run mcmc
m = lmls(y ~ x, scale = ~x, light = FALSE)
m_spline = spline(m, kn = c(55,55), order = c(5,5), p_order = c(1,1), smooth = c(1,1))
plot(m_spline)
mcmc_m_spline = mcmc.spline(m_spline, it = 1000, burning = 100, thinning = 10)

#plot beta and gamma components against time
layout(matrix(c(1,2), 1, 2))
ind = seq(1, length(mcmc_m_spline$beta[,1]))
beta_first_component = mcmc_m_spline$beta[,1]
plot(ind, beta_first_component, type = "l")

ind = seq(1, length(mcmc_m_spline$gamma[,1]))
gamma_first_component = mcmc_m_spline$gamma[,1]
plot(ind, gamma_first_component, type = "l")
layout(1)

####needed values for update gamma in first iteration of mcmc sampler
Z = m_spline$scale$Z
y = m_spline$y
X = m_spline$loc$X
epsilon = m_spline$smooth
gamma = m_spline$coefficients$scale
beta = m_spline$coefficients$location
K1 = m_spline$loc$K
K2 = m_spline$scale$K
rk_K1 = m_spline$loc$ext_kn - m_spline$p_order[1]
rk_K2 = m_spline$scale$ext_kn - m_spline$p_order[2]

a = 1 + 1/2 * rk_K1
b = 0.0005 + 1/2 * t(beta) %*% K1 %*% beta
tau = 1 / rgamma(1, shape = a, scale = b)

a = 1 + 1/2 * rk_K2
b = 0.0005 + 1/2 * t(gamma) %*% K2 %*% gamma
epsilon = 1 / rgamma(1, shape = a, scale = b)

unpenalized_info = m_spline$unpenalized_info
info_gamma = unpenalized_info + 1/epsilon * K2
chol_info_gamma = chol(info_gamma)
#gamma = gamma +rnorm(length(gamma))

log_full_cond = function(gamma)
{
  faktor1 = -sum(Z %*% gamma)
  faktor2_helper = (y - X %*% beta) / exp(Z %*% gamma)
  faktor2 = -.5 * (t(faktor2_helper) %*% faktor2_helper)
  faktor3 = -1/(2 * epsilon) * t(gamma) %*% K2 %*% gamma
  return(faktor1 + faktor2 + faktor3)
}

stepsize = matrix(nrow = length(gamma), ncol = 100)
gradient_descent = function() {
  for( i in 1:100){
    fitted_values_scale = drop(exp(Z %*% as.matrix(gamma)))
    residuals = drop(y - X %*% as.matrix(beta))
    score_gamma = t((residuals/fitted_values_scale)^2 - 1) %*% Z
    
    fwd = forwardsolve(l = chol_info_gamma,
                       x = t(score_gamma) - 1/epsilon * (K2 %*% as.matrix(gamma)),
                       upper.tri = TRUE, transpose = TRUE)
    step = backsolve(r = chol_info_gamma, x = fwd)
    
    gamma <- gamma + step
    print(log_full_cond(gamma))
    stepsize[,i] = step
  }
  return(list(gamma, stepsize))
}

optimum = gradient_descent()
stepsize = optimum[[2]]



##proposal
fitted_values_scale = drop(exp(Z %*% as.matrix(gamma)))
residuals = drop(y - X %*% as.matrix(beta))
score_gamma = t((residuals/fitted_values_scale)^2 - 1) %*% Z

fwd = forwardsolve(l = chol_info_gamma,
                   x = t(score_gamma) - 1/epsilon * (K2 %*% as.matrix(gamma)),
                   upper.tri = TRUE, transpose = TRUE)
step = backsolve(r = chol_info_gamma, x = fwd)

mean_sampler <- gamma + step
proposal <- drop(rmvnorm(1, mean_sampler, solve(info_gamma)))

log_full_cond(proposal)
log_full_cond(gamma)

forward <- dmvnorm(proposal, mean_sampler, solve(info_gamma), log = TRUE)

##backward probability
fitted_values_scale = drop(exp(Z %*% as.matrix(proposal)))
residuals = drop(y - X %*% as.matrix(beta))
score_proposal = t((residuals/fitted_values_scale)^2 - 1) %*% Z

fwd = forwardsolve(l = chol_info_gamma,
                   x = t(score_proposal) - 1/epsilon * (K2 %*% as.matrix(proposal)),
                   upper.tri = TRUE, transpose = TRUE)
step = backsolve(r = chol_info_gamma, x = fwd)

mean_sampler_proposal <- proposal + step
backward <- dmvnorm(drop(gamma), mean_sampler_proposal, solve(info_gamma), log = TRUE)



##acceptance prob
logp = log_full_cond(proposal) - log_full_cond(gamma) + backward - forward

accept = logp > log(runif(1))


optim(gamma,log_full_cond, method = "BFGS" ,control = list(fnscale = - 1))$value
par = optim(gamma,log_full_cond, method = "BFGS", control = list(fnscale = - 1))$par
log_full_cond(optimum)
log_full_cond(par)



cbind(par,optimum)








