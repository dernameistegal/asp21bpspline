set.seed(10)
x <- runif(1000, 0 , 10)
e <- rnorm(1000,sd = (3*sin(x) + 0.1*x^2- 0.01*x^3+ 0.0005* x^4)/3+3)
y <- 3*sin(x) + 0.1*x^2- 0.01*x^3+ 0.001* x^4 + e
plot(x,y)

library(lmls)
library(asp21bpspline)
m = lmls(y ~ x, scale = ~x, light = FALSE)
m_spline = spline(m, kn = c(15,15), order = c(3,3), p_order = c(2,2), smooth = c(10,10))
plot(m_spline)
mcmc_m_spline = mcmc.spline(m_spline, it = 5000, burning = 100, thinning = 10)

ind = seq(1, length(mcmc_m_spline$beta[,1]))
beta_first_component = mcmc_m_spline$beta[,1]
plot(ind, beta_first_component, type = "l")


lol = mcmc.spline(m_spline, it = 100, burning = 1, thinning = 4)
