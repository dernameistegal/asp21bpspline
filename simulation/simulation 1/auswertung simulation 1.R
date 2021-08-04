library(simsalapar)

simulation1 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 10),
  n = list(type = "grid", value = 1000),
  beta = list(type = "frozen", expr = quote(beta), value = beta),
  gamma = list(type = "frozen", expr = quote(gamma), value = gamma),
  knots = list(type = "frozen", value = c(15, 15)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(0,0)),
  smooth =  list(type = "frozen", value = c(0,0)))



res10 = maybeRead("simulation/simulation 1/take1")
beta = read.csv("simulation/simulation 1/beta_sim1")
gamma = read.csv("simulation/simulation 1/gamma_sim1")

truth = list(beta, gamma)

# checking for bias in parameters
bias = biasSE(truth, res10, MCMC = F, parameter = T, simulation1, x = NA)
abs(bias$location[,1]) > bias$location[,2] * 1.96
abs(bias$scale[,1]) > bias$scale[,2] * 1.96

bias = biasSE_parameters(beta, gamma, get.n.sim(simulation1), res10, MCMC = T)
abs(bias$beta[,1]) > bias$beta[,2] * 1.96
abs(bias$gamma[,1]) > bias$gamma[,2] * 1.96


# plotting mean prediction
meanbeta = findmean(17, 10, res10, 1)
meangamma = findmean(17,10, res10, 2)
x = seq(0, 20, length.out = 100)
pred = predict_simulation(meanbeta, meangamma, knots = c(15, 15), order = c(3,3), x)
plot_simulation(pred, x)

# checking for MSE in predictions
MSE = MSE_predictions(beta, gamma, get.n.sim(simulation1), res10, seq = x, knots = c(15,15), 
                order = c(3,3))
sd = sqrt(MSE)




bias = bias_predictions(beta, gamma, get.n.sim(simulation1), res10, seq = x, knots = c(15,15), 
                order = c(3,3))
bias
colMeans(bias)


