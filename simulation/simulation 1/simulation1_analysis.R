source("simulation/general functions/analysis_functions.R")
source("simulation/general functions/generic_simulation_functions.R")
library(simsalapar)
library(ggplot2)
require(asp21bpspline)


simulation1 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 500),
  n = list(type = "grid", value = 1000),
  it = list(type = "frozen", value = 1500),
  beta = list(type = "frozen", expr = quote(beta), value = beta),
  gamma = list(type = "frozen", expr = quote(gamma), value = gamma),
  knots = list(type = "frozen", value = c(15, 15)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(0,0)),
  smooth =  list(type = "frozen", value = c(0,0)),
  burning =  list(type = "frozen", value = 500),
  thinning =  list(type = "frozen", value = 10))



res10 = maybeRead("simulation/simulation 1/500samplesseeded")
beta = read.csv("simulation/simulation 1/beta_sim1")
gamma = read.csv("simulation/simulation 1/gamma_sim1")
x = seq(0,20, length.out = 1000)

truth1 = list(beta, gamma)

# checking for bias and MSE in parameters
bias = biasSE(truth1, res10, MCMC = F, parameter = T, simulation1, x = NA)
mean(abs(bias$location[,1]) < bias$location[,2] * 1.96)
mean(abs(bias$scale[,1]) < bias$scale[,2] * 1.96)
mean_MSE(truth1, res10, MCMC = F, parameter = T, simulation1, x = NA)


# checking for bias and MSE in parameters MCMC
bias2 = biasSE(truth1, res10, MCMC = T, parameter = T, simulation1, x = NA)
mean(abs(bias$location[,1]) < bias$location[,2] * 1.96)
mean(abs(bias$scale[,1]) < bias$scale[,2] * 1.96)
mean_MSE(truth1, res10, MCMC = T, parameter = T, simulation1, x = NA)


# for exporting
#xtable(cbind(bias$location, bias$scale, bias2$location, bias2$scale), digits = 3)

# checking for bias and MSE in predictions
x = seq(0,20, length.out = 1000)
truepred = predict_simulation(beta, gamma, simulation1, x)
bias = biasSE(truepred, res10, MCMC = F, parameter = F, simulation1, x = x)
mean(abs(bias$location[,1]) < bias$location[,2] * 1.96)
mean(abs(bias$scale[,1]) < bias$scale[,2] * 1.96)
mean_MSE(truepred, res10, MCMC = F, parameter = F, simulation1, x = x)


# checking for bias and MSE in predictions MCMC
bias = biasSE(truepred, res10, MCMC = T, parameter = F, simulation1, x = x)
mean(abs(bias$location[,1]) < bias$location[,2] * 1.96)
mean(abs(bias$scale[,1]) < bias$scale[,2] * 1.96)
mean_MSE(truepred, res10, MCMC = T, parameter = F, simulation1, x = x)


# plotting mean prediction ML
meanbeta = findmean(res10, 1)
meangamma = findmean(res10, 2)
pred = predict_simulation(meanbeta, meangamma, simulation1, x)
pred = data.frame(x = x, 
                  loc = as.vector(pred$location), 
                  scale = as.vector(pred$scale), 
                  true_or_pred = rep("4", length(x)))

truth = data.frame(x = x, 
                   loc = truepred[[1]], 
                   scale = truepred[[2]], 
                   true_or_pred = rep("2",length(x)))
truth_pred = rbind(truth, pred)
plot_simulation(truth_and_pred = truth_pred, sd = 1.96,c(-17, 9))

p1 = plot_simulation(truth_and_pred = truth_pred, sd = 1.96,c(-17, 9))
ggsave("plotsim1ML.pdf",p1,"pdf",width = 7, height = 5)


# plotting mean prediction MCMC
meanbeta = findmean(res10, 3)
meangamma = findmean(res10, 4)
pred = predict_simulation(meanbeta, meangamma, simulation1, x)
pred = data.frame(x = x, 
                  loc = as.vector(pred$location), 
                  scale = as.vector(pred$scale), 
                  true_or_pred = rep("4", length(x)))

truth = data.frame(x = x, 
                   loc = truepred[[1]], 
                   scale = truepred[[2]], 
                   true_or_pred = rep("2",length(x)))
truth_pred = rbind(truth, pred)

plot_simulation(truth_and_pred = truth_pred, sd = 1.96, ylim = c(-17,9))
p2 = plot_simulation(truth_and_pred = truth_pred, sd = 1.96,c(-17, 9))
ggsave("plotsim1mcmc.pdf",p2,"pdf",width = 7, height = 5)

#Plotting quantiles

#without MCMC
spline_values = getEstimateValuesFourData(res10, simulation1, x, MCMC = F)
est_quantile = getQuantiles(spline_values, quantile = c(0.025,0.975))
est_mean = predict_simulation(findmean(res10, 1),findmean(res10,2),simulation1,x)
plot_simulation3(est_mean, est_quantile,x, c(-20, 14),y = "mean and confidence intervals" )
p3 = plot_simulation3(est_mean, est_quantile,x, c(-20, 14),y = "mean and confidence intervals" )
ggsave("quantilessim1ML.pdf",p3,"pdf",width = 7, height = 5)

#with MCMC
spline_values = getEstimateValuesFourData(res10, simulation1, x, MCMC = T)
est_quantile = getQuantiles(spline_values, quantile = c(0.025,0.975))
est_mean = predict_simulation(findmean(res10, 3),findmean(res10,4),simulation1,x)
plot_simulation3(est_mean, est_quantile,x, c(-20, 14), y = "mean and confidence intervals")
p4 = plot_simulation3(est_mean, est_quantile,x, c(-20, 14), y = "mean and confidence intervals")
ggsave("quantilessim1MCMCL.pdf",p4,"pdf",width = 7, height = 5)



