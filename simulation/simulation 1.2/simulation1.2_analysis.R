source("simulation/general functions/analysis_functions.R")
source("simulation/general functions/generic_simulation_functions.R")
library(simsalapar)
library(ggplot2)
require(asp21bpspline)


simulation1.2 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 500),
  n = list(type = "frozen", value = 1000),
  it = list(type = "frozen", value = 1500),
  beta = list(type = "frozen", expr = quote(beta), value = beta),
  gamma = list(type = "frozen", expr = quote(gamma), value = gamma),
  knots = list(type = "grid", value = c(20, 40)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(3,3)),
  smooth =  list(type = "frozen", value = c("Platzhalter")),
  burning =  list(type = "frozen", value = 500),
  thinning =  list(type = "frozen", value = 10))




res = maybeRead("simulation/simulation 1.2/500samplesseeded")
beta = read.csv("simulation/simulation 1/beta_sim1")
gamma = read.csv("simulation/simulation 1/gamma_sim1")
x = seq(0,20, length.out = 1000)

truth = list(beta, gamma)

# make sure to use the correct function
source("simulation/simulation 1/simulation1_functions.R")
truepred = predict_simulation(beta, gamma, knots = c(15, 15), order = c(3, 3), x)
source("simulation/general functions/generic_simulation_functions.R")

#choose 20 or 40 knots results

# for 20 knots
res10 = res[1,]
res10 = RemoveErrors(res10)
simulation1.2$knots$value = c(20,20)

# for 40 knots
res10 = res[2,]
res10 = RemoveErrors(res10)
simulation1.2$knots$value = c(40,40)

### Do Analysis for both 20 and 40 knots ###

# checking for bias and MSE in predictions
# bias = biasSE(truepred, res10, MCMC = F, parameter = F, simulation1.2, x = x)
# mean(abs(bias$location[,1]) < bias$location[,2] * 1.96)
# mean(abs(bias$scale[,1]) < bias$scale[,2] * 1.96)
# mean_MSE(truepred, res10, MCMC = F, parameter = F, simulation1.2, x = x)


# checking for bias and MSE in predictions MCMC
bias = biasSE(truepred, res10, MCMC = T, parameter = F, simulation1.2, x = x)
mean(abs(bias$location[,1]) < bias$location[,2] * 1.96)
mean(abs(bias$scale[,1]) < bias$scale[,2] * 1.96)
mean_MSE(truepred, res10, MCMC = T, parameter = F, simulation1.2, x = x)

# plotting mean prediction ML
# meanbeta = findmean(res10, 1)
# meangamma = findmean(res10, 2)
# pred = predict_simulation(meanbeta, meangamma, simulation1.2, x)
# pred = data.frame(x = x, 
#                   loc = as.vector(pred$location), 
#                   scale = as.vector(pred$scale), 
#                   true_or_pred = rep("4", length(x)))
# 
# truth = data.frame(x = x, 
#                    loc = truepred[[1]], 
#                    scale = truepred[[2]], 
#                    true_or_pred = rep("2",length(x)))
# truth_pred = rbind(truth, pred)
# plot_simulation(truth_and_pred = truth_pred, sd = 1.96, ylim = c(-22,9))
# p1 = plot_simulation(truth_and_pred = truth_pred, sd = 1.96, ylim = c(-22,9))
# ggsave("simulation/plots/sim1.2/40knotssim12ML.pdf",p1,"pdf",width = 7, height = 5)



# plotting mean prediction MCMC
meanbeta = findmean(res10, 3)
meangamma = findmean(res10, 4)
pred = predict_simulation(meanbeta, meangamma, simulation1.2, x)
pred = data.frame(x = x, 
                  loc = as.vector(pred$location), 
                  scale = as.vector(pred$scale), 
                  true_or_pred = rep("4", length(x)))

truth = data.frame(x = x, 
                   loc = truepred[[1]], 
                   scale = truepred[[2]], 
                   true_or_pred = rep("2",length(x)))
truth_pred = rbind(truth, pred)
plot_simulation(truth_and_pred = truth_pred, sd = 1.96, ylim = c(-22,9))
p2 = plot_simulation(truth_and_pred = truth_pred, sd = 1.96, ylim = c(-22,9))
ggsave("simulation/plots/sim1.2/sim12_40knots_mean_mcmc.pdf",p2,"pdf",width = 7, height = 5)


#plotting specific predictions to visualize penalization
ind = sample(1:500, 1)
beta1 = res10[[ind]]$value[,3]
gamma1 = res10[[ind]]$value[,4]
pred = predict_simulation(beta1, gamma1, simulation1.2, x)
pred = data.frame(x = x, 
                  loc = as.vector(pred$location), 
                  scale = as.vector(pred$scale), 
                  true_or_pred = rep("4", length(x)))
truth = data.frame(x = x, 
                   loc = truepred[[1]], 
                   scale = truepred[[2]], 
                   true_or_pred = rep("2",length(x)))
truth_pred = rbind(truth, pred)
plot_simulation(truth_and_pred = truth_pred, sd = 1.96, ylim = c(-22,9))
p3 = plot_simulation(truth_and_pred = truth_pred, sd = 1.96, ylim = c(-22,9))
ggsave("simulation/plots/sim1.2/sim12_40knots_specific_mcmc.pdf",p3,"pdf",width = 7, height = 5)

#plotting non-penalized MLE estimate (for this last part source other predict simulation function!)
source("simulation/simulation 1/simulation1_functions.R")
x = runif(1000, 0, 20)
x = sort(x)
pred = predict_simulation(beta, gamma, c(15, 15), c(3, 3), x)
y = pred$location + rnorm(1000, 0, pred$scale)
m1 = list(x = x, z = x, y = y)
model = spline(m1, c(20, 20), order = c(3,3), p_order = c(0,0), smooth = c(0,0))


x = seq(0,20, length.out = 1000)
pred = predict(model, x, x)
pred = data.frame(x = x, 
                  loc = as.vector(pred$location), 
                  scale = as.vector(pred$scale), 
                  true_or_pred = rep("4", length(x)))
truth = data.frame(x = x, 
                   loc = truepred[[1]], 
                   scale = truepred[[2]], 
                   true_or_pred = rep("2",length(x)))
truth_pred = rbind(truth, pred)
plot_simulation(truth_and_pred = truth_pred, sd = 1.96, ylim = c(-22,9))
p4 = plot_simulation(truth_and_pred = truth_pred, sd = 1.96, ylim = c(-22,9))
ggsave("simulation/plots/sim1.2/sim12_20knots_nopen.pdf",p4,"pdf",width = 7, height = 5)


