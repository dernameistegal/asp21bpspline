source("simulation/general functions/analysis_functions.R")
source("simulation/general functions/generic_simulation_functions.R")
source("simulation/simulation 2/simulation2_functions.R")
library(simsalapar)
library(ggplot2)
require(asp21bpspline)

"Analysis of simulation21"

# result from simulation
res21 = maybeRead("simulation/simulation 2/test111")

# simulation object
simulation2 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 5),
  n = list(type = "grid", value = c(1000)),
  it = list(type = "frozen", value = 1500),
  knots = list(type = "frozen", value = c(40,40)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(3,3)),
  smooth =  list(type = "frozen", value = c(0,0)))

# plot (true values need to be obtained in a different way depending on the simulation and need to be
# provided in vector form with names true_loc and true_scale)
meanbeta = findmean(res21, 3)
meangamma = findmean(res21, 4)
pred_seq = seq(0, 20, length.out = 10000)
pred = predict_simulation(meanbeta, meangamma, simulation2, pred_seq)
pred = data.frame(x = pred_seq, loc = as.vector(pred$location), scale = as.vector(pred$scale), true_or_pred = rep("true", length(pred_seq)))

true_loc = loc_sim2_easy(pred_seq)
true_scale = scale_sim2_hard(pred_seq)
truth = data.frame(x = pred_seq, loc = true_loc, scale = true_scale, true_or_pred = rep("pred",length(pred_seq)))

truth_pred = rbind(truth, pred)
plot_simulation(truth_and_pred = truth_pred, sd = 1.96)

# percent unbiased
bias = biasSE(list(true_loc, true_scale), res21, MCMC = T, parameter = F, simulation2, x = pred_seq)

within_conf_int_loc = abs(bias$location[,1]) < 1.96 * bias$location[,2]
percent_unbiased_loc = mean(within_conf_int_loc)
percent_unbiased_loc

within_conf_int_scale = abs(bias$scale[,1]) < 1.96 * bias$scale[,2]
percent_unbiased_scale = mean(within_conf_int_scale)
percent_unbiased_scale

# mean MSE for predictions
mean_mse = mean_MSE(list(true_loc, true_scale), res21, MCMC = T, parameter = F, simulation2, x = pred_seq)
mean_mse_loc = mean_mse[1]  
mean_mse_scale = mean_mse[2]
mean_mse_loc
mean_mse_scale

"Analysis of simulation22"

# result from simulation
res22 = maybeRead("simulation/simulation 2/test112")

# simulation object
simulation2 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 5),
  n = list(type = "grid", value = c(1000)),
  it = list(type = "frozen", value = 1500),
  knots = list(type = "frozen", value = c(40,40)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(3,3)),
  smooth =  list(type = "frozen", value = c(0,0)))

# plot (true values need to be obtained in a different way depending on the simulation and need to be
# provided in vector form with names true_loc and true_scale)
meanbeta = findmean(res22, 3)
meangamma = findmean(res22, 4)
pred_seq = seq(0, 20, length.out = 10000)
pred = predict_simulation(meanbeta, meangamma, simulation2, pred_seq)
pred = data.frame(x = pred_seq, loc = as.vector(pred$location), scale = as.vector(pred$scale), true_or_pred = rep("true", length(pred_seq)))

true_loc = loc_sim2_hard(pred_seq)
true_scale = scale_sim2_easy(pred_seq)
truth = data.frame(x = pred_seq, loc = true_loc, scale = true_scale, true_or_pred = rep("pred",length(pred_seq)))

truth_pred = rbind(truth, pred)
plot_simulation(truth_and_pred = truth_pred, sd = 1.96)

# percent unbiased
bias = biasSE(list(true_loc, true_scale), res22, MCMC = T, parameter = F, simulation2, x = pred_seq)

within_conf_int_loc = abs(bias$location[,1]) < 1.96 * bias$location[,2]
percent_unbiased_loc = mean(within_conf_int_loc)
percent_unbiased_loc

within_conf_int_scale = abs(bias$scale[,1]) < 1.96 * bias$scale[,2]
percent_unbiased_scale = mean(within_conf_int_scale)
percent_unbiased_scale

# mean MSE for predictions
mean_mse = mean_MSE(list(true_loc, true_scale), res22, MCMC = T, parameter = F, simulation2, x = pred_seq)
mean_mse_loc = mean_mse[1]  
mean_mse_scale = mean_mse[2]
mean_mse_loc
mean_mse_scale

"Analysis of simulation23"

# result from simulation
res23 = maybeRead("simulation/simulation 2/test114")

# simulation object
simulation2 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 5),
  n = list(type = "grid", value = c(1000)),
  it = list(type = "frozen", value = 1500),
  knots = list(type = "frozen", value = c(40,40)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(3,3)),
  smooth =  list(type = "frozen", value = c(0,0)))

# plot (true values need to be obtained in a different way depending on the simulation and need to be
# provided in vector form with names true_loc and true_scale)
meanbeta = findmean(res23, 3)
meangamma = findmean(res23, 4)
pred_seq = seq(0, 20, length.out = 10000)
pred = predict_simulation(meanbeta, meangamma, simulation2, pred_seq)
pred = data.frame(x = pred_seq, loc = as.vector(pred$location), scale = as.vector(pred$scale), true_or_pred = rep("true", length(pred_seq)))

true_loc = loc_sim2_hard(pred_seq)
true_scale = scale_sim2_hard(pred_seq)
truth = data.frame(x = pred_seq, loc = true_loc, scale = true_scale, true_or_pred = rep("pred",length(pred_seq)))

truth_pred = rbind(truth, pred)
plot_simulation(truth_and_pred = truth_pred, sd = 1.96)

# percent unbiased
bias = biasSE(list(true_loc, true_scale), res23, MCMC = T, parameter = F, simulation2, x = pred_seq)

within_conf_int_loc = abs(bias$location[,1]) < 1.96 * bias$location[,2]
percent_unbiased_loc = mean(within_conf_int_loc)
percent_unbiased_loc

within_conf_int_scale = abs(bias$scale[,1]) < 1.96 * bias$scale[,2]
percent_unbiased_scale = mean(within_conf_int_scale)
percent_unbiased_scale

# mean MSE for predictions
mean_mse = mean_MSE(list(true_loc, true_scale), res23, MCMC = T, parameter = F, simulation2, x = pred_seq)
mean_mse_loc = mean_mse[1]  
mean_mse_scale = mean_mse[2]
mean_mse_loc
mean_mse_scale

