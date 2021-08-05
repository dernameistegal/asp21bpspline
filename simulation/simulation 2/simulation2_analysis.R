source("simulation/general functions/analysis_functions.R")
source("simulation/general functions/generic_simulation_functions.R")
source("simulation/simulation 2/simulation2_functions.R")
library(simsalapar)
library(ggplot2)
require(asp21bpspline)

# result from simulation
res20 = maybeRead("simulation/simulation 2/test99")

# simulation object
simulation2 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 5),
  n = list(type = "grid", value = c(500)),
  it = list(type = "frozen", value = 1500),
  knots = list(type = "frozen", value = c(40,40)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(3,3)),
  smooth =  list(type = "frozen", value = c(0,0)))

# plot (true values need to be obtained in a different way depending on the simulation and need to be
# provided in vector form with names true_loc and true_scale)
meanbeta = findmean(res20, 3)
meangamma = findmean(res20, 4)
pred_seq = seq(0, 20, length.out = 10000)
pred = predict_simulation(meanbeta, meangamma, simulation2, pred_seq)
pred = data.frame(x = pred_seq, loc = as.vector(pred$location), scale = as.vector(pred$scale), true_or_pred = rep("true", length(pred_seq)))

true_loc = loc_sim2(pred_seq)
true_scale = scale_sim2(pred_seq)
truth = data.frame(x = pred_seq, loc = true_loc, scale = true_scale, true_or_pred = rep("pred",length(pred_seq)))

truth_pred = rbind(truth, pred)
plot_simulation(truth_and_pred = truth_pred, sd = 1.96)

# percent unbiased
bias = biasSE(list(true_loc, true_scale), res20, MCMC = T, parameter = F, simulation2, x = pred_seq)

within_conf_int_loc = abs(bias$location[,1]) < 1.96 * bias$location[,2]
percent_unbiased_loc = mean(within_conf_int_loc)
percent_unbiased_loc

within_conf_int_scale = abs(bias$scale[,1]) < 1.96 * bias$scale[,2]
percent_unbiased_scale = mean(within_conf_int_scale)
percent_unbiased_scale

# mean MSE for predictions
mean_mse = mean_MSE(list(true_loc, true_scale), res20, MCMC = T, parameter = F, simulation2, x = pred_seq)
mean_mse_loc = mean_mse[1]  
mean_mse_scale = mean_mse[2]
mean_mse_loc
mean_mse_scale


"probably bullshit from here. do tommorow
"





# function to compute the monte carlo standard error of the sample bias
sim2_compute_bias_sd = function(sim2_prediction) {
  dim = dim(sim2_prediction)
  mean_prediction = apply(sim2_prediction, c(1,2,3), mean)
  mean_prediction_temp = array(dim = dim)
  
  for (i in 1:dim[4]) {
    mean_prediction_temp[,,,i] = mean_prediction
  }
  
  mean_prediction = mean_prediction_temp
  
  temp = (sim2_prediction - mean_prediction)^2
  temp = temp/(dim[4] * (dim[4] - 1))
  temp = apply(temp, c(1,2,3), sum)
  temp = sqrt(temp)
  
  result = temp
  return(result)
}

# compute monte carlo SE of bias
sim2_bias_sd = sim2_compute_bias_sd(sim2_prediction)

# see if bias is significant
abs(sim2_bias) < sim2_bias_sd * 1.96