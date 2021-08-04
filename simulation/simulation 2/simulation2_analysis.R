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

#compute bias and standard error
biasSE(list(true_loc, true_scale), res20, MCMC = T, parameter = F, simulation2, x = pred_seq)

#HIER WEITER MACHEN
# bias = biasSE(list(true_loc, true_scale), res20, MCMC = F, parameter = F, simulation2, x = pred_seq)
# abs(bias$location[,1]) > bias$location[,2] * 1.96
# abs(bias$scale[,1]) > bias$scale[,2] * 1.96
# data.frame(pred_seq, bias = bias$location[,1],se196= bias$location[,2] * 1.96, biased= abs(bias$location[,1]) > bias$location[,2] * 1.96)

# checking for bias in parameters
bias = biasSE_parameters(beta, gamma, get.n.sim(simulation1), res10, MCMC = F)
abs(bias$beta[,1]) > bias$beta[,2] * 1.96
abs(bias$gamma[,1]) > bias$gamma[,2] * 1.96

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

# n anzahl simu, len anzahl parameterkomponenten, j = 3 (locmcmc) j = 4 scalemcmc, res = res)
findmean = function(len, n, res, j)
{
  
  vector = matrix(0, nrow = len, ncol = n)
  
  for (i in 1:n)
  {
    vector[,i] = res[[i]]$value[,j]
  }
  mean = rowSums(vector) / n
  return(mean)
}

"probably bullshit from here. do tommorow
"

# function to compute the sample bias for scale and location with predictions obtained by posterior mean
sim2_compute_bias = function(sim2_prediction) {
  pred_seq = seq(0, 20, length.out = dim(sim2_prediction)[1])
  true_values = array(dim = dim(sim2_prediction))
  true_loc = loc_sim2(pred_seq) # function from file do simulation 2.r
  true_scale = scale_sim2(pred_seq) # function from file do simulation 2.r
  true_values[,1,,] = true_loc
  true_values[,2,,] = true_scale
  bias = sim2_prediction - true_values
  bias = apply(bias, c(1,2,3), sum)
  bias = bias/dim(sim2_prediction)["n.sim"]
  return(bias) 
}

# compute sample bias for scale and location with predictions obtained by posterior mean
sim2_bias = sim2_compute_bias(sim2_prediction)

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