
library(simsalapar)
simulation3 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 4),
  init = list(type = "grid", value = c("normal", "random" ,"error")),
  n = list(type = "frozen", value = 1000),
  it = list(type = "frozen", value = 1500),
  beta = list(type = "frozen", expr = quote(beta), value = beta),
  gamma = list(type = "frozen", expr = quote(gamma), value = gamma),
  knots = list(type = "frozen", value = c(40,40)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(3,3)),
  smooth =  list(type = "frozen", value = c(0,0)),
  burning =  list(type = "frozen", value = 500),
  thinning =  list(type = "frozen", value = 10))
source("simulation/general functions/analysis_functions.R")
source("simulation/general functions/generic_simulation_functions.R")
source("simulation/simulation 3/simulation3_functions.R")
RemoveErrors(res3[3,])

result_normal = maybeRead("simulation/simulation 3/simulation3_normal_seed500")
result_random = maybeRead("simulation/simulation 3/simulation3_random_500")
result_error = maybeRead("simulation/simulation 3/simulation3_error_500")

# res3 = maybeRead("simulation/simulation 3/simulation3_test13")
# result_normal =  res3[1,]
# result_random =  res3[2,]
# result_error =   res3[3,]


result_normal[[1]]$value
library(ggplot2)

x <- seq(0,20, length.out = 100)
# Remove all with any errror
res_noerror_normal = RemoveErrors(result_normal)
res_noerror_random = RemoveErrors(result_random)
res_noerror_error = RemoveErrors(result_error)

#remove stuck mcmcs
clean_normal = CleanStuck(res_noerror_normal)
clean_random = CleanStuck(res_noerror_random)
clean_error = CleanStuck(res_noerror_error)

#object gets transformed to normal simualtion objects like in simulation 2 and 1
res_normal = all_ToNormal(resline = clean_normal)
res_random = all_ToNormal(resline = clean_random)
res_error = all_ToNormal(resline = clean_error)

# estimations for the mean of Scale and location for the mcmcs are made
est_mean_normal = predict_simulation(findmean(res_normal, 3),findmean(res_normal,4),simulation3,x)
est_mean_random = predict_simulation(findmean(res_random, 3),findmean(res_random,4),simulation3,x)
est_mean_error = predict_simulation(findmean(res_error, 3),findmean(res_error,4),simulation3,x)

#estimations for the pointwise quantiles for mcmc are made 
est_quantile_normal = estimate_quantile_splines(clean_normal, x, quantile = c(0.1,0.9),simulation = simulation3)
est_quantile_random = estimate_quantile_splines(clean_random, x, quantile = c(0.1,0.9),simulation = simulation3)
est_quantile_error = estimate_quantile_splines(clean_error, x, quantile = c(0.1,0.9),simulation = simulation3)

#the functions are plotted
normal_plot = plot_simulation3(est_mean_normal, est_quantile_normal,x)
random_plot = plot_simulation3(est_mean_random, est_quantile_random,x)
error_plot = plot_simulation3(est_mean_error, est_quantile_error,x)
ggsave("simulation/plots/sim3/normal_plot.pdf",plot =normal_plot,device = "pdf",width = 7, height = 5)
ggsave("simulation/plots/sim3/random_plot.pdf",plot =random_plot,device = "pdf",width = 7, height = 5)
ggsave("simulation/plots/sim3/error_plot.pdf",plot =error_plot,device = "pdf",width = 7, height = 5)

#   
# a = predict_simulation(beta, gamma,simulation3, x )
# ggplot(mapping = aes(x = x)) + geom_line(aes(y = a[[1]])) + geom_line(aes(y = a[[1]]+ 1.96*a[[2]]))+
#    geom_line(aes(y = a[[1]]- 1.96*a[[2]]))
# 
