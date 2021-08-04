source("simulation/general functions/analysis_functions.R")
source("simulation/general functions/generic_simulation_functions.R")
source("simulation/simulation 3/simulation3_functions.R")
library(simsalapar)
res3 = maybeRead("simulation/simulation 3/simulation3_test1")
a = res3[[1]]$value


normal_res = all_ToNormal(res3[1,])

predict_simulation()
findmean(res3[1,],3)
findmean(res3[1,],4)

# est_mean      predictions für den mean
# est_quantile  predictions für die Quantile
library(ggplot2)
plot_simulation3 = function(est_mean, est_quant, x){
  data = data.frame(x = x, loc_mean = est_mean$location, 
                    loc_quant_lower = est_quant[1,,1],loc_quant_upper = est_quant[2,,1],
                    sc_mean = est_mean$scale,
                    sc_qu_low = est_quant[1,,2], sc_qu_upper = est_quant[2,,2])
  ggplot(data, aes(x = x)) + geom_line(aes(y = loc_mean))+
    geom_ribbon(aes(ymin=loc_quant_lower,ymax=loc_quant_upper),alpha=0.3)+
    geom_line(aes(y= loc_mean + 1.96 * sc_mean))+
    geom_line(aes(y= loc_mean - 1.96 * sc_mean))+
    geom_ribbon(aes(ymin=loc_mean - 1.96 * sc_qu_upper,ymax=loc_mean - 1.96 *sc_qu_low )
                ,alpha=0.3)+
    geom_ribbon(aes(ymin=loc_mean + 1.96 * sc_qu_low,ymax=loc_mean + 1.96 *sc_qu_upper )
                ,alpha=0.3)
}

est_mean_normal
x = seq(0,20, length.out = 100)
plot_simulation3(est_mean_normal, est_normal,x)
plot_simulation3(est_mean_random, est_random,x)
plot_simulation3(est_mean_error, est_error,x)
a = predict_simulation(beta, gamma,simulation3, x )
ggplot(mapping = aes(x = x)) + geom_line(aes(y = a[[1]])) + geom_line(aes(y = a[[1]]+ 1.96*a[[2]]))+
   geom_line(aes(y = a[[1]]- 1.96*a[[2]]))


x <- seq(0,20, length.out = 100)
res_normal = all_ToNormal(resline = res3[1,])
res_random = all_ToNormal(resline = res3[2,])
res_error = all_ToNormal(resline = res3[3,])
est_mean_normal = predict_simulation(findmean(res_normal, 3),findmean(res_normal,4),simulation3,x)
est_mean_random = predict_simulation(findmean(res_random, 3),findmean(res_random,4),simulation3,x)
est_mean_error = predict_simulation(findmean(res_error, 3),findmean(res_error,4),simulation3,x)

est_normal = estimate_quantile_splines(res3[1,], x, quantile = c(0.1,0.9),simulation = simulation3)
est_random = estimate_quantile_splines(res3[2,], x, quantile = c(0.1,0.9),simulation = simulation3)
est_error = estimate_quantile_splines(res3[3,], x, quantile = c(0.1,0.9),simulation = simulation3)

plot(x,est_normal[1,,1], type = "l")
lines(x,est_normal[2,,1], type = "l")
lines(x,est_mean_normal$location, type = "l")


lines(x,est_random[1,,1], type = "l", col = 2)
lines(x,est_random[2,,1], type = "l", col = 2)
lines(x,est_error[1,,1], type = "l", col = 3)
lines(x,est_error[2,,1], type = "l", col = 3)



