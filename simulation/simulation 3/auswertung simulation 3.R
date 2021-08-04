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
plot_simulation3 = function(est_mean, est_quantile, x){
  data = data.frame(x = x, loc_mean = est_mean[[1]], loc_quantile_lower = est_quantile[1,,1],
                    loc_quantile_upper = est_quantile[2,,1])
  ggplot(data, aes(x = x)) + geom_line(aes(y = loc_mean))+
    geom_line(aes(y = loc_quantile_lower))+
    geom_line(aes(y = loc_quantile_upper))
}
x = seq(0,20, length.out = 1000)
plot_simulation3(est_mean_normal, est_normal,x)


est_mean_normal = predict_simulation(findmean(res3[1,], 3),findmean(res3[1,],4),simulation3,x)
est_mean_random = predict_simulation(findmean(res3[2,], 3),findmean(res3[2,],4),simulation3,x)
est_mean_error = predict_simulation(findmean(res3[3,], 3),findmean(res3[3,],4),simulation3,x)

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



