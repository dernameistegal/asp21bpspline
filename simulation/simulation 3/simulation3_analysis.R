source("simulation/simulation functions_mainfolder.R")
source("simulation/analysis_functions.R")
library(simsalapar)
res3 = maybeRead("simulation/simulation 3/simulation3_test1")
a = res3[[1]]$value


normal_res = all_ToNormal(res3[1,])
findmea

x = seq(0,20, length.out = 100)
findmean(res3[1,])
estimations = getEstimateValues(res3[[1]]$value,simulation3, x)
dim(getQuantiles(estimations, quantile = c(0.8,0.7,0.9)))

est_normal = estimate_quantile_splines(res3[1,], x, quantile = c(0.1,0.9),simulation = simulation3)
est_random = estimate_quantile_splines(res3[2,], x, quantile = c(0.1,0.9),simulation = simulation3)
est_error = estimate_quantile_splines(res3[3,], x, quantile = c(0.1,0.9),simulation = simulation3)

plot(x,est_normal[1,,1], type = "l")
lines(x,est_normal[2,,1], type = "l")
lines(x,est_random[1,,1], type = "l", col = 2)
lines(x,est_random[2,,1], type = "l", col = 2)
lines(x,est_error[1,,1], type = "l", col = 3)
lines(x,est_error[2,,1], type = "l", col = 3)



