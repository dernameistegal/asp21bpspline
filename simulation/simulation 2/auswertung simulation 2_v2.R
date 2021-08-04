source("simulation/simulation functions.R")

library(asp21bpspline)
library(simsalapar)
res20 = maybeRead("simulation/simulation 2/simulation2_test1")
val = getArray(res20)

dim(val)
# an der stelle n das array zerlegen, falls es NAs irgendwo gibt
whereNA(val)
###
val_250 = val[,,1,,drop = F]
val_250 = cleanNA(val_250)

val_rest = val[,,-1,,drop = F]
any(is.na(val_rest))
###
simulation2 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 5),
  n = list(type = "grid", value = c(1000)),
  it = list(type = "frozen", value = 1000),
  knots = list(type = "frozen", value = c(50,50)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(3,3)),
  smooth =  list(type = "frozen", value = c(0,0)))

x = seq(0,20, length.out = 100)
#### for 250 ####
est_250 = getEstimateSplines(val_250, simulation2, x)
quant_250  = getQuantiles(est_250)
any(is.na(est_250))
#### for rest ####
set.seed(1)
est_rest = getEstimateSplines(val_rest, simulation2, x)
quant_rest  = getQuantiles(est_rest)
any(is.na(est_rest))
unique(which(is.na(est_rest), arr.ind = T)[,-2])
#######################
dim(est_rest)
dim(quant_rest)
dim(est_250)
#1 0.025 quantil und 0.975 quantil 2 scale oder location3 anzahl der PUnkte 4 c(250)
dim(quant_250)

plot(x, quant_250[1,1,,1], type = "l", xlim = c(0,20), ylim = c(-5,20))
lines(x, quant_250[2,1,,1])

plot(x, quant_rest[1,1,,1], col = 2, type ="l")
lines(x, quant_rest[2,1,,1], col = 2)

lines(x, quant_rest[1,1,,2], col = 3)
lines(x, quant_rest[2,1,,2], col = 3)

