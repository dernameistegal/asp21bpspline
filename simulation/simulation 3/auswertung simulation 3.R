source("simulation/simulation functions_mainfolder.R")
library(simsalapar)
res3 = maybeRead("simulation/simulation 3/simulation3_test1")
a = res3[[1]]$value
ToNormal(res3[[1]]$value)
x = seq(0,20, length.out = 100)

estimations = getEstimateValues(res3[[1]]$value,simulation3, x)
dim(getQuantiles(estimations, quantile = c(0.8,0.7,0.9)))
# res_zeile Ist eine Zeile des Resultats
# x         sind die x Werte
# qunatile  selbsterklärend
# simulaiton Simulationsobjekt

#return dimension 1 sind die Quantile, dimension 2 sind die Stellen an
#denen geschätzt wurde, DImension drei sind location bzw. Scale

estimate_quantile_splines <- function(res_zeile,x, quantile = c(0.025, 0.975)
                                      ,simulation){
  # wie viele durchläufe hatte doOne für den Parameter der Grid
  n = length(res_zeile)
  data = array(data=NA, dim = c(n,length(quantile),length(x),2))
  for (i in 1:n){
    estimations = getEstimateValues(res_zeile[[i]]$value,simulation, x)
    estimations = getQuantiles(estimations, quantile)
    data[i,,,] = estimations
  }
  data = apply(data,MARGIN =c(2,3,4) ,FUN = mean )
  return(data)
}
b = estimate_quantile_splines(res3[1,], x, quantile = c(0.95),simulation = simulation3)
dim(b)
plot(x,b[1,,1], type = "l")
lines(x,b[2,,1], type = "l")

length(res3[1,])

dim(estimations)



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

