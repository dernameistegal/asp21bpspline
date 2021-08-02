library(simsalapar)
res20 = maybeRead("simulation/simulation 2/t_4")
val = getArray(res20)

simulation2 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 5),
  n = list(type = "grid", value = c(1000)),
  it = list(type = "frozen", value = 1000),
  knots = list(type = "frozen", value = c(50,50)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(3,3)),
  smooth =  list(type = "frozen", value = c(0,0)))

array2df(getArray(res20))
str(getArray(res20))
dim(val)
# 1 number of splines 2 number of predictet columns 3 wie viele  unterschiedliche Punkte  4 wie viele simulationen pro ding
#spline loacation
rowMeans(val[,1,1,])
#spline scale
rowMeans(val[,2,1,])
#mcmc location
rowMeans(val[,3,1,])
#mcmc scale
rowMeans(val[,4,1,])


##### ab hier hat Valentin was gemacht#####
x <- seq(0,20, length.out = 1000)
predict_simulation <- function(location_coef, scale_coef, simulation, x){
  m = list()
  class(m) = "spline"
  m$coefficients$location = location_coef
  m$coefficients$scale = scale_coef
  m$loc$knots = simulation[["knots"]]$value[1]
  m$loc$order = simulation[["order"]]$value[1]
  m$scale$knots= simulation[["knots"]]$value[2]
  m$scale$order = simulation[["order"]]$value[2]
  spline_Werte <- predict(m, x, x)
  return(spline_Werte)
}
#val            Ergebnisse der durchgeführten Simulation als array
#simulation     name der durchgeführten Simulation
# x             an welchen stellen sollen die x werte betrachtet werden
getEstimateSplines = function(val, simulation, x){
  dimension = dim(val)
  nr_diff_obs = (dimension[2] - 2)/2
  #die zwei ist für die location und die scale parameter
  pb = txtProgressBar(min = 0, max = dimension[4], initial = 0,  style = 3) 
  results = array(data = NA, dim = c(2,length(x), nr_diff_obs, dimension[3], dimension[4]))
  for ( k in 1:dimension[4]){
    for (j in 1:dimension[3]){
      for (i in 1 : nr_diff_obs){
        location_coef = val[,i,1,1]
        scale_coef = val[, nr_diff_obs + i , 1, 1]
        predictions = predict_simulation(location_coef, scale_coef, simulation, x)
        results[1, ,i,j, k] = predictions[[1]]
        results[2, ,i,j, k] = predictions[[2]]
      }
    }
    setTxtProgressBar(pb,k)
  }
  #dimnames(results) = c("sca/loc", "y", "obs", "n", "n.sim")
  close(pb)
  return(results)
}
dim(spline_values)
# spline_values       y Werte der geschätzten Splines für alle Dimensionen
#qunatile             Welche Quantilswerte
#location             sollen location oder scale Werte abgerufen werden

#return               quantiles of location wise spline
getQunatiles = function(spline_values, quantile = 0.95, location = T){
  if (location){
    location = 1
  }else{
    location = 2
  }
  #dier erste dimension geht wegen dem subsetting verloren
  quantiles = apply(X = spline_values[location,,,,,drop = F], FUN = quantile, c(1,2,4,5), quantile)
  return(quantiles)
}

spline_values  = getEstimateSplines(val, simulation2, x)
quantile_values = getQunatiles(spline_values)
dim(spline_values)
dim(quantile_values)

quantile_functions = list()
for (i in (0:10) * 10){
  quantile_functions[[(i/10) + 1]] =  getQunatiles(spline_values, quantile = i/100)[1,,1,1]
}




splines_result <- predict_simulation(rowMeans(val[,5,1,]),rowMeans(val[,2,1,]), simulation2,x )
mcmc_result <- predict_simulation((val[,4,1,20]),(val[,4,1,20]), simulation2, x )


# wie weit liegen die Werte von den wahren Werten weg?
plot(x,type = "l", -0.0004*x^4 + 0.005* x^3 - 0.05*x^2 + 2*x + 6*sin(x))
lines(x, quantile_functions[[2]], col = 2)
lines(x, quantile_functions[[10]], col = 3)
#lines(x,type = "l", -0.04*x^2 + 0.5* x + 2*(0.05 *x + 0.3), col = "blue")
#lines(x,type = "l", -0.04*x^2 + 0.5* x - 2*(0.05 *x + 0.3), col = "blue")
for (i in 1:11){
  lines(x, quantile_functions[[i]], col = i)
}

#lines(x, spline_Werte[[1]] + 2 * spline_Werte[[2]], col = "red")
#lines(x, spline_Werte[[1]] - 2 * spline_Werte[[2]], col = "red")
lines(x, mcmc_result[[1]], col = "Blue")

