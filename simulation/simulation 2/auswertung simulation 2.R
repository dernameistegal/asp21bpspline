library(simsalapar)
res20 = maybeRead("simulation/simulation 2/simulation2_test1")
val = getArray(res20)
val250 = val[,,1,,drop = F]
val = val[,,c(2,3),,drop = F]

any(is.na(val))

simulation2 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 50),
  n = list(type = "grid", value = c(250,500,1000)),
  it = list(type = "frozen", value = 1500),
  knots = list(type = "frozen", value = c(40,40)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(3,3)),
  smooth =  list(type = "frozen", value = c(0,0)))

# array2df(getArray(res20))
# str(getArray(res20))
dim(val)
# 1: number of components in parameter vector, 2: (n-2)/2 samples from posterior for beta and gamma and one MLE estimate for beta and gamma, 
# 3: number of different amounts of available data, 4: number of simulations
#spline loacation

# rowMeans(val[,1,1,])
# #spline scale
# rowMeans(val[,2,1,])
# #mcmc location
# rowMeans(val[,3,1,])
# #mcmc scale
# rowMeans(val[,4,1,])

posterior_mean_prediction = function(val, simulation, nseq) {
  number_components_parameter = dim(val)[1]
  number_posterior_samples = (dim(val)[2]-2)/2
  number_diff_datasizes = dim(val)[3]
  number_simulations = dim(val)[4]
  pb = txtProgressBar(min = 0, max = number_simulations, initial = 0,  style = 3) 
  
  posterior_means = array(dim = c(number_components_parameter, 2, number_diff_datasizes, number_simulations))
  
  for (i in 1:2) {
    for (j in 1:number_diff_datasizes) {
      for (k in 1:number_simulations) {
      ind1 = 3 + (number_posterior_samples) * (i - 1)
      ind2 = 2 + (number_posterior_samples) * i
      posterior_means[,i,j,k] = rowMeans(val[,ind1:ind2,j,k])
      }
    }
  }
  
  pred_seq = seq(0, 20, length.out = nseq)
  results = array(dim = c(nseq, 2, number_diff_datasizes, number_simulations))
  

    for (j in 1:number_diff_datasizes) {
      for (k in 1:number_simulations) {
        location_coef = posterior_means[, 1, j, k]
        scale_coef = posterior_means[, 2, j, k]
        predictions = predict_simulation(location_coef, scale_coef, simulation, pred_seq)
        results[, 1, j, k] = predictions$location
        results[, 2, j, k] = predictions$scale
      }
      setTxtProgressBar(pb,k)
    }
  close(pb)
  return(results)
}

predicted_y = posterior_mean_prediction(val, simulation2, nseq = 100)
dim(predicted_y)

true_loc = -0.0004*pred_seq^4 + 0.005* pred_seq^3 - 0.05*pred_seq^2 + 2*pred_seq + 4*sin(pred_seq)
true_scale = (2.1 + 2*sin(x) + x^2/200)
true_values = array(true_y, dim = dim(predicted_y))
dim(true_y)
true_y


mean = -0.0004*x^4 + 0.005* x^3 - 0.05*x^2 + 2*x + 4*sin(x)
sd = (2.1 + 2*sin(x) + x^2/200)






getEstimateSplines = function(val, simulation, x)
{
  # ermittle die anzahl der für mcmc relevanten Spalten
  dimension = dim(val)
  iterations = (dimension[2] - 2)/2
  
  pb = txtProgressBar(min = 0, max = dimension[4], initial = 0,  style = 3) 
  
  # entferne ML estimates
  val = val[,3:dimension[2], , , drop = F]
  
  # Dummy Matrix um ergebnisse zu speichen
  results = array(data = NA, dim = c(2,length(x), iterations, 
                                     dimension[3], dimension[4]))
  
  for (k in 1:dimension[4])
  {
    for (j in 1:dimension[3])
    {
      for (i in 1:iterations)
      {
        location_coef = val[,i, j, k]
        scale_coef = val[, iterations + i, j, k]
        predictions = predict_simulation(location_coef, scale_coef, simulation, x)
        results[, 1, i, j, k] = predictions$location
        results[, 2, i, j, k] = predictions$scale
      }
    }
    setTxtProgressBar(pb,k)
  }
  close(pb)
  return(results)
}

    



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
        location_coef = val[,i,j,k]
        scale_coef = val[, nr_diff_obs + i , j, k]
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
getQunatiles = function(spline_values, quanti = c(0.025, 0.975), location = T){
  if (location){
    location = 1
  }else{
    location = 2
  }
  #dier erste dimension geht wegen dem subsetting verloren
  quantiles = apply(X = spline_values[location,,,,,drop = F], FUN = quantile, c(1,2,4,5), quanti)
  return(quantiles)
}
x <- seq(0,20, length.out = 1000)
spline_values  = getEstimateSplines(val, simulation2, x)
quantile_values = getQunatiles(spline_values)


dim(spline_values)
dim(quantile_values)

quantile_functions = list()
for (i in (0:10) * 10){
  quantile_functions[[(i/10) + 1]] =  getQunatiles(spline_values, quantile = i/100)[1,,1,1]
}

dim(quantile_values)



splines_result <- predict_simulation(rowMeans(val[,5,1,]),rowMeans(val[,2,1,]), simulation2,x )
mcmc_result <- predict_simulation((val[,4,1,20]),(val[,4,1,20]), simulation2, x )


# wie weit liegen die Werte von den wahren Werten weg?
plot(x,type = "l", -0.0004*x^4 + 0.005* x^3 - 0.05*x^2 + 2*x + 4*sin(x))
lines(x, quantile_values[2,1,,1,1], col = 2)
lines(x, quantile_values[2,1,,2,1], col = 3)
lines(x, quantile_functions[[10]], col = 3)
#lines(x,type = "l", -0.04*x^2 + 0.5* x + 2*(0.05 *x + 0.3), col = "blue")
#lines(x,type = "l", -0.04*x^2 + 0.5* x - 2*(0.05 *x + 0.3), col = "blue")
for (i in 1:11){
  lines(x, quantile_functions[[i]], col = i)
}

#lines(x, spline_Werte[[1]] + 2 * spline_Werte[[2]], col = "red")
#lines(x, spline_Werte[[1]] - 2 * spline_Werte[[2]], col = "red")
lines(x, mcmc_result[[1]], col = "Blue")

