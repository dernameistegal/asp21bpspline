library(simsalapar)
res20 = maybeRead("simulation/simulation 2/simulation2_test1")
val = getArray(res20)
val_withNA = val[,,1,,drop = F] # check of NA values were simulated (not final)
val = val[,,c(2,3),,drop = F] # check of NA values were simulated (not final)
any(is.na(val)) # check of NA values were simulated (not final)

# just copied from do simulation 2.r
simulation2 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 50),
  n = list(type = "grid", value = c(250,500,1000)),
  it = list(type = "frozen", value = 1500),
  knots = list(type = "frozen", value = c(40,40)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(3,3)),
  smooth =  list(type = "frozen", value = c(0,0)))

# 1: number of components in parameter vector, 2: (n-2)/2 samples from posterior for beta and gamma and one MLE estimate for beta and gamma, 
# 3: number of different amounts of available data, 4: number of simulations
dim(val)

# function to compute predictions for scale and location with posterior mean for each simulation (n.sim) and for each sample size of underlying data set (n)
sim2_predict = function(val, simulation, nseq) {
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

# predict scale and location with posterior mean for each simulation and for each sample size of underlying data set
sim2_prediction = sim2_predict(val, simulation2, nseq = 100)

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

# plot simulation function from simulation 1 (not changed)
plot_simulation = function(predictions, truth, x)
{
  sd = 1.96
  data = data.frame(x = x,
                    ypred = predictions[[1]],
                    scalepred = predictions[[2]],
                    ytrue = truth[[1]],
                    scaletrue = truth[[2]])
  ggplot2::ggplot(data, mapping = aes(x = x)) +
    geom_line(aes(y = ypred), colour = "brown4", size = 1)+
    geom_line(aes(y = ypred + sd * scalepred), colour = "brown3", size = 0.5)+
    geom_line(aes(y = ypred - sd * scalepred), colour = "brown3", size = 0.5)+
    geom_line(aes(y = ytrue), colour = "dodgerblue4", size = 1)+
    geom_line(aes(y = y_true + sd * scaletrue), colour = "dodgerblue3", size = 0.5)+
    geom_line(aes(y = y_true - sd * scaletrue), colour = "dodgerblue3", size = 0.5)+
    ylab("dependent variable")+
    xlab("explaining variable")
}

# plot mean of predictions obtained from posterior mean
plot_predictions = function(sim2_prediction) {
  dim = dim(sim2_prediction)
  mean_predictions = apply(sim2_prediction, c(1, 2, 3), mean)
  for (i in 1:dim[3]) {
    mean_predictemp = mean_predictions[,,i]
    mean_predictemp = list(loc = mean_predictemp[,1], scale = mean_predictemp[,2])
    pred_seq = seq(0, 20, length.out = dim(sim2_prediction)[1])
    print(plot_simulation(predictions = mean_predictemp, x = pred_seq))
    }
  
}
require(ggplot2)
plot_predictions(sim2_prediction)



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

