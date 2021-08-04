library(simsalapar)
# result from simulation
res = maybeRead("simulation/simulation 2/test99")

# simulation object
simulation2 =  varlist(
  n.sim = list(type = "N", expr = quote(N[sim]), value = 5),
  n = list(type = "grid", value = c(500)),
  it = list(type = "frozen", value = 1500),
  knots = list(type = "frozen", value = c(40,40)),
  order = list(type = "frozen", value = c(3, 3)),
  p_order = list(type = "frozen", value = c(3,3)),
  smooth =  list(type = "frozen", value = c(0,0)))

# 



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



# function to compute predictions for scale and location with posterior mean for each simulation (n.sim) and for each sample size of underlying data set (n)
sim2_compute_posterior_means = function(val) {
  number_components_parameter = dim(val)[1]
  number_posterior_samples = (dim(val)[2]-2)/2
  number_diff_datasizes = dim(val)[3]
  number_simulations = dim(val)[4]
  
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
  return(posterior_means)
}

sim2_posterior_means = sim2_compute_posterior_means(val)

# sim2_predict = function(sim2_posterior_means, simulation, nseq = 1000) {
#   pred_seq = seq(0, 20, length.out = nseq)
#   results = array(dim = c(nseq, 2, number_diff_datasizes, number_simulations))
# 
# 
#     for (j in 1:number_diff_datasizes) {
#       for (k in 1:number_simulations) {
#         location_coef = posterior_means[, 1, j, k]
#         scale_coef = posterior_means[, 2, j, k]
#         predictions = predict_simulation(location_coef, scale_coef, simulation, pred_seq)
#         results[, 1, j, k] = predictions$location
#         results[, 2, j, k] = predictions$scale
#       }
#     }
#   return(results)
# }

# predict scale and location with posterior mean for each simulation and for each sample size of underlying data set
sim2_prediction = sim2_predict(sim2_posterior_means, simulation2)

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
plot_simulation = function(truth_and_pred, sd = 1.96)
{
  ggplot2::ggplot(truth_and_pred, aes(x = x, colour = true_or_pred))+
    geom_line(aes(y = loc), size = 1.5)+
    geom_line(aes(y = loc + sd * scale), size = 1)+
    geom_line(aes(y = loc - sd * scale), size = 1)+
    scale_color_brewer(palette="Dark2")
  
}

# plot mean of predictions obtained from posterior mean
plot_predictions = function(sim2_prediction) {
  dim = dim(sim2_prediction)
  pred_seq = seq(0, 20, length.out = dim[1])
  true_loc = loc_sim2(pred_seq) # function from file do simulation 2.r
  true_scale = scale_sim2(pred_seq) # function from file do simulation 2.r
  truth = data.frame(x = pred_seq, loc = true_loc, scale = true_scale, true_or_pred = rep("true",length(pred_seq)))
  mean_predictions = apply(sim2_prediction, c(1, 2, 3), mean)
  
  for (i in 1:dim[3]) {
    mean_predictemp = mean_predictions[,,i]
    pred = data.frame(x = pred_seq, loc = mean_predictemp[,1], scale = mean_predictemp[,2], true_or_pred = rep("pred", length(pred_seq)))
    truth_and_pred = rbind(truth, pred)
    print(plot_simulation(truth_and_pred = truth_and_pred))
  }
  
}
require(ggplot2)
plot_predictions(sim2_prediction)



ggplot(df, aes(x = instance, y = total_hits)) +
  geom_point(size = 1) + 
  geom_line()+
  geom_line(aes(x=instance, y = line1, colour="myline1")) +
  geom_vline(xintercept=805) + 
  geom_line(aes(x=instance, y = line2, colour="myline2"))+
  geom_line(aes(x=instance, y = line3, colour="myline3")) +
  
  
  
  
  
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













