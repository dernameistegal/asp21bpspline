library(simsalapar)

# predictions for location and scale new x values
# simulation is the variable list
predict_simulation = function(beta, gamma, simulation, x)
{
  m = list()
  class(m) = "spline"
  m$coefficients$location = beta
  m$coefficients$scale = gamma
  m$loc$knots = simulation[["knots"]]$value[1]
  m$loc$order = simulation[["order"]]$value[1]
  m$scale$knots= simulation[["knots"]]$value[2]
  m$scale$order = simulation[["order"]]$value[2]
  predictions = predict(m, x, x)
  return(predictions)
}


# helper funktion für die simulation study, um confidence intervals für
# Grafiken zu erstellen

#val            Ergebnis der durchgeführten Simulation als array
# 1. Dimension länge des beta/gamma vectors
# 2. Dimension anzahl zurückgegebener Werte
#              hier Beta_ML, Gamma_ML, Beta_MCMC, Gamma_MCMC
# MCMC spalten können auch aus der gesamten ECDF bestehen
# 3. Dimension Elemente im Grid, Eintrag hier: n = 1000
# 4. Dimension Anzahl der Simuliationen

#simulation     varlist der durchgeführten simulation
# x             X vektor für predictions.

# returns predicted location and scale parameters for every simulation and iteration
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
        results[1, ,i,j, k] = predictions$location
        results[2, ,i,j, k] = predictions$scale
      }
    }
    setTxtProgressBar(pb,k)
  }
  close(pb)
  return(results)
}




# spline_values        return der getEstimatesSplines funktion
# quantile             Welche Quantilswerte
# location             sollen location oder scale Werte abgerufen werden

# return               für jede simulation quantile der vorhersagen.
getQuantiles = function(spline_values, quantile = c(0.025, 0.975), location = T)
{
  if (location)
  {
    location = 1
  }
  else
  {
    location = 2
  }
  
  
  #dier erste dimension geht wegen dem subsetting verloren
  quantiles = apply(X = spline_values[location,,,,,drop = F], FUN = quantile, 
                    c(1,2,4,5), quantile)
  
  quantiles = apply(quantiles, FUN = mean, c(1,2,3))
  return(quantiles)
}


#zeigt an welche objekte 0 sind im val objekt
whereNA = function(val){
  elemente = which(is.na(val), arr.ind = T)[,c(3:4)]
  return(unique(elemente))
}
#haut alle NAs aus dem val objekt
cleanNA = function(val){
  nas = whereNA(val)
  for ( i in 1:nrow(nas)){
    #das minus i, damit die richtigen Zeilen rausgehauen werden
    val = val[,,nas[i,1],- (nas[i,2] - i + 1), drop = F]
  }
  return(val)
}
# # val ist ein getarray objekt
# whereNA(val)
# any(is.na(cleanNA(val)))