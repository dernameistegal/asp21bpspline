
# helper funktion für die simulation study, um confidence intervals für
# Grafiken zu erstellen

#input ist eine Liste des doOne mit MCMC, 2 Die Daten zur Simulation,
# an welchen Stellen geschätt werden soll

# returns 1 scale or location 2 value at specific x postion, 3 which iteration
getEstimateValues = function(reslist, simulation, x)
{
  # ermittle die anzahl der für mcmc relevanten Spalten
  iteration = (dim(reslist)[2] - 2)/2
  # entferne ML estimates
  reslist = reslist[,3:dim(reslist)[2]]
  # Dummy Matrix um ergebnisse zu speichen
  results = array(data = NA, dim = c( length(x),2, iteration))
  for (i in 1:iteration)
  {
    location_coef = reslist[,i]
    scale_coef = reslist[, iteration + i]
    predictions = predict_simulation(location_coef,
                                     scale_coef, simulation, x)
    results[ ,1,i] = predictions$location
    results[ ,2,i] = predictions$scale
  }
  return(results)
}




# spline_values        return der getEstimatesSplines funktion
# quantile             Welche Quantilswerte
# location             sollen location oder scale Werte abgerufen werden

# return               für jede simulation quantile der vorhersagen.
getQuantiles = function(spline_values, quantile = c(0.025, 0.975))
{
  
  #dier erste dimension geht wegen dem subsetting verloren
  quantiles = apply(X = spline_values, FUN = quantile, 
                    c(1,2), quantile)
  
  return(quantiles)
}

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
  pb = txtProgressBar(min = 0, max = n, style = 3)
  for (i in 1:n){
    estimations = getEstimateValues(res_zeile[[i]]$value,simulation, x)
    estimations = getQuantiles(estimations, quantile)
    data[i,,,] = estimations
    setTxtProgressBar(pb, i)
  }
  data = apply(data,MARGIN =c(2,3,4) ,FUN = mean )
  close(pb)
  return(data)
}


#zeigt an welche objekte 0 sind im val objekt
whereNA = function(val){
  elemente = which(is.na(val), arr.ind = T)[,c(3:4)]
  return(unique(elemente))
}
#haut alle NAs aus dem val objekt
cleanNA = function(val){
  nas = whereNA(val)
  if (length(nas) == 0){
    return(val)
  }
  for ( i in 1:nrow(nas)){
    #das minus i, damit die richtigen Zeilen rausgehauen werden
    val = val[,,nas[i,1],- (nas[i,2] - i + 1), drop = F]
  }
  return(val)
}
# # val ist ein getarray objekt
# whereNA(val)
# any(is.na(cleanNA(val)))
#nimmt ein Array mit MCMC schätzern und wandelt sie zu einem Array
#wo nur noch loc scale locmcmc und scalemcmc vorkommen um
ToNormal <- function(reslist){
  dimension = dim(reslist)
  nobs = (dimension[2] - 2)/2
  locmcmc = rowMeans(reslist[,3:nobs + 2])
  scalemcmc = rowMeans(reslist[,(nobs+3):dimension[2]])
  return(cbind(reslist[,1:2],locmcmc,scalemcmc))
}
#erweiterung der ToNormal funktion auf einen ganzen Vektor des Ergebnisses
all_ToNormal = function(resline){
  for (i in 1:length(resline)){
    resline[[i]]$value = ToNormal(resline[[i]]$value)
  }
  return(resline)
}
