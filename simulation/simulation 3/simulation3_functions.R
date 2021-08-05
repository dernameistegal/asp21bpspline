
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
apply(res3[[3,3]]$value[,104:204], 1, quantile)


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

estimate_quantile_splines = function(res_zeile,x, quantile = c(0.025, 0.975)
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
ToNormal = function(reslist){
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

plot_simulation3 = function(est_mean, est_quant, x){
  data = data.frame(x = x, loc_mean = est_mean$location, 
                    loc_quant_lower = est_quant[1,,1],loc_quant_upper = est_quant[2,,1],
                    sc_mean = est_mean$scale,
                    sc_qu_low = est_quant[1,,2], sc_qu_upper = est_quant[2,,2])
  ggplot(data, aes(x = x)) + geom_line(aes(y = loc_mean))+
    geom_ribbon(aes(ymin=loc_quant_lower,ymax=loc_quant_upper),alpha=0.3)+
    geom_line(aes(y= loc_mean + 1.96 * sc_mean))+
    geom_line(aes(y= loc_mean - 1.96 * sc_mean))+
    geom_ribbon(aes(ymin=loc_mean - 1.96 * sc_qu_upper,ymax=loc_mean - 1.96 *sc_qu_low )
                ,alpha=0.3)+
    geom_ribbon(aes(ymin=loc_mean + 1.96 * sc_qu_low,ymax=loc_mean + 1.96 *sc_qu_upper )
                ,alpha=0.3)+
    labs(x = "predictor" , y = "mean and credible intevalls")
}
# Eine Simulationliste ist der Input
# Returned wird ob der MCMC Sampler für Gamma irgendwann stuck ist

FindStuck <- function(resij)
{
  n = (ncol(resij$value)-2) / 2
  quants = apply(resij$value[,(n+3):(2*n + 2)], 1, quantile, c(0,1))
  return(all(quants[1,] == quants[2,]))
}
#entfernt in einer Zeile alle Stucks, bzw. zeigt alle an die Stuck sind
CleanStuck = function(resi, remove = T){
  n = length(resi)
  stucks = c(rep(NA,n))
  for (j in 1:n)
  {
    stucks[j] = FindStuck(resi[[j]])
  }
  print(paste0(100 * sum(stucks)/(length(stucks)), "% sind stuck"))
  if (!remove)
  {
    return(stucks)
  }
  else
  {
    unstuck = list()
    for (j in 1:length(stucks))
    {
      if (!stucks[j])
      {
        unstuck = append(unstuck,resi[j])
      }
      else
      {
        next
      }
    }
    return(unstuck)
  }
}

