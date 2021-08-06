
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

#nimmt ein Array mit MCMC schätzern und wandelt sie zu einem Array
#wo nur noch loc scale locmcmc und scalemcmc vorkommen um
ToNormal = function(resij){
  dimension = dim(resij)
  nobs = (dimension[2] - 2)/2
  locmcmc = rowMeans(resij[,3:nobs + 2])
  scalemcmc = rowMeans(resij[,(nobs+3):dimension[2]])
  return(cbind(resij[,1:2],locmcmc,scalemcmc))
}
#erweiterung der ToNormal funktion auf einen ganzen Vektor des Ergebnisses
all_ToNormal = function(resline){
  for (i in 1:length(resline)){
    resline[[i]]$value = ToNormal(resline[[i]]$value)
  }
  return(resline)
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

