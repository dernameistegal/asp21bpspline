print.spline <- function(m, digits = max(3, getOption("digits") - 3), ...) {
  cat(
    "\nCall:\n",
    #paste(deparse(m$call), sep = "\n", collapse = "\n"),
    "\n\n",
    sep = ""
  )
  
  if (length(coef(m)$location)) 
  {
    cat("Location coefficients:\n")
    
    print.default(
      x = format(coef(m)$location, digits = digits),
      print.gap = 2,
      quote = FALSE
    )
  }
  
  else 
  {
    cat("No location coefficients\n")
  }
  
  cat("\n")
  
  if (length(coef(m)$scale)) 
  {
    cat("Scale coefficients:\n")
    
    print.default(
      x = format(coef(m)$scale, digits = digits),
      print.gap = 2,
      quote = FALSE
    )
  } 
  
  else 
  {
    cat("No scale coefficients\n")
  }
  
  cat("\n")
  invisible(m)
}


#Hier müssen noch x und y richtig definiert werden für den scatterplot
plot.spline <- function(m, sd = 1.96)
{
  data = data.frame(x = m$formerx, y = m$y,
                    ypred = m[["fitted.values"]]$location,
                    scalepred = m[["fitted.values"]]$scale)
  ggplot2::ggplot(data, mapping = aes(x = m$formerx)) +
    geom_point(aes(y = y)) +
    geom_line(aes(y = ypred), colour = "green", size = 2)+
    geom_line(aes(y = ypred + sd * scalepred), colour = "blue", size = 1)+
    geom_line(aes(y = ypred - sd * scalepred), colour = "blue", size = 1)+
    ylab("dependent variable")+
    xlab("explaining variable")
}

predict.spline = function(m, X, Z)
{
  beta = m$coefficients$location
  gamma = m$coefficients$scale
  
  location = X %f*f% as.matrix(beta)
  scale = exp(Z %f*f% as.matrix(gamma))
  return(list(location = location, scale =  scale))
}



summary.spline = function(model, par)
{
  #todo
  return()
}



plot.mcmcspline = function(m, sample, sd = 1.96)
{
  m$coefficients$location = colMeans(sample$beta)
  m$coefficients$scale = colMeans(sample$gamma)
  
  temp = predict.spline(m, m$loc$X, m$scale$Z)
  m[["fitted.values"]]$location = temp[[1]]
  m[["fitted.values"]]$scale = temp[[2]]
  plot.spline(m)
}


summary.mcmcspline = function(model, par)
{
  #todo
  return()
}

