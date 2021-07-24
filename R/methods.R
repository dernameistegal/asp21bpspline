#Hier müssen noch x und y richtig definiert werden für den scatterplot
print.spline <- function(m, sd = 1.96)
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

predict.spline = function(beta, gamma, X)
{
  
  
  location = X %f*f% as.matrix(beta)
  scale = exp(X %f*f% as.matrix(gamma))
  return(list(location = location, scale =  scale))
}

plot.spline = function(model, par)
{
  #todo
  return()
}


summary.spline = function(model, par)
{
  #todo
  return()
}



plot.mcmcspline = function(model, par)
{
  #todo
  return()
}


summary.mcmcspline = function(model, par)
{
  #todo
  return()
}

