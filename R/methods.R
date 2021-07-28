#' print method for spline objects
#'
#' @param m 
#' @param digits 
#' @param ... 
#'
#' @return
#' @export
#'
#' 
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


#' plot method for spline objects
#'
#' @param m 
#' @param sd
#'
#' @return
#' @export
#'
#' 
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


#' predict method for spline objects
#'
#' @param m 
#' @param X 
#' @param Z 
#'
#' @return
#' @export
#'
#' 
predict.spline = function(m, X, Z)
{
  beta = m$coefficients$location
  gamma = m$coefficients$scale
  
  location = X %f*f% as.matrix(beta)
  scale = exp(Z %f*f% as.matrix(gamma))
  return(list(location = location, scale =  scale))
}


#' summary method for spline objects
#'
#' @param model 
#' @param par 
#'
#' @return
#' @export
#'
#' 
summary.spline = function(model, par)
{
  #todo
  return()
}


#' predict method for spline objects
#'
#' @param m 
#' @param sample 
#'
#' @return
#' @export
#'
#' 
predict.mcmcspline = function(m, sample)
{
  m$coefficients$location = colMeans(sample$beta)
  m$coefficients$scale = colMeans(sample$gamma)
  
  return(predict.spline(m, m$loc$X, m$scale$Z))
}



#' plot function for MCMC samples of a spline model
#'
#' @param m 
#' @param sample 
#' @param sd 
#'
#' @return
#' @export
#'
#' 
plot.mcmcspline = function(m, sample)
{
  predict.mcmcspline(m, sample)
  m[["fitted.values"]]$location = temp[[1]]
  m[["fitted.values"]]$scale = temp[[2]]
  plot.spline(m, sd =  1.96)
}


#' summary method for MCMC samples of a spline model
#'
#' @param m 
#' @param par 
#'
#' @return
#' @export
#'
#' 
summary.mcmcspline = function(model, par)
{
  #todo
  return()
}

