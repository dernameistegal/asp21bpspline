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
    paste(deparse(m$call), sep = "\n", collapse = "\n"),
    "\n\n",
    sep = ""
  )
  
  if (length(coef(m)$location)) 
  {
    cat("Location coefficients:\n")
    
    print.default(
      x = as.vector(format(coef(m)$location, digits = digits)),
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
      x = as.vector(format(coef(m)$scale, digits = digits)),
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
    geom_point(aes(y = y), size = 1, alpha = 0.4) +
    geom_line(aes(y = ypred), colour = "black", alpha = 0.9, size = 1)+
    geom_line(aes(y = ypred + sd * scalepred), colour = "black", alpha = 0.9, size = 1)+
    geom_line(aes(y = ypred - sd * scalepred), colour = "black", alpha = 0.9, size = 1)+
    ylab("response")+
    xlab("predictor")+
    theme(axis.text = element_text(size = 22),
          axis.title = element_text(size=22))
}

#' predict method for spline objects
#'
#' @param m 
#' @param X vector of new x values 
#' @param Z vector of new z values
#' @param isDesignmatrix Indication whether X and Z are already spline matrices
#'
#' @return
#' @export
#'
#' 
predict.spline = function(m, X, Z, isDesignmatrix = F)
{
  beta = m$coefficients$location
  gamma = m$coefficients$scale
  
  
  
  if (!isDesignmatrix)
  {
    X = basis_generation(X, m$loc$knots, m$loc$order)$X
    Z = basis_generation(Z, m$scale$knots, m$scale$order)$X
  }
  
  # error catching
  # stopifnot(dim(X)[2] == length(beta))
  # stopifnot(dim(Z)[2] == length(gamma))
  
  location = X %f*f% as.matrix(beta)
  scale = exp(Z %f*f% as.matrix(gamma))
  return(list(location = location, scale =scale))
}

#' summary method for spline objects
#'
#' @param model
#'
#' @return
#' @export
#'
#' 
summary.spline = function(model)
{ 
  cat(
  "\nCall:\n",
  paste(deparse(m$call), sep = "\n", collapse = "\n"),
  "\n\n",
  sep = "")
  prediction = predict(model, model$loc$X, model$scale$Z, T)
  data = cbind(prediction$location, prediction$scale)
  colnames(data) = c("location", "scale")
  cat("\n predicted location and scale values:\n")
  print(head(data))
  cat("... ommited rows for better readability")
  return(invisible(data))
}

#' print method for mcmcspline objects
#'
#' @param m 
#' @param digits 
#' @param ... 
#'
#' @return
#' @export
#'
#' 
print.mcmcspline <- function(m, digits = max(3, getOption("digits") - 3), ...) {
  cat(
    "\nCall:\n",
    paste(deparse(m$call), sep = "\n", collapse = "\n"),
    "\n\n",
    sep = ""
  )
  
  if (length(m$beta[1,])) 
  {
    cat("Posterior mean location coefficients:\n")
    
    print.default(
      x = format(colMeans(m$beta), digits = digits),
      print.gap = 2,
      quote = FALSE
    )
  }
  
  else 
  {
    cat("No location coefficients\n")
  }
  
  cat("\n")
  
  if (length(m$gamma[1,]))
  {
    cat("Posterior mean scale coefficients:\n")
    
    print.default(
      x = format(colMeans(m$gamma), digits = digits),
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

#' predict method for spline objects
#'
#' @param sample sample from the mcmc.spline function
#' @param m model from the spline function 
#'
#' @return
#' @export
#'
#' 
predict.mcmcspline = function(sample, m)
{
  m$coefficients$location = colMeans(sample$beta)
  m$coefficients$scale = colMeans(sample$gamma)
  
  return(predict.spline(m, m$loc$X, m$scale$Z, isDesignmatrix = T))
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
plot.mcmcspline = function(sample, m)
{
  temp = predict.mcmcspline(sample, m)
  m[["fitted.values"]]$location = temp[[1]]
  m[["fitted.values"]]$scale = temp[[2]]
  plot.spline(m, sd =  1.96)
}

#' summary method for MCMC samples of a spline model
#'
#' @param m 
#'
#' @return
#' @export
#'
#' 
summary.mcmcspline = function(model)
{
  cat(
    "\nCall:\n",
    paste(deparse(m$call), sep = "\n", collapse = "\n"),
    "\n\n",
    sep = ""
  )
  cat("Posterior mean location coefficients:\n pointwise quantiles obtained from posterior \n\n")
  spline_values = getEstimateValues2(model)
  quantile = getQuantiles(spline_values, quantile = c(0.05,0.1,0.9, 0.95))
  quantile = t(rbind(quantile[,,1], quantile[,,2]))
  colnames(quantile) = c("loc0.05","loc0.1","loc0.9", "loc0.95","scale0.05","scale0.1","scale0.9", "scale0.95")
  print(head(quantile))
  cat("...ommited rows for better readability")
  return(invisible(quantile))
}







