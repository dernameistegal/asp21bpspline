


#' Location-Scale basis spline regression
#'
#' @description
#'
#' The Location-scale bspline regression model enables flexible non-parametric
#' modeling of the mean and of the standard deviation. It is only possible to
#' model one predictor variable. To control the smoothness of the model a
#' penalization term is applied
#'
#' @param m  a list of the form list(x = x, z = x, y = y), with x the predictor
#'           vector for the location, z, the predictor vector for the scale,
#'           and y the response vector.
#'
#' @param kn kn vector of integers 1. specifying number of knots for location
#'           2. specifying number of knots for scale
#'
#' @param order vector of integers 1. specifying order of spline for location
#'              2. specifying order of spline for scale
#'
#' @param p_order vector of integers 1. specifying order of spline for location
#'                2. specifying order of spline for scale
#'
#' @param smooth vector of positive floats specifying strength of penalization
#'               1. for location parameters (1/tau)
#'               2. for scale parameters (1/epsilon)
#'
#' @return a spline object with estimations for the location and scale
#'         coefficients.
#' @export

spline = function(m, kn, order, p_order, smooth)
{
  m = initialisation(m, kn, p_order, order, smooth)
  m = estimation(m = m, maxit = 100)
  m$call = match.call()
  return(m)
}