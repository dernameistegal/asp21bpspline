library(simsalapar)
require(ggplot2)
require(asp21bpspline)

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