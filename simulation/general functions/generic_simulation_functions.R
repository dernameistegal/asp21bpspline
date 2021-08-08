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

# plots true and predicted values (see simulation2_analysis.R file for specific form of truth_and_pred object)
plot_simulation = function(truth_and_pred, sd = 1, ylim = c(-50,50))
{
  ggplot2::ggplot(truth_and_pred, aes(x = x, linetype = true_or_pred, alpha = true_or_pred))+
    geom_line(aes(y = loc), size = 1)+
    geom_line(aes(y = loc + sd * scale), size = 1)+
    geom_line(aes(y = loc - sd * scale), size = 1)+
    labs(x = "predictor", y = "location and scale value")+
    theme(legend.title = element_blank())+
    scale_alpha_manual(values = c(0.4, 1), guide = "none")+
    scale_linetype_manual(values = c(1, 3),
                          labels = c("true", "predicted"))+
    scale_color_brewer(palette="Dark2") + xlim(min(x), max(x)) + ylim(ylim[1],ylim[2])+
    theme(axis.text = element_text(size = 22),
          axis.title = element_text(size=22),
          legend.title=element_text(size=22),
          legend.text=element_text(size=22))
  
}
