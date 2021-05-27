
#Hier müssen noch x und y richtig definiert werden für den scatterplot
print.spl <- function(m, sd = 1.96)
{
  require(ggplot2)
  ggplot(mapping = aes(x = x)) +
    geom_point(aes(y = y)) +
    geom_line(aes(y = m$spline[["fitted.values"]]$location), colour = "green", size = 2)+
    geom_line(aes(y = m$spline[["fitted.values"]]$location + sd * m$spline[["fitted.values"]]$scale ), colour = "blue", size = 1)+
    geom_line(aes(y = m$spline[["fitted.values"]]$location - sd * m$spline[["fitted.values"]]$scale ), colour = "blue", size = 1)+
    ylab("dependent variable")+
    xlab("explaining variable")
}

