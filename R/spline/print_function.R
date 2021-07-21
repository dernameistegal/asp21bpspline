
#Hier müssen noch x und y richtig definiert werden für den scatterplot
print.spl <- function(m, sd = 1.96)
{
  data = data.frame(x = m$formerx, y = m$y,
                    ypred = m[["fitted.values"]]$location,
                    scalepred = m[["fitted.values"]]$scale)
  require(ggplot2)
  ggplot(data, mapping = aes(x = m$formerx)) +
    geom_point(aes(y = y)) +
    geom_line(aes(y = ypred), colour = "green", size = 2)+
    geom_line(aes(y = ypred + sd * scalepred), colour = "blue", size = 1)+
    geom_line(aes(y = ypred - sd * scalepred), colour = "blue", size = 1)+
    ylab("dependent variable")+
    xlab("explaining variable")
}

