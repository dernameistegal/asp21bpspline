library(simsalapar)
res20 = maybeRead("simulation/simulation 2/take_9_andere")
val = getArray(res20)


array2df(getArray(res20))
str(getArray(res20))
dim(val)
#spline loacation
rowMeans(val[,1,1,])
#spline scale
rowMeans(val[,2,1,])
#mcmc location
rowMeans(val[,3,1,])
#mcmc scale
rowMeans(val[,4,1,])


##### ab hier hat Valentin was gemacht#####
x <- seq(0,20, length.out = 1000)
predict_simulation <- function(location_coef, scale_coef, simulation2){
  m = list()
  class(m) = "spline"
  m$coefficients$location = location_coef
  m$coefficients$scale = scale_coef
  m$loc$knots = simulation2[["knots"]]$value[1]
  m$loc$order = simulation2[["order"]]$value[1]
  m$scale$knots= simulation2[["knots"]]$value[2]
  m$scale$order = simulation2[["order"]]$value[2]
  spline_Werte <- predict(m, x, x)
  return(spline_Werte)
}

splines_result <- predict_simulation(rowMeans(val[,1,1,]),rowMeans(val[,2,1,]), simulation2 )
mcmc_result <- predict_simulation(rowMeans(val[,3,1,]),rowMeans(val[,4,1,]), simulation2 )


# wie weit liegen die Werte von den wahren Werten weg?
plot(x,type = "l", 5*sin(x))
#lines(x,type = "l", -0.04*x^2 + 0.5* x + 2*(0.05 *x + 0.3), col = "blue")
#lines(x,type = "l", -0.04*x^2 + 0.5* x - 2*(0.05 *x + 0.3), col = "blue")
lines(x, splines_result[[1]], col = "red")
#lines(x, spline_Werte[[1]] + 2 * spline_Werte[[2]], col = "red")
#lines(x, spline_Werte[[1]] - 2 * spline_Werte[[2]], col = "red")
lines(x, mcmc_result[[1]], col = "Blue")

