require(asp21bpspline)


### Only for usage in Simulation 1 and 3 ### 

# done
predict_simulation = function(beta, gamma, knots, order, x)
{
  m = list()
  class(m) = "spline"
  
  # sample from the spline model
  m$coefficients$location = beta
  m$coefficients$scale = gamma
  m$loc$knots = knots[1]
  m$loc$order = order[1]
  m$scale$knots= knots[2]
  m$scale$order = order[1]
  
  pred = predict(m, x, x)
  return(pred)
}

x <- seq(0,20,length.out = 100)
a = predict_simulation(beta, gamma, c(15,15), c(3,3), x)
plot(x,a$location, type = "l")
lines(x,predict_simulation(res10[[1]]$value[,3], res10[[1]]$value[,4], c(15,15), c(3,3), x)$location)
lines(x,predict_simulation(res10[[2]]$value[,3], res10[[2]]$value[,4], c(15,15), c(3,3), x)$location)
lines(x,predict_simulation(res10[[3]]$value[,3], res10[[3]]$value[,4], c(15,15), c(3,3), x)$location)
lines(x,predict_simulation(res10[[4]]$value[,3], res10[[4]]$value[,4], c(15,15), c(3,3), x)$location)
lines(x,predict_simulation(res10[[5]]$value[,3], res10[[5]]$value[,4], c(15,15), c(3,3), x)$location)
lines(x,predict_simulation(res10[[6]]$value[,3], res10[[6]]$value[,4], c(15,15), c(3,3), x)$location)
lines(x,predict_simulation(res10[[7]]$value[,3], res10[[7]]$value[,4], c(15,15), c(3,3), x)$location)
lines(x,predict_simulation(res10[[8]]$value[,3], res10[[8]]$value[,4], c(15,15), c(3,3), x)$location)
lines(x,predict_simulation(res10[[9]]$value[,3], res10[[9]]$value[,4], c(15,15), c(3,3), x)$location)
lines(x,predict_simulation(res10[[10]]$value[,3], res10[[10]]$value[,4], c(15,15), c(3,3), x)$location)

cbind(res10[[431]]$value[,3]- gamma)
plot(x,a$location, type = "l")
plot(NULL, xlim = c(0,20), ylim = c(-10,10))
k <- c(1:500)
for (  i in 1:500){
  b = predict_simulation(res10[[i]]$value[,3], res10[[i]]$value[,4], c(15,15), c(3,3), x)$location
  d = predict_simulation(beta, gamma,c(15,15), c(3,3), x)$location
  # lines(x,b-d)
  # print(i)
  k[i] = (sum((b-d)^2)/length(b))
}
sum(k)/length(k)


