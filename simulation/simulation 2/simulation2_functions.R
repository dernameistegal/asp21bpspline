loc_sim2 = function(x) {
  return(-0.0004*x^4 + 0.005* x^3 - 0.05*x^2 + 2*x + 4*sin(x))
}

scale_sim2 = function(x) {
  return(2.1 + 2*sin(x) + x^2/200)
}

rsimu2data <- function(n){
  x = runif(n,0,20)
  mean = loc_sim2(x)
  scale = scale_sim2(x)
  y =  mean + rnorm(n, sd = scale)
  return(list(x = x , y = y))
}