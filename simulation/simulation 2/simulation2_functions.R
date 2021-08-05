loc_sim2_hard = function(x) {
  return(-0.0004*x^4 + 0.005* x^3 - 0.05*x^2 + 2*x + 4*sin(x))
}

loc_sim2_easy = function(x) {
  return(rep(0,length(x)))
}

scale_sim2_hard = function(x) {
  return(2.1 + 2*sin(x) + x^2/200)
}

scale_sim2_easy = function(x) {
  return(rep(4, length(x)))
}

rsimu2.1data <- function(n){
  x = runif(n,0,20)
  mean = loc_sim2_easy(x)
  scale = scale_sim2_hard(x)
  y =  mean + rnorm(n, sd = scale)
  return(list(x = x , y = y))
}

rsimu2.2data <- function(n){
  x = runif(n,0,20)
  mean = loc_sim2_hard(x)
  scale = scale_sim2_easy(x)
  y =  mean + rnorm(n, sd = scale)
  return(list(x = x , y = y))
}

rsimu2.3data <- function(n){
  x = runif(n,0,20)
  mean = loc_sim2_hard(x)
  scale = scale_sim2_hard(x)
  y =  mean + rnorm(n, sd = scale)
  return(list(x = x , y = y))
}


