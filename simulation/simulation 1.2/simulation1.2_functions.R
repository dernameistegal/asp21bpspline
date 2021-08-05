require(asp21bpspline)


### Only for usage in Simulation 1, 1.2 and 3 ### 

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


# find optimal smoothing parameter for simulation 1.2

beta = read.csv("simulation/simulation 1/beta_sim1")
gamma = read.csv("simulation/simulation 1/gamma_sim1")

set.seed(2)
x = runif(n, 0, 20)
x = sort(x)
truth = predict_simulation(beta, gamma, c(15, 15), order, x)
n  = 1000
knots = c(20, 20)
order = c(3, 3)
p_order = c(3,3)
y = truth$location + rnorm(n, 0, truth$scale)
train = list(x = x, z = x, y = y)
plot(train$x,train$y)
m = list()
m[["fitted.values"]]$location = truth[[1]]
m[["fitted.values"]]$scale = truth[[2]]
require(ggplot2)
sd = 1.96
data = data.frame(x = x,
                  ypred = m[["fitted.values"]]$location,
                  scalepred = m[["fitted.values"]]$scale)
ggplot2::ggplot(data, mapping = aes(x = x)) +
  geom_line(aes(y = ypred), colour = "green", size = 2)+
  geom_line(aes(y = ypred + sd * scalepred), colour = "blue", size = 1)+
  geom_line(aes(y = ypred - sd * scalepred), colour = "blue", size = 1)+
  ylab("dependent variable")+
  xlab("explaining variable")



# do gridsearch before doing the simulation study separately for both models
gridsearch = function(alpha, knots)
{
  len = length(alpha)
  pb = txtProgressBar(min = 0, max = len, style = 3)
  
  models = array(list(matrix), dim = c(len, len))
  # location
  for (i in 1:len)
  {
    # scale
    for (j in 1:len)
    {
      temp = spline(train, knots, order = order, p_order = p_order, 
                            smooth = c(alpha[i], alpha[j]))
      models[[i, j]] = list(temp$coefficients$location, temp$coefficients$scale)
    }
    setTxtProgressBar(pb, i)
  }
  return(models)
}


specialMSE = function(truth, result, knots, order, x)
{
  len = length(result[[1]])
  
  meanbeta = result[[1]]
  meangamma = result[[2]]
  
  pred = predict_simulation(meanbeta, meangamma, knots, order, x)
  
  loc = (pred[[1]] - truth[[1]])^2
  scale = (pred[[2]] - truth[[2]])^2
  loc = colMeans(loc)
  scale = colMeans(scale)
  
  return(c(loc, scale))
}


findopt = function(models, alpha, x)
{
  len = length(alpha)
  MSE = array(NA, dim = c(len, len, 2))
  # location
  for (i in 1:len)
  {
    # scale
    for (j in 1:len)
    {
      MSE[i, j,] = specialMSE(truth, models[[i,j]], knots, order, x)
    }
  }
  msesum = MSE[,,1] + MSE[,,2]
  opt = min(msesum)
  opt = which(msesum == opt, arr.ind = T)
  
  return(c(alpha[opt[1]], alpha[opt[2]]))
}

#adapt gridsearch for different alpha vectors
gridsearch2 = function(alpha1, alpha2, knots)
{
  len1 = length(alpha1)
  len2 = length(alpha2)
  pb = txtProgressBar(min = 0, max = len1, style = 3)
  
  models = array(list(matrix), dim = c(len1, len2))
  # location
  for (i in 1:len1)
  {
    # scale
    for (j in 1:len2)
    {
      temp = spline(train, knots, order = order, p_order = p_order, 
                    smooth = c(alpha1[i], alpha2[j]))
      models[[i, j]] = list(temp$coefficients$location, temp$coefficients$scale)
    }
    setTxtProgressBar(pb, i)
  }
  return(models)
}

findopt2 = function(models, alpha1, alpha2, x)
{
  len1 = length(alpha1)
  len2 = length(alpha2)
  MSE = array(NA, dim = c(len1, len2, 2))
  # location
  for (i in 1:len1)
  {
    # scale
    for (j in 1:len2)
    {
      MSE[i, j,] = specialMSE(truth, models[[i,j]], knots, order, x)
    }
  }
  msesum = MSE[,,1] + MSE[,,2]
  opt = min(msesum)
  opt = which(msesum == opt, arr.ind = T)
  
  return(c(alpha1[opt[1]], alpha2[opt[2]]))
}

#### 20 knots #### 

alpha = exp(seq(0, 15, length.out = 10))
models = gridsearch(alpha, knots)

findopt(models, alpha, x)
# opt 1 1

alpha = seq(0, 2, length.out = 10)
models2 = gridsearch(alpha, knots)
findopt(models2, alpha, x)
# opt 0.222 0

alpha1 = seq(0.1, 0.2, length.out = 10)
alpha2 = seq(0.0, 0.05, length.out = 5)
models3 = gridsearch2(alpha1, alpha2, knots)
findopt2(models3, alpha1, alpha2, x)
# seed 1 opt 0.2894737 0
# seed 2 opt 0.1888, 0.0
# seed 3 opt 0.0733, 0.011
# seed 4 opt 0.1222 0.00
mean(c(0.2894737, 0.1888, 0.0733, 0.1222))
mean(c(0,0,0,0.011))

m = spline(train, knots, order = order, p_order = p_order, 
           smooth = c(0.2894737, 0))
plot(m)


#### 40 knots #### 

knots = c(40, 40)

alpha = exp(seq(0, 15, length.out = 30))
models4 = gridsearch(alpha, knots)
findopt(models4, alpha, x)
# opt 22.27462, 2.813651


alpha1 = seq(15, 30, length.out = 5)
alpha2 = seq(1, 5, length.out = 5)
models5 = gridsearch2(alpha1, alpha2, knots)
findopt2(models5, alpha1, alpha2, x)
# opt 18.75, 4.0

alpha1 = seq(16, 22, length.out = 5)
alpha2 = seq(3,5, length.out = 5)
models5 = gridsearch2(alpha1, alpha2, knots)
findopt2(models5, alpha1, alpha2, x)
# opt 19, 3.5

alpha1 = seq(15, 20, length.out = 4)
alpha2 = seq(0.5, 1, length.out = 4)
models5 = gridsearch2(alpha1, alpha2, knots)
findopt2(models5, alpha1, alpha2, x)
# seed 1 opt 18.5, 3.55555
# seed 2 opt 18.333, 0.66667
# seed 3 opt 8 0.825
# seed 4 opt 12.6666, 0.5555

mean(c(18.5, 18.333, 8, 12.6666))
mean(c(3.555, 0.66667, 0.825, 0.5555))

