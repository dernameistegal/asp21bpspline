"   VARIABLES
      Input
m       - lslm object
knots   - positive integer      Number of knots
i       - positive integer      index of knots
d       - positive integer      degree of spline
range   - tulpel of two floats  range of spline
x       - vector of floats      input value for bspline function
lslm    - Modus if an LSLM object is returned or just the model matrix

      Other
pos   - vector of length knots + 2 * d
exknots - extended number of knots
"



source("R/Model_Matrix/bsplinesJan.R")




# returns a suitable modelmatrix for bspline
bspline = function(m, knots, d, lslm = F)
{
  # removing superfluous intercept
  if (all(m$x[,1] == 1))
  {
    m$x = m$x[,-1]
  }


  exknots = knots - 1 + d
  mat = data.frame(matrix(0, nrow = length(m$x), ncol = exknots))
  range = range(m$x)


  # applying basis function to every element
  for (i in 1:exknots)
  {
    mat[,i]= basis(knots, i, d, range, m$x, pos = NA)
  }
  m$x = model.matrix(~ . - 1, data = mat)
  if (lslm == F)
  {
    return(m$x)
  }
  return(m)

}

# Thu May 20 10:21:52 2021 ------------------------------

# Random data
require(lslm)
set.seed(1)
n = 1000
xa = seq(0,1, length.out = n)
y = xa + rnorm(n, 0, 0.3)
m = lslm(y ~ xa, light = F)


# trying to implement it with lslm not working as of now
#
# x = mspline$x
# mspline = lslm(y ~ x[,1] + x[,2]+ x[,3]+ x[,4]+ x[,5] - 1, light = F)
# summary(mspline)
#
#
# plot(xa, y)
# points(predict(mspline)$location, y, col = "green")


# Thu May 20 10:22:44 2021 ------------------------------
# implementing a ML estimator

x = bspline(m, 40, 3)

beta = solve(t(x) %*% x) %*% t(x) %*% y
pred = x %*% beta


plot(xa, y)
points(xa, pred, col = 2)
lines(xa, xa, col = 3)


# testing performance for difficult data


n = 1000
xa = runif(n,0,10)
xa = sort(xa)
s = (exp(0.2*xa)-1)/10+0.3
epsilon = rnorm(n,0, s)

ytrue =sin(xa*4) *xa/4
plot(xa, ytrue)
y = ytrue + epsilon
plot(xa,y)


m = lslm(y ~ xa, light = F)
x = bspline(m, 200, 3)

beta = solve(t(x) %*% x) %*% t(x) %*% y
pred = x %*% beta


plot(xa, y)
lines(xa, pred, col = 2, pch = 5)
points(xa, ytrue, col = 3)




