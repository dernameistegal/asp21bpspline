"   VARIABLES
      Input
knots   - positive integer      Number of knots
i       - positive integer      index of knots
d       - positive integer      degree of spline
range   - tulpel of two floats  range of spline
x       - vector of floats      input value for bspline function

      Other
pos   - vector of length knots + 2 * d
exknots - extended number of knots
"



source("R/Model_Matrix/bsplinesJan.R")




# returns a suitable modelmatrix for bspline
bspline = function(m, knots, d)
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
  return(m)

}



install.packages("lslm")
require(lslm)
xa = seq(0,1, length.out = 100)
y = xa + rnorm(100)
m = lslm(y ~ xa, light = F)

x = mspline$x
mspline = lslm(y ~ x[,1] + x[,2]+ x[,3]+ x[,4]+ x[,5] - 1, light = F)
summary(mspline)


plot(xa, y)
points(predict(mspline)$location, y, col = "green")




