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




# Mon May 24 19:30:25 2021 ------------------------------
# optimizing penalty matrix

penalty = function(knots,diffd = 2)
{
  if (diffd == 1)
  {
    ones = cbind(0,diag(knots - 1))
    nones = cbind(diag(knots - 1) * -1, 0)
    K = ones + nones
    K = t(K) %*% K
    return(ones + nones)
  }

  # recursive definition of penalty matrix
  #shrink starting knots such that end knots are correct
  knots = knots - diffd + 1
  K = penalty(knots, 1)
  for (i in 1:(diffd - 1))
  {
    K = K %*% penalty(knots + i, 1)
  }
  K = t(K) %*% K
  return(K)
}


penalty(9,2)




# wrapper
ml = function(xa,y, kn, d, diffd, lambda)
{
  m = lslm(y ~ xa, light = F)
  x = bspline(m, knots = kn, d = d)
  K = penalty(knots = kn + d - 1, diffd = diffd)

  beta = solve(t(x) %*% x + lambda * K) %*% t(x) %*% y
  pred = x %*% beta


  plot(xa, y)
  lines(xa, pred, col = 2, lwd = 5)

}


x = seq(0,10,length.out = 100)
y = 0.1 * x + rnorm(100,0,0.3)
plot(x,y)
ml(x,y,kn = 20, d = 4, diffd = 1, lambda = 0.1)






