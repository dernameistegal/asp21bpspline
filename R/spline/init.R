initialisation = function(m, kn, p_order, order, lambda)
{
  m = basis_generation(m, kn, order)
  D = penalty(m$spline$ext_kn, p_order)
  m$spline$K = t(D) %*% D
  m$spline$y = m$y
  m$spline$lambda = lambda
  return(m)
}


basis_generation = function(m, kn, order, lmls = T)
{
  # removing superfluous intercept
  if (all(m$x[,1] == 1))
  {
    m$spline$x = m$x[,-1]
  }


  m$spline$ext_kn = kn - 1 + order
  mat = data.frame(matrix(0, nrow = length(m$spline$x), ncol = m$spline$ext_kn))
  m$spline$range = range(m$x)

  # applying basis function to every element
  for (i in 1:m$spline$ext_kn)
  {
    mat[,i]= basis(kn, i, order, m$spline$range, m$spline$x, pos = NA)
  }

  m$spline$x = model.matrix(~ . - 1, data = mat)
  return(m)
}


penalty = function(kn, p_order = 2)
{
  if (p_order == 1)
  {
    ones = cbind(0, diag(kn - 1))
    nones = cbind(diag(kn - 1) * -1, 0)
    D = ones + nones
    return(ones + nones)
  }

  # recursive definition of prerequisite for penalty matrix
  D = penalty(kn, 1)[1:(kn - p_order), 1:(kn - p_order + 1)]
  D = D %*% penalty(kn, p_order - 1)
  return(D)
}

" The functions in this section compute the basis for a b-spline model

"

# setitup is called in basis function to provide an accurate extended pos vector
setitup = function(kn, d, range, len)
{
  pos = rep(NA, times = kn + 2 * d)
  low = 1 + d
  up = kn + d
  pos[(low):(up)] = seq(min(range), max(range), length.out = kn)
  #extend by d cases
  for (i in 1:(d))
  {
    pos[low-i] = pos[low - i + 1] - len
    pos[up + i] = pos[up + i - 1] + len
  }

  return(pos)
}



# recursive definition of bspline based on kneib p 429
# order is renamed d here to provide more readable code
recursivespline = function(i, d, range, x, pos)
{
  # defining base case
  if (d == 0)
  {
    index = pos[i] <= x & x < pos[i + 1]
    out = x
    out[index] = 1
    out[!index] = 0

    return(out)
  }

  # defining recursive case
  iterand1 = recursivespline(i - 1, d - 1, range, x, pos = pos)
  iterand2 = recursivespline(i    , d - 1, range, x, pos = pos)

  product1 = rep(0, times = length(x))
  product2 = product1

  faktor1 = (x - pos[i - d]) / (pos[i] - pos[i - d])
  product1 = iterand1 * faktor1

  faktor2 = (pos[i + 1] - x) / (pos[i + 1] - pos[i + 1 - d])
  product2 = iterand2 * faktor2

  # cat("Called:", "i:", i,"d:", d,"range:", range, "x:", x, "out:", product1 + product2, "\n")
  return(product1 + product2)
}



# main function
basis = function(kn, i, order, range, x, pos = NA)
{
  len = (max(range) - min(range)) / (kn - 1)
  if (any(is.na(pos)) == T)
  {
    pos = setitup(kn, order, range, len)
    i = i + order
  }
  out = recursivespline(i, order, range, x, pos)
  return(out)
}



# library(lmls)
#
# set.seed(100)
# x <- runif(1000,0,10)
# y <- 0.5 * x + cos(x) +rnorm(1000)
#
# m = lmls(y ~ x, light = FALSE)
# m = initialisation(m, 11, 2, 2)




