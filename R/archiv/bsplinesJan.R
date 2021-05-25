"   VARIABLES
      Input
knots   - positive integer      Number of knots
i       - positive integer      index of knots
d       - positive integer      degree of spline
range   - tulpel of two floats  range of spline
x       - vector of floats      input value for bspline function

      Other
pos   - vector of length knots + 2 * d
"
# setitup is called in basis function to provide an accurate extended pos vector
setitup = function(knots, d, range, len)
{
  pos = rep(NA, times = knots + 2 * d)
  low = 1 + d
  up = knots + d
  pos[(low):(up)] = seq(min(range), max(range), length.out = knots)
  #extend by bd cases
  for (i in 1:(d))
  {
    pos[low-i] = pos[low - i + 1] - len
    pos[up + i] = pos[up + i - 1] + len
  }

  return(pos)
}


# check is called in basis function and verifies that the input is valid
check = function()
{
  return(T)
}

# recursive definition of bspline based on kneib p 429
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


  #cat("Called:", "i:", i,"d:", d,"range:", range, "x:", x, "out:", product1 + product2, "\n")
  return(product1 + product2)
}

#iterative definition of bspline based on wikipedia
iterativespline = function(i, d, range, x, pos)
{
  stop("iterativespline is not defined")
}

# main function
basis = function(knots, i, d, range, x, pos = NA, mode = "recursive")
{
  len = (max(range) - min(range)) / (knots - 1)
  if (any(is.na(pos)) == T)
  {
    pos = setitup(knots, d, range, len)
    i = i + d
  }

  stopifnot(check())

  if (mode == "recursive")
  {
    out = recursivespline(i, d, range, x, pos)

    return(out)
  }
  else if (mode == "iterative")
  {
    out = iterativespline(i, d, range, x, pos)
    return(out)
  }
  else
  {
    stop("Called inexistent mode")
  }
}


# Testing code ------------------------------

#
# setitup(4, 1, c(0,3), 1)
# "
# Setitup works perfectly
#
#
# "
#
#
# # basis(knots = 3, i = 3, d = 1, range = c(0,3), x = 1.5)
# basis(knots = 4, i = 1, d = 0, range = c(0,3), x = 1.5)
#
#
# y = rep(NA, times = 100)
# s = seq(0,3, length.out = 100)
#
#
# basis(knots = 4, i = 1, d = 2, range = c(0, 3), x = s)
#
# plot(s, basis(knots = 4, i = 5, d = 2, range = c(0, 3), x = s))
#
#
# basis(knots = 4, i = 5, d = 3, range =c(1, 4), x = 2.1)

