"   VARIABLES
      Input
knots   - positive integer      Number of knots
i       - positive integer      index of knots
d       - positive integer      degree of spline
range   - tulpel of two floats  range of spline
x       - input x for actual Bspline function

      Other
pos   - vector of length knots + 2 * d
"
# setitup is called in basis function to provide an accurate extended pos vector
setitup = function(knots, d, range, len)
{
  pos = rep(NA, times = knots + 2 * d + 1)
  low = 1 + d
  up = knots + d
  pos[(low):(up)] = seq(min(range), max(range), length.out = knots)
  #extend by bd cases
  for (i in 1:d)
  {
    pos[low-i] = pos[low - i + 1] - len
    pos[up + i] = pos[up + i - 1] + len
  }
  pos[up + d + 1] = pos[up + d] + len

  return(pos)
}


# check is called in basis function and verifies that the input is valid
check = function()
{
  return(T)
}

# needs to be redefined.function from wikipedia is inconsistently parameterised
# recursive definition of bspline
recursivespline = function(knots, i, d, range, x, pos)
{
  # defining base case
  if (d == 1)
  {
    out = 0
    if (pos[i] <= x & x < pos[i + 1])
    {
      out = 1
    }
    return(out)
  }

  # defining recursive case
  iterand1 = recursivespline(knots, i,     d - 1, range, x, pos = pos)
  iterand2 = recursivespline(knots, i + 1, d - 1, range, x, pos = pos)


  product1 = 0
  product2 = 0

  if (iterand1 != 0)
  {
    faktor1 = (x - pos[i]) / (pos[i + d - 1] - pos[i])
    product1 = iterand1 * faktor1
  }

  if (iterand2 != 0)
  {
    faktor2 = (pos[i + d] - x) / (pos[i + d] - pos[i + 1])
    product2 = iterand2 * faktor2
  }
  cat("Called:", "i:", i,"d:", d,"range:", range, "x:", x, "out:", product1 + product2, "\n")
  return(product1 + product2)
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
    out = recursivespline(knots, i, d, range, x, pos)

    return(out)
  }
  else
  {
    stop("Called inexistent mode")
  }
}


# Testing code ------------------------------


setitup(4, 1, c(0,3), 1)
"
Setitup works perfectly


"


# basis(knots = 3, i = 3, d = 1, range = c(0,3), x = 1.5)
basis(knots = 3, i = 2, d = 1, range = c(0,3), x = 1.5)


y = rep(NA, times = 100)
s = seq(1,10, length.out = 100)
for (i in 1:100)
{
  y[i] = basis(knots = 10, i = 5, d = 2, range =
                 c(1,10), x = s[i])
}
plot(seq, y)




