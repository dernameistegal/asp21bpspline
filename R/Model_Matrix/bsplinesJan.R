"   VARIABLES
    Input
knots   - positive integer
knotno  - positive integer
order   - positive integer
range   - tulpel of two floats
value   - input value for actual Bspline function
setup   - boolean

    Other
knotp   - vector of length knots + 2 * order
"
# setitup is called in basis function to provide an accurate extended knotp vector
setitup = function(knots, order, range, len)
{
  knotp = rep(NA, times = knots + 2 * order)
  low = 1 + order
  up = knots + order
  knotp[(low):(up)] = seq(min(range), max(range), length.out = knots)
  #extend by border cases
  for (i in 1:order)
  {
    knotp[low-i] = knotp[low - i + 1] - len
    knotp[up + i] = knotp[up + i - 1] + len
  }

  return(knotp)
}


basis = function(knots, knotno, order, range, value, knotp = NA)
{

  len = (max(range) - min(range)) / (knots - 1)
  if (any(is.na(knotp)) == T)
  {
    knotp = setitup(knots, order, range, len)
    knotno = knotno + order
  }

  # defining base case
  if (order < 1)
  {
    if (knotp[knotno] < value & value < knotp[knotno + 1])
    {
      return(1)
    }
    else
    {
      return(0)
    }
  }

  # defining recursive case
  iterand1 = basis(knots, knotno, order - 1, range, value, knotp = knotp)
  iterand2 = basis(knots, knotno + 1, order - 1, range, value, knotp = knotp)
  faktor1 = (value - knotp[knotno]) / (knotp[knotno + order] - knotp[knotno])
  faktor2 = (knotp[knotno + order + 1] - value) / (knotp[knotno + order + 1] - knotp[knotno + 1])
  return(faktor1 * iterand1 + faktor2 * iterand2)
}



basis(knots = 10, knotno = 5, order = 2, range =
        c(1,10), value = 5)


y = rep(NA, times = 100)
s = seq(1,10, length.out = 100)
for (i in 1:100)
{
  y[i] = basis(knots = 10, knotno = 5, order = 2, range =
                 c(1,10), value = seq[i])
}
plot(seq, y)
