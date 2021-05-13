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
setitup = function(len)
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



basis = function(knots, knotno, order, range, value)
{

  len = (max(range) - min(range)) / (knots - 1)
  if (!exists("knotp"))
  {
    knotp = setitup(len)
  }

  # defining base case
  if (order < 1)
  {
    if (knotp[knotno] <= value && value <= knotp[knotno + 1])
    {
      return(1)
    }
    else
    {
      return(0)
    }
  }

  # defining recursive case
  iterand1 = basis(knots, knotno, order - 1, range, value)
  iterand2 = basis(knots, knotno + 1, order - 1, range, value)
  faktor1 = (value - knotp[knotno]) / (len * order)
  faktor2 = (knotp[knotno + order + 1] - value) / (len * (order + 1))
  return(faktor1 * iterand1 + faktor2 * iterand2)
}


seq = seq(1, 10, length.out = 1000)
y = rep(NA, times = 1000)
for (i in 1:1000)
{
  y[i] = basis(knots = 10, knotno = 3, order = 2, range =c(1,10), value = seq[i])
}

plot(seq, y)




