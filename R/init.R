# Main function to initialize Design matrices and penalty matrices
initialisation = function(m, kn, p_order, order, smooth)
{
  #Building Matrices for location (Design and Penalty)
  templist = basis_generation(m$x, kn[1], order[1])
  m$spline$loc$X = templist$X
  m$spline$loc$ext_kn = templist$ext_kn
  m$spline$loc$order = order[1]
  m$spline$loc$knots = kn[1]
  m$spline$formerx = templist$formerx
  D = penalty(m$spline$loc$ext_kn, p_order[1])
  m$spline$loc$K = t(D) %*% D

  #Building Matrices for scale (Design and Penalty)
  templist = basis_generation(m$z, kn[2], order[2])
  m$spline$scale$Z = templist$X
  m$spline$scale$ext_kn = templist$ext_kn
  m$spline$scale$order = order[2]
  m$spline$scale$knots = kn[2]
  m$spline$formerz = m$formerx
  D = penalty(m$spline$scale$ext_kn, p_order[2])
  m$spline$scale$K = t(D) %*% D

  m$spline$p_order = p_order
  m$spline$y = m$y
  m$spline$smooth = smooth

  m = m$spline
  class(m) = "spline"
  return(m)
}

# Generates model matrices
basis_generation = function(X, kn, order, lmls = T)
{
  # Removing superfluous intercept if present
  if (!is.null(dim(X)))
  {
    if (all(X[,1] == 1))
    {
      X = X[,-1]
    }
  }
  formerx = X

  ext_kn = kn - 1 + order
  mat = data.frame(matrix(0, nrow = length(X), ncol = ext_kn))
  range = range(X)

  # Applying basis functions to every data point
  for (i in 1:ext_kn)
  {
    mat[,i]= basis(kn, i, order, range, X, pos = NA)
  }

  X = model.matrix(~ . - 1, data = mat)

  return(list(X = X, ext_kn = ext_kn, formerx = formerx))
}

# Generates penalty matrices
penalty = function(kn, p_order = 2)
{
  if (p_order == 1)
  {
    ones = cbind(0, diag(kn - 1))
    nones = cbind(diag(kn - 1) * -1, 0)
    D = ones + nones
    return(ones + nones)
  }
  
  if (p_order == 0)
  {
    return(matrix(0, ncol = kn, nrow = kn))
  }
  

  # Recursive definition of prerequisite for penalty matrix (not yet the true penalty matrix)
  D = penalty(kn, 1)[1:(kn - p_order), 1:(kn - p_order + 1)]
  D = D %*% penalty(kn, p_order - 1)
  return(D)
}

# A helper function to provide external knots needed for recursive definition of basis functions
setitup = function(kn, d, range, len)
{
  pos = rep(NA, times = kn + 2 * d)
  low = 1 + d
  up = kn + d
  pos[(low):(up)] = seq(min(range), max(range), length.out = kn)
  #Extend by d cases
  for (i in 1:(d))
  {
    pos[low-i] = pos[low - i + 1] - len
    pos[up + i] = pos[up + i - 1] + len
  }

  return(pos)
}

# Recursive definition of basis splines based on Kneib p. 429. order is renamed d here to provide more readable code.
recursivespline = function(i, d, range, x, pos)
{
  # Defining base case
  if (d == 0)
  {
    index = pos[i] <= x & x < pos[i + 1]
    out = x
    out[index] = 1
    out[!index] = 0

    return(out)
  }

  # Defining recursive case
  iterand1 = recursivespline(i - 1, d - 1, range, x, pos = pos)
  iterand2 = recursivespline(i    , d - 1, range, x, pos = pos)

  product1 = rep(0, times = length(x))
  product2 = product1

  faktor1 = (x - pos[i - d]) / (pos[i] - pos[i - d])
  product1 = iterand1 * faktor1

  faktor2 = (pos[i + 1] - x) / (pos[i + 1] - pos[i + 1 - d])
  product2 = iterand2 * faktor2

  return(product1 + product2)
}



# Main function to calculate values of basis functions
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