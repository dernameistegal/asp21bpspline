spline_user_function = function(m, kn, p_order, lambda)
{
  m(X, K) = initialisation(m, kn, p_order)
  m = estimation(m, lambda)
  m = output()
  return(m)


  m = lslm(y ~ xa, light = F)
  x = bspline(m, knots = kn, order = order)
  K = penalty(knots = kn + order - 1, p_order = p_order)

  beta = solve(t(x) %*% x + lambda * K) %*% t(x) %*% y
  pred = x %*% beta


}
