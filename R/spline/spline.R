spline_user_function = function(m, kn, order, p_order, lambda)
{
  m = initialisation(m, kn, p_order, order, lambda)
  m = estimation(m = m, maxit = 20)
  #m = output()
  return(m)

}





