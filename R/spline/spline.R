spline_user_function = function(m, kn, order, p_order, lambda)
{
  m = initialisation(m, kn, p_order, order)
  m = estimation(m, lambda)
  #m = output()
  return(m)

}



require(lslm)
x = seq(0,10, length.out = 100)
y = x + rnorm(100, 0, 0.1)
m = lslm(y ~ x, light = F)
spline_user_function(m, 10, 2, 2, 0)
m = initialisation(m,10,2,2)
