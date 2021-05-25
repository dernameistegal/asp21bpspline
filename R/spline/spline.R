spline_user_function = function(m, kn, order, p_order, lambda)
{
  m = initialisation(m, kn, p_order, order, lambda)
  m = estimation(m, lambda)
  #m = output()
  return(m)

}



require(lslm)
set.seed(1000)
x = seq(0,10, length.out = 500)
y =  x + rnorm(500, 0, 1) * (0.5 + x*0.3)
m = lslm(y ~ x, light = F)
m = spline_user_function(m, 10, 2, 2, 20)
#so ist das Modell mit splines
print.spl(m, sd = 1.96)

