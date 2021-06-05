spline_user_function = function(m, kn, order, p_order, lambda)
{
  m = initialisation(m, kn, p_order, order, lambda)
  m = estimation(m = m, maxit = 20)
  #m = output()
  return(m)

}



require(lslm)
set.seed(1000)
x = seq(0,10, length.out = 500)
y =  x + rnorm(500, 0, 1) * (0.5 + x*0.3)
m = lslm(y ~ x, light = F)
#penalization problem etwas verbessert, nun haben große lambda eine große Wirkung, aber wir müssen noch überlgen, ob update gamma auch betroffen ist
#bis jetzt ist nur update beta etwas verändert
m = spline_user_function(m = m, kn =  11, order =  3, p_order =  3,lambda =  1)
#so ist das Modell mit splines
print.spl(m, sd = 1.96)


