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
#penalization funktioniert zwar, ist aber vollkommen nutzlos, da eh weggeupdated und da keine Rolle mehr spielt, da müssen wir uns ncoh was überlegen.
#man sieht das z.B. daran, dass es mehr Iterations gibt, wenn ich das Lambda erhöhe lambda = 0 führt zu 12 große lambda aber zu 13
m = spline_user_function(m = m, kn =  11, order =  3, p_order =  1,lambda =  1)
#so ist das Modell mit splines
print.spl(m, sd = 1.96)

