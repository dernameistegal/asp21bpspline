"
m object from lmls package

kn vector of integers 1. specifying number of knots for location
                        2. specifying number of knots for scale

order vector of integers 1. specifying order of spline for location
                        2. specifying order of spline for scale

p_order vector of integers 1. specifying order of spline for location
                          2. specifying order of spline for scale

smooth vector of positive floats specifying strength of penalization
                1. for location parameters (1/tau)
                2. for scale parameters (1/epsilon)
"



spline_user_function = function(m, kn, order, p_order, smooth)
{
  m = initialisation(m, kn, p_order, order, smooth)
  m = estimation(m = m, maxit = 20)
  #m = output()
  return(m)

}





