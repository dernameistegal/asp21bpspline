x = runif(n, 0, 10)

y = x + rnorm(n)

m1 = list(x = x, z = x, y = y)

spline(m1, kn = c(15, 15), order = c(3, 3), 
      p_order = c(0,0), smooth = c(0,0))
