value = seq(1,10, length.out = 100)
y = rep(NA, times = 100)
knotp = setitup(10, 2, c(1,10), 1)
knotno = 5
order = 2
for (i in 1:100)
{
  y[i] = (value[i] - knotp[knotno]) / (knotp[knotno + order] - knotp[knotno])
}

plot(value, y)
