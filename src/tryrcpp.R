require(Rcpp)
library(RcppEigen)
.libPaths(c(.libPaths(), "C:/Users/Jan Schneider/AppData/Local/Temp/RtmpojvPKp/downloaded_packages"))
sourceCpp("rcpptest.cpp")


n = 100
a = matrix(rnorm(n*n), ncol = n)
b = matrix(rnorm(n*n), ncol = n)


mult100 = function(a,b,n)
{
  for (i in 1:n)
  {
    c = a %*% b

  }
  return(c)
}


require(microbenchmark)
microbenchmark(
  eigenMatMult(a,b, 100),
  mult100(a,b, 100)
)
