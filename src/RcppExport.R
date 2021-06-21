require(Rcpp)
library(RcppEigen)
.libPaths(c(.libPaths(), "C:/Users/Jan Schneider/AppData/Local/Temp/RtmpojvPKp/downloaded_packages"))

sourceCpp("src/rcpptest.cpp")



`%f*f%` <- function(a, b)
{
  eigenMatMult(a, b, 1)
}

`%f*f%` <- function(a, b)
{
  return(a %*% b)
}
