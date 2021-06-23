require(Rcpp)
library(RcppEigen)
.libPaths(c(.libPaths(), "C:/Users/Jan Schneider/AppData/Local/Temp/RtmpojvPKp/downloaded_packages"))

sourceCpp("src/rcpptest.cpp")


`%f*f%` <- function(a, b)
{
  eigenMapMatMult(a, b)
}


'`%f*f%` <- function(a, b)
{
  a %*% b
}
'

