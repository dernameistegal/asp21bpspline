// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>

// [[Rcpp::export]]

SEXP eigenMatMult(Eigen::MatrixXd A, Eigen::MatrixXd B, int n)
{
  Eigen::MatrixXd C;
  
  for (int i = 0; i < n; i++){
    C = A * B;
  }
  
  return Rcpp::wrap(C);
}
  

