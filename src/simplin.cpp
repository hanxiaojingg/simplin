#include <RcppArmadillo.h>
#include <Rcpp.h>
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
List simp_lin_cpp(NumericVector x, NumericVector y, int n){
  double x_bar = mean(x);
  double y_bar = mean(y);
  NumericVector x_c = x - x_bar;
  NumericVector m = pow(x_c,2);
  double dn = sum(m);
  double beta1 = sum(x_c*y)/dn;
  double beta0 = y_bar - beta1*x_bar;
  NumericVector pred = beta0 + beta1*x;
  NumericVector res = y - pred;
  NumericVector ss = pow(res,2);
  double mse = sum(ss)/(n-2);
  double se0 = sqrt(mse*(pow(x_bar,2)/dn  + 0.01));
  double se1 = sqrt(mse/dn);
  NumericVector b0 = NumericVector::create(beta0,se0,beta0-1.96*se0,beta0+1.96*se0);
  NumericVector b1 = NumericVector::create(beta1,se1,beta1-1.96*se1,beta1+1.96*se1);
  List out = List::create(b0, b1, pred, res);
  return out;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//


