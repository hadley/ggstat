#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::interfaces(r, cpp)]]


// From http://stackoverflow.com/a/4609795/16632
template <typename T> int sign(T val) {
  return (T(0) < val) - (val < T(0));
}

//' Modulus transformation (and its inverse).
//'
//' A generalisation of the box-cox transformation that works for
//' values with both positive and negative values.
//'
//' This is useful for compressing the tails of long-tailed distributions,
//' often encountered with very large datasets.
//'
//' @param x values to transform
//' @param lambda degree of transformation
//' @export
//' @references J. John and N. Draper. "An alternative family of
//'  transformations." Applied Statistics, pages 190-197, 1980.
//'  \url{http://www.jstor.org/stable/2986305}
//' @examples
//' x <- seq(-10, 10, length = 100)
//' plot(x, mt(x, 0), type = "l")
//' plot(x, mt(x, 0.25), type = "l")
//' plot(x, mt(x, 0.5), type = "l")
//' plot(x, mt(x, 1), type = "l")
//' plot(x, mt(x, 2), type = "l")
//' plot(x, mt(x, -1), type = "l")
//' plot(x, mt(x, -2), type = "l")
// [[Rcpp::export]]
NumericVector mt(NumericVector x, double lambda = 0) {
  int n = x.size();
  NumericVector out(n);

  if (lambda == 0) {
    for (int i = 0; i < n; ++i) {
      double val = x[i];
      out[i] = sign(val) * log(fabs(val) + 1);
    }
  } else {
    for (int i = 0; i < n; ++i) {
      double val = x[i];
      out[i] = sign(val) * (pow(fabs(val) + 1, lambda) - 1) / lambda;
    }
  }

  return out;
}

//' @export
//' @rdname mt
// [[Rcpp::export]]
NumericVector inv_mt(NumericVector x, double lambda = 0) {
  int n = x.size();
  NumericVector out(n);

  if (lambda == 0) {
    for (int i = 0; i < n; ++i) {
      double val = x[i];
      out[i] = sign(val) * (exp(fabs(val)) - 1);
    }
  } else {
    for (int i = 0; i < n; ++i) {
      double val = x[i];
      out[i] = sign(val) * (pow(fabs(val) * lambda + 1,  1 / lambda) - 1);
    }
  }

  return out;
}
