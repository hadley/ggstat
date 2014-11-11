#include <Rcpp.h>
#include "smoothers.h"
using namespace Rcpp;

inline double tricube(double x) {
  if (NumericVector::is_na(x)) return 0;
  x = fabs(x);
  if (x > 1) return 0;

  double y = 1 - x * x * x;
  return y * y * y;
}

bool both_na(double x, double y) {
  return NumericVector::is_na(x) && NumericVector::is_na(y);
}

template <class Smoother>
NumericVector smooth(const NumericVector& x_in,
                     const NumericVector& z_in,
                     const NumericVector& w_in,
                     const NumericVector& x_out,
                     const Smoother& smoother, const double h) {
  if (h <= 0) stop("h <= 0");
  if (x_in.size() != z_in.size()) stop("x and z must be same length");
  if (x_in.size() != w_in.size()) stop("x and w must be same length");

  int n_in = x_in.size(), n_out = x_out.size();
  NumericVector z_out(n_out);

  for(int j = 0; j < n_out; ++j) {
    std::vector<double> x1, z1, w1;

    // Could make this fast if we assume x_out is ordered: use binary
    // search to find first and last locations
    for (int i = 0; i < n_in; ++i) {
      double in = x_in[i], out = x_out[j];
      double dist = both_na(in, out) ? 0 : in - out;
      double w = tricube(dist / h) * w_in[i];
      if (w == 0) continue;

      x1.push_back(dist);
      z1.push_back(z_in[i]);
      w1.push_back(w);

    }
    z_out[j] = smoother.compute(x1, z1, w1);
  }

  return z_out;
}


// [[Rcpp::interfaces(r, cpp)]]
// [[Rcpp::export]]
NumericVector smooth_linear(const NumericVector& x_in,
                            const NumericVector& z_in,
                            const NumericVector& w_in,
                            const NumericVector& x_out,
                            const double h) {
  return smooth(x_in, z_in, w_in, x_out, LinearSmoother(), h);
}

// [[Rcpp::export]]
NumericVector smooth_robust(const NumericVector& x_in,
                            const NumericVector& z_in,
                            const NumericVector& w_in,
                            const NumericVector& x_out,
                            const double h, int iterations = 3) {
  return smooth(x_in, z_in, w_in, x_out, RobustSmoother(iterations), h);
}

// [[Rcpp::export]]
NumericVector smooth_mean(const NumericVector& x_in,
                          const NumericVector& z_in,
                          const NumericVector& w_in,
                          const NumericVector& x_out,
                          const double h) {
  return smooth(x_in, z_in, w_in, x_out, MeanSmoother(), h);
}
