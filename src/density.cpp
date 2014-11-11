#include <Rcpp.h>
using namespace Rcpp;

inline double tricube(double x) {
  if (NumericVector::is_na(x)) return 0;
  x = fabs(x);
  if (x > 1) return 0;

  double y = 1 - x * x * x;
  return y * y * y;
}

// [[Rcpp::export]]
List density(NumericVector x, NumericVector w, double bw, double width,
             double from, double to, bool reflect = false) {
  int span = ceil(bw / width * 2) + 1;
  if (span <= 1) stop("Width is too large relative to bw");

  int n = x.size(), n_bins = (to - from) / width + 1;
  bool has_w = x.size() == 0;
  double total = 0;

  std::vector<double> out(n_bins);

  for (int i = 0; i < n; ++i) {
    double xi = x[i];
    double wi = (has_w ? w[i] : 1);
    total += wi;

    int start = (xi - bw - from) / width;
    int end = start + span;

    for (int j = start; j < end + 1; ++j) {
      // Pull out into own function
      // Make sure location is in range of output
      double j_adj = j;
      if (j < 0) {
        if (reflect) {
          j_adj = -j;
        } else {
          j_adj = 0;
        }
      } else if (j >= n_bins) {
        // Something like 1s complement?
        if (reflect) {
          j_adj = n_bins - (j - n_bins) - 1;
        } else {
          j_adj = n_bins - 1;
        }
      }

      double dist = (xi - (j_adj * width + from)) / bw;
      out[j_adj] += tricube(dist) * wi;
    }
  }

  NumericVector x_(n_bins), density_(n_bins);

  for (int i = 0; i < n_bins; ++i) {
    x_[i] = from + width * i;
    density_[i] = out[i] / (total * bw);
  }

  return List::create(
    _["x_"] = x_,
    _["density_"] = density_
  );
}
