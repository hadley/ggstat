#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector weightedQuantile(NumericVector x, IntegerVector w,
                               NumericVector probs) {
  int n = x.size(), m = probs.size();
  double sum = 0;
  std::vector<std::pair<double, int> > values(n);

  if (w.size() == 0) {
    for(int i = 0; i < n; ++i) {
      values[i] = std::make_pair(x[i], 1);
      sum++;
    }
  } else if (w.size() == n) {
    for(int i = 0; i < n; ++i) {
      int wi = w[i];
      values[i] = std::make_pair(x[i], wi <= 0 ? 0 : wi);
      sum += wi;
    }
  } else {
    stop("w is not the same size as x");
  }
  std::sort(values.begin(), values.end());

  std::vector<std::pair<double, int> >::iterator v_it = values.begin(),
    v_end = values.end();
  NumericVector quantiles(m);
  double cur_pos = 0;
  double next_q = 1 + probs[0] * (sum - 1);
  double cur_val = NAN, last_val = NAN;
  if (next_q < 0 || next_q > sum) stop("Invalid quantile");

  for(int q = 0; v_it != v_end; ++v_it) {
    cur_val = v_it->first;
    cur_pos += v_it->second;
    while (cur_pos >= next_q) {
      if (cur_pos == next_q || v_it == values.begin()) {
        // Quantile is exactly on data value
        quantiles[q] = cur_val;
      } else {
        // Just missed it - interpolation between this value and last
        double alpha = next_q - floor(next_q);
        quantiles[q] = (1 - alpha) * last_val + alpha * cur_val;
      }

      // Advance to next quantile
      q++;

      // Found all the quantiles we're looking for
      if (q >= m) break;

      next_q = 1 + probs[q] * (sum - 1);
      if (next_q < 0 || next_q > sum) stop("Invalid quantile");
    }

    last_val = cur_val;
    if (q >= m) break;
  }

  return quantiles;
}
// Q[i](p) = (1 - g) x[j] + g x[j+1]
// j = floor(np + m)
// g = np + m - j
//
// For type 7:
//   m = 1 - p =>
//   j = floor(1 + (n - 1) * p)
//   g = (np + 1 - p) - floor(1 + (n - 1) * p)

