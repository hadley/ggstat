#include <Rcpp.h>
#include "BinnedVector.h"
#include "condensers.h"
using namespace Rcpp;

template<typename Group, typename Condenser>
List condense(const Group& group,
              const Condenser& condenser,
              const NumericVector& z,
              const NumericVector& w) {
  bool has_w = (w.size() > 0);
  bool has_z = (z.size() > 0);

  int n_bins = group.nbins();
  std::vector<Condenser> condensers(n_bins, condenser);

  int n_obs = group.size();
  for(int i = 0; i < n_obs; ++i) {
    int bin = group.bin_i(i);
    condensers[bin].push(has_z ? z[i] : 1, has_w ? w[i] : 1);
  }

  return group.output(condenser, condensers);
}

// [[Rcpp::export]]
List condense_count(const NumericVector& x, double origin, double width,
                    bool pad, bool right_closed,
                    const NumericVector& w) {
  BinnedVector group(x, width, origin, pad, right_closed);
  return condense(group, SumCondenser(0), NumericVector::create(), w);
}

// [[Rcpp::export]]
List condense_sum(const NumericVector& x, double origin, double width,
                  bool pad, bool right_closed,
                  const NumericVector& z, const NumericVector& w) {
  BinnedVector group(x, width, origin, pad, right_closed);
  return condense(group, SumCondenser(1), z, w);
}

// [[Rcpp::export]]
List condense_moments(const NumericVector& x, double origin, double width,
                      bool pad, bool right_closed,
                      const NumericVector& z, const NumericVector& w,
                      int moments) {

  BinnedVector group(x, width, origin, pad, right_closed);
  return condense(group, MomentCondenser(moments), z, w);
}

// [[Rcpp::export]]
List condense_median(const NumericVector& x, double origin, double width,
                     bool pad, bool right_closed,
                     const NumericVector& z, const NumericVector& w) {
  BinnedVector group(x, width, origin, pad, right_closed);
  return condense(group, MedianCondenser(), z, w);
}
