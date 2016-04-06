#include <Rcpp.h>
#include "Group.h"
using namespace Rcpp;

// [[Rcpp::export]]
List group_fixed(NumericVector x, double width, double origin = 0,
                              bool pad = false, bool right_closed = false) {

  GroupFixed grp(width, origin, pad, right_closed);

  int n = x.size();
  IntegerVector out(n);

  for (int i = 0; i < n; ++i) {
    out[i] = grp.bin(x[i]);
  }

  return List::create(
    _["x"] = out,
    _["bins"] = grp.outColumns()
  );
}
