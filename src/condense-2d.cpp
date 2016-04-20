#include <Rcpp.h>
#include "Group-2d.h"
#include "GroupHex.h"
#include "Condense.h"
using namespace Rcpp;

List makeDf(const List& x, const List& y, int n);

template<typename Group, typename Condenser>
List condense_2d(Group* pGroup,
                 Condenser* pCondenser,
                 const NumericVector& x,
                 const NumericVector& y,
                 const NumericVector& z,
                 const NumericVector& w) {

  bool has_w = (w.size() > 0);
  bool has_z = (z.size() > 0);

  pCondenser->init(pGroup->nbins());

  int n_obs = x.size();
  for(int i = 0; i < n_obs; ++i) {
    int bin = pGroup->bin(x[i], y[i]);
    pCondenser->push(bin, has_z ? z[i] : 1, has_w ? w[i] : 1);
  }

  return makeDf(
    pGroup->outColumns(x, y),
    pCondenser->outColumns(),
    pGroup->nbins()
  );
}

// [[Rcpp::export]]
List count_2d_fixed(const NumericVector& x,
                    const NumericVector& y,
                    const NumericVector& w,
                    double min_x, double min_y,
                    double max_x, double max_y,
                    double width_x, double width_y,
                    bool right_closed_x, bool right_closed_y) {
  GroupFixed
    groupX(width_x, min_x, max_x, right_closed_x),
    groupY(width_y, min_y, max_y, right_closed_y);
  Group2d<GroupFixed> grp(&groupX, &groupY);

  CondenseCount cnd;
  return condense_2d(&grp, &cnd, x, y, NumericVector::create(), w);
}

// [[Rcpp::export]]
List count_2d_hex(const NumericVector& x,
                  const NumericVector& y,
                  const NumericVector& w,
                  double min_x, double min_y,
                  double max_x, double max_y,
                  double width_x, double width_y) {
  GroupHex grp(width_x, width_y, min_x, min_y, max_x, max_y);
  CondenseCount cnd;

  return condense_2d(&grp, &cnd, x, y, NumericVector::create(), w);
}
