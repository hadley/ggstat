#include <Rcpp.h>
#include "Group.h"
#include "Condense.h"
using namespace Rcpp;

List makeDf(const List& x, const List& y, int n);

template<typename Group, typename Condenser>
List condense(Group* pGroup,
              Condenser* pCondenser,
              const NumericVector& x,
              const NumericVector& z,
              const NumericVector& w) {

  bool has_w = (w.size() > 0);
  bool has_z = (z.size() > 0);

  pCondenser->init(pGroup->nbins());

  int n_obs = x.size();
  for(int i = 0; i < n_obs; ++i) {
    int bin = pGroup->bin(x[i]);
    pCondenser->push(bin, has_z ? z[i] : 1, has_w ? w[i] : 1);
  }

  return makeDf(
    pGroup->outColumns(x),
    pCondenser->outColumns(),
    pGroup->nbins()
  );
}

// [[Rcpp::export]]
List count_fixed(const NumericVector& x, const NumericVector& w,
                 double min, double max,
                 double width, bool right_closed) {
  GroupFixed grp(width, min, max, right_closed);
  CondenseCount cnd;

  return condense(&grp, &cnd, x, NumericVector::create(), w);
}

// [[Rcpp::export]]
List count_variable(const NumericVector& x, const NumericVector& w,
                   std::vector<double> breaks, bool right_closed) {
  GroupVariable grp(breaks);
  CondenseCount cnd;

  return condense(&grp, &cnd, x, NumericVector::create(), w);
}



// [[Rcpp::export]]
List condense_sum(const NumericVector& x, double min, double max,
                  double width, bool right_closed,
                  const NumericVector& z, const NumericVector& w) {
  GroupFixed grp(width, min, max, right_closed);
  CondenseSum cnd;

  return condense(&grp, &cnd, x, z, w);
}

// [[Rcpp::export]]
List condense_moments(const NumericVector& x, double min, double max,
                      double width, bool right_closed,
                      const NumericVector& z, const NumericVector& w) {
  GroupFixed grp(width, min, max, right_closed);
  CondenseMoments cnd;

  return condense(&grp, &cnd, x, z, w);
}

// [[Rcpp::export]]
List condense_median(const NumericVector& x, double min, double max,
                     double width, bool right_closed,
                     const NumericVector& z, const NumericVector& w) {
  GroupFixed grp(width, min, max, right_closed);
  CondenseMedian cnd;

  return condense(&grp, &cnd, x, z, w);
}
