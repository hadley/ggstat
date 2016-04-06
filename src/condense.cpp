#include <Rcpp.h>
#include "Group.h"
#include "Condense.h"
using namespace Rcpp;

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

  List grpCols = pGroup->outColumns(), outCols = pCondenser->outColumns();

  int p = grpCols.size() + outCols.size();
  List both(p);
  CharacterVector
    grpNames = grpCols.attr("names"),
    outNames = outCols.attr("names"),
    bothNames(p);

  for (int i = 0; i < grpCols.size(); ++i) {
    both[i] = grpCols[i];
    bothNames[i] = grpNames[i];
  }
  for (int i = 0; i < outCols.size(); ++i) {
    both[i + grpCols.size()] = outCols[i];
    bothNames[i + grpCols.size()] = outNames[i];
  }

  both.attr("names") = bothNames;
  both.attr("class") = CharacterVector::create("tbl_df", "tbl", "data.frame");
  both.attr("row.names") = IntegerVector::create(NA_INTEGER, -pGroup->nbins());

  return both;
}

// [[Rcpp::export]]
List condense_count(const NumericVector& x, double min, double max,
                    double width, bool pad, bool right_closed,
                    const NumericVector& w) {
  GroupFixed grp(width, min, max, pad, right_closed);
  CondenseCount cnd;

  return condense(&grp, &cnd, x, NumericVector::create(), w);
}

// [[Rcpp::export]]
List condense_sum(const NumericVector& x, double min, double max,
                  double width, bool pad, bool right_closed,
                  const NumericVector& z, const NumericVector& w) {
  GroupFixed grp(width, min, max, pad, right_closed);
  CondenseSum cnd;

  return condense(&grp, &cnd, x, z, w);
}

// [[Rcpp::export]]
List condense_moments(const NumericVector& x, double min, double max,
                      double width, bool pad, bool right_closed,
                      const NumericVector& z, const NumericVector& w) {
  GroupFixed grp(width, min, max, pad, right_closed);
  CondenseMoments cnd;

  return condense(&grp, &cnd, x, z, w);
}

// [[Rcpp::export]]
List condense_median(const NumericVector& x, double min, double max,
                     double width, bool pad, bool right_closed,
                     const NumericVector& z, const NumericVector& w) {
  GroupFixed grp(width, min, max, pad, right_closed);
  CondenseMedian cnd;

  return condense(&grp, &cnd, x, z, w);
}
