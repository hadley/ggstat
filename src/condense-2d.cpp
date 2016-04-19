#include <Rcpp.h>
#include "Group.h"
#include "Condense.h"
using namespace Rcpp;


List makeDf(const List& x, const List& y, int n);

template<typename Group>
class Group2d {
  Group* pGroupX_;
  Group* pGroupY_;

public:

  Group2d(Group* pGroupX, Group* pGroupY) :
    pGroupX_(pGroupX), pGroupY_(pGroupY) {
  }

  int nbins() const {
    return pGroupX_->nbins() * pGroupY_->nbins();
  }

  int bin(double x, double y) {
    return pGroupX_->bin(x) + pGroupY_->bin(y) * pGroupX_->nbins();
  }

  Rcpp::List outColumns(SEXP x, SEXP y) const {
    int nx = pGroupX_->nbins(), ny = pGroupY_->nbins(), n = nx * ny;
    Rcpp::NumericVector xmin(n), xmax(n), ymin(n), ymax(n);

    int idx = 0;
    for (int i = 0; i < nx; ++i) {
      for (int j = 0; j < ny; ++j) {
        xmin[idx] = i == 0 ? NA_REAL : pGroupX_->left_side(i);
        xmax[idx] = i == 0 ? NA_REAL : pGroupX_->left_side(i + 1);
        ymin[idx] = j == 0 ? NA_REAL : pGroupY_->left_side(j);
        ymax[idx] = j == 0 ? NA_REAL : pGroupY_->left_side(j + 1);
        idx++;
      }
    }

    return Rcpp::List::create(
      Rcpp::_["xmin_"] = restore_(x, xmin),
      Rcpp::_["xmax_"] = restore_(x, xmax),
      Rcpp::_["ymin_"] = restore_(x, ymin),
      Rcpp::_["ymax_"] = restore_(x, ymax)
    );
  }
};



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
