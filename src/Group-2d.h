#include <Rcpp.h>
#include "Group-1d.h"

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
