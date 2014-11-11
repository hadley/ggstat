#include <Rcpp.h>

// Wrapper for numeric vector that makes it easy figure to out which
// bin each observation belongs to.
class BinnedVector {
    const Rcpp::NumericVector& x_;
    double width_;
    double origin_;
    bool pad_;
    bool right_closed_;

  public:
    BinnedVector(const Rcpp::NumericVector& x, double width, double origin = 0,
                 bool pad = false, bool right_closed = true)
       : x_(x), width_(width), origin_(origin), pad_(pad),
         right_closed_(right_closed) {

      if (width <= 0) Rcpp::stop("Width must be positive");
    }

    int bin_i(int i) const {
      return bin(x_[i]);
    }

    int bin(double x) const {
      if (ISNAN(x) || x == INFINITY || x == -INFINITY) return 0;

      double x_adj = x;
      // If very close to boundary, prefer closed side.
      // 1e-8 =~ sqrt(.Machine$double.eps)
      if (right_closed_) {
        x_adj -= width_ * 1e-8;
      } else {
        x_adj += width_ * 1e-8;
      }

      return (x_adj - origin_) / width_ + 1 + (pad_ ? 1 : 0);
    }

    double unbin(int bin) const {
      if (bin == 0) return(NA_REAL);
      return (bin - 1 - (pad_ ? 1 : 0)) * width_ + origin_ + width_ / 2;
    }

    int nbins() const {
      double max = -INFINITY;

      int n = x_.size();
      for(int i = 0; i < n; ++i) {
        if (x_[i] == INFINITY) continue;
        // Normal FP ops ensure that NA and -Inf don't increase max
        if (x_[i] > max) max = x_[i];
      }

      return bin(max) + 1 + (pad_ ? 1 : 0);
    }

    int size() const {
      return x_.size();
    }

    double origin() const {
      return origin_;
    }

    double width() const {
      return width_;
    }

};
