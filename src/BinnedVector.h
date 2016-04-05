#include <Rcpp.h>

// Wrapper for numeric vector that makes it easy figure to out which
// bin each observation belongs to.
class BinnedVector {
    double width_;
    double origin_;
    double max_;
    bool pad_;
    bool right_closed_;

  public:
    BinnedVector(double width, double origin = 0,
                 bool pad = false, bool right_closed = true)
       : width_(width), origin_(origin), pad_(pad),
         right_closed_(right_closed) {

      if (width <= 0) Rcpp::stop("Width must be positive");
    }

    void init(const Rcpp::DoubleVector& x) {
      // Compute and cache maximum
      max_ = -INFINITY;
      int n = x.size();

      for(int i = 0; i < n; ++i) {
        if (x[i] == INFINITY) continue;
        // Normal FP ops ensure that NA and -Inf don't increase max
        if (x[i] > max_) max_ = x[i];
      }
    }

    int bin(double x) const {
      if (!R_finite(x)) return 0;

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
      return bin(max_) + 1 + (pad_ ? : 0);
    }

    double origin() const {
      return origin_;
    }

    double width() const {
      return width_;
    }

    Rcpp::List outColumns() const {
      int n = nbins();
      Rcpp::NumericVector xmin(n), xmax(n);
      for (int i = 0; i < n; ++i) {
        double x = unbin(i);
        xmin[i] = x - width_ / 2;
        xmax[i] = x + width_ / 2;
      }

      return Rcpp::List::create(
        Rcpp::_["xmin_"] = xmin,
        Rcpp::_["xmax_"] = xmax
      );
    }
};
