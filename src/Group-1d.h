#include <Rcpp.h>
SEXP restore_(SEXP old_, SEXP new_);

class GroupFixed {
    double width_;
    double min_, max_;
    bool right_closed_;

  public:
    GroupFixed(double width, double min, double max,
               bool right_closed = true)
       : width_(width), min_(min), max_(max),
         right_closed_(right_closed) {

      if (width <= 0)
        Rcpp::stop("`width` must be positive");
    }

    int bin(double x) const {
      if (!R_finite(x))
        return 0;
      if (x > max_)
        return 0;

      // If very close to boundary, prefer closed side.
      // 1e-8 =~ sqrt(.Machine$double.eps)
      double bin = (x - min_) / width_ + (right_closed_ ? -1e-8 : 1e-8);
      if (bin < 0)
        return 0;

      return bin + 1;
    }

    int nbins() const {
      // number of bins is largest bin + 1 for NA bin
      return bin(max_) + 1;
    }

    Rcpp::List outColumns(SEXP x) const {
      int n = nbins();
      Rcpp::NumericVector xmin(n), xmax(n);

      xmin[0] = NA_REAL;
      xmax[0] = NA_REAL;

      for (int i = 1; i < n; ++i) {
        xmin[i] = left_side(i);
        xmax[i] = left_side(i + 1);
      }

      return Rcpp::List::create(
        Rcpp::_["xmin_"] = restore_(x, xmin),
        Rcpp::_["xmax_"] = restore_(x, xmax)
      );
    }

    double left_side(int bin) const {
      if (bin < 0) return(NA_REAL);
      return (bin - 1) * width_ + min_;
    }
};

// -----------------------------------------------------------------------------

class GroupBreaks {
    std::vector<double> breaks_;
    bool right_closed_;

  public:
    GroupBreaks(std::vector<double> breaks, bool right_closed = true)
       : breaks_(breaks), right_closed_(right_closed) {

      if (breaks.size() < 1)
        Rcpp::stop("`breaks` must have at least one element");

      // Ensure breaks are in ascending order
      std::sort(breaks.begin(), breaks.end());
    }

    int bin(double x) const {
      if (ISNAN(x))
        return 0;

      if (x < breaks_[0] || x > breaks_[breaks_.size() - 1])
        return 0;

      std::vector<double>::const_iterator it = (right_closed_) ?
        std::lower_bound(breaks_.begin(), breaks_.end(), x) :
        std::upper_bound(breaks_.begin(), breaks_.end(), x);

      return (it - breaks_.begin());
    }

    int nbins() const {
      // + 1 for NA bin
      // - 1 less interval than breaks
      return breaks_.size();
    }

    Rcpp::List outColumns(SEXP x) const {
      int n = nbins();
      Rcpp::NumericVector xmin(n), xmax(n);

      xmin[0] = NA_REAL;
      xmax[0] = NA_REAL;

      for (int i = 1; i < n; ++i) {
        xmin[i] = breaks_[i - 1];
        xmax[i] = breaks_[i];
      }

      return Rcpp::List::create(
        Rcpp::_["xmin_"] = restore_(x, xmin),
        Rcpp::_["xmax_"] = restore_(x, xmax)
      );
    }
};
