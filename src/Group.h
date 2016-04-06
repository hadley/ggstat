#include <Rcpp.h>

class GroupFixed {
    double width_;
    double origin_;
    double max_;
    bool pad_;
    bool right_closed_;

  public:
    GroupFixed(double width, double origin = 0,
               bool pad = false, bool right_closed = true)
       : width_(width), origin_(origin), pad_(pad),
         right_closed_(right_closed) {

      if (width <= 0)
        Rcpp::stop("`width` must be positive");
    }

    void init(const Rcpp::DoubleVector& x) {
      max_ = -INFINITY;
      int n = x.size();

      for(int i = 0; i < n; ++i) {
        if (x[i] == INFINITY) continue;
        // Normal FP ops ensure that NA and -Inf don't increase max
        if (x[i] > max_)
          max_ = x[i];
      }
    }

    int bin(double x) const {
      if (!R_finite(x))
        return 0;

      // If very close to boundary, prefer closed side.
      // 1e-8 =~ sqrt(.Machine$double.eps)
      double bin = (x - origin_) / width_ + (right_closed_ ? -1e-8 : 1e-8);
      if (bin < 0)
        return 0;

      return bin + 1 + (pad_ ? 1 : 0);
    }

    int nbins() const {
      // number of bins is largest bin
      // + 1 for bin zero
      // + 1 for empty padding bin on right
      return bin(max_) + 1 + (pad_ ? 1 : 0);
    }

    Rcpp::List outColumns() const {
      int n = nbins();
      Rcpp::NumericVector xmin(n), xmax(n);

      xmin[0] = NA_REAL;
      xmax[0] = NA_REAL;

      for (int i = 1; i < n; ++i) {
        xmin[i] = left_side(i);
        xmax[i] = left_side(i + 1);
      }

      return Rcpp::List::create(
        Rcpp::_["xmin_"] = xmin,
        Rcpp::_["xmax_"] = xmax
      );
    }

private:
    double left_side(int bin) const {
      if (bin < 0) return(NA_REAL);
      return (bin - 1 - (pad_ ? 1 : 0)) * width_ + origin_;
    }
};

// -----------------------------------------------------------------------------

class GroupVariable {
    std::vector<double> breaks_;
    bool right_closed_;

  public:
    GroupVariable(std::vector<double> breaks, bool right_closed = true)
       : breaks_(breaks), right_closed_(right_closed) {

      if (breaks.size() < 1)
        Rcpp::stop("`breaks` must have at least one element");

      // Ensure breaks are in ascending order
      std::sort(breaks.begin(), breaks.end());
    }

    void init(const Rcpp::DoubleVector& x) {
    }

    int bin(double x) const {
      if (ISNAN(x))
        return 0;

      std::vector<double>::const_iterator it = (right_closed_) ?
        std::lower_bound(breaks_.begin(), breaks_.end(), x) :
        std::upper_bound(breaks_.begin(), breaks_.end(), x);

      return (it - breaks_.begin()) + 1;
    }

    int nbins() const {
      // + 1 for NAs
      // + 1 more interval than breaks
      return breaks_.size() + 1 + 1;
    }

    Rcpp::List outColumns() const {
      int n = nbins();
      Rcpp::NumericVector xmin(n), xmax(n);

      xmin[0] = NA_REAL;
      xmax[0] = NA_REAL;

      for (int i = 1; i < n; ++i) {
        xmin[i] = i == 1 ?       -INFINITY : breaks_[i - 2];
        xmax[i] = i == (n - 1) ?  INFINITY : breaks_[i - 1];
      }

      return Rcpp::List::create(
        Rcpp::_["xmin_"] = xmin,
        Rcpp::_["xmax_"] = xmax
      );
    }
};
