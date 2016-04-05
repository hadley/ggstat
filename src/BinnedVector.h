#include <Rcpp.h>

// Wrapper for numeric vector that makes it easy figure to out which
// bin each observation belongs to.
class BinnedVector {
    const Rcpp::NumericVector& x_;
    double width_;
    double origin_;
    double max_;
    bool pad_;
    bool right_closed_;

  public:
    BinnedVector(const Rcpp::NumericVector& x, double width, double origin = 0,
                 bool pad = false, bool right_closed = true)
       : x_(x), width_(width), origin_(origin), pad_(pad),
         right_closed_(right_closed) {

      if (width <= 0) Rcpp::stop("Width must be positive");

      // Compute and cache maximum
      max_ = -INFINITY;
      int n = x_.size();

      for(int i = 0; i < n; ++i) {
        if (x_[i] == INFINITY) continue;
        // Normal FP ops ensure that NA and -Inf don't increase max
        if (x_[i] > max_) max_ = x_[i];
      }
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
      return bin(max_) + 1 + (pad_ ? : 0);
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

    template<typename Condenser>
    Rcpp::List output(const Condenser& condenser,
                      const std::vector<Condenser>& condensers) const {

      bool has_na = !condensers[0].empty();
      int offset = has_na ? 0 : 1;
      int n_out = nbins() - offset;

      // Compute values from condensers and determine bins
      int n_condensers = condenser.size();
      Rcpp::List out(n_condensers + 3);
      Rcpp::CharacterVector out_cols(n_condensers + 3);

      // First thre columns give bin info
      Rcpp::NumericVector x_(n_out), xmin_(n_out), xmax_(n_out);
      for (int i = 0; i < n_out; ++i) {
        double x = unbin(i + offset);
        x_[i] = x;
        xmin_[i] = x - width_ / 2;
        xmax_[i] = x + width_ / 2;
      }

      out[0] = x_;     out_cols[0] = "x_";
      out[1] = xmin_;  out_cols[1] = "xmin_";
      out[2] = xmax_;  out_cols[2] = "xmax_";

      // Last columns give summaries from condensers
      for (int j = 0; j < n_condensers; ++j) {
        Rcpp::NumericVector condensed(n_out);
        for (int i = 0; i < n_out; ++i) {
          condensed[i] = condensers[i + offset].compute(j);
        }
        out[j + 3] = condensed;
        out_cols[j + 3] = condenser.name(j);
      }

      out.attr("names") = out_cols;
      return out;
    }

};
