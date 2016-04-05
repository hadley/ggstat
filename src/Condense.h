#include <Rcpp.h>
#include "median.h"

class CondenseMoments {
    int n_;
    std::vector<double> weight_, mean_, m2_;

  public:
    void init(int n) {
      n_ = n;
      weight_.resize(n);
      mean_.resize(n);
      m2_.resize(n);
    }

    // Algorithm adapted from
    // http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Weighted_incremental_algorithm
    void push(int i, double y, double w) {
      if (Rcpp::NumericVector::is_na(y)) return;

      // counts and weights
      weight_[i] += w;

      // mean
      double delta = y - mean_[i];
      mean_[i] += delta * w / weight_[i];

      // variance
      m2_[i] += delta * delta * w * (1 - w / weight_[i]);

      return;
    }

    Rcpp::List outColumns() {
      Rcpp::NumericVector mean(n_), sd(n_);

      for (int i = 0; i < n_; ++i) {
        if (weight_[i] == 0) {
          mean[i] = NAN;
          sd[i] = NAN;
        } else {
          mean[i] = mean_[i];
          sd[i] = std::pow(m2_[i] / (weight_[i] - 1), 0.5);
        }
      }

      return Rcpp::List::create(
        Rcpp::_["count_"] = weight_,
        Rcpp::_["mean_"] = mean,
        Rcpp::_["sd_"] = sd
      );
    }
};

class CondenseSum {
    int n_;
    std::vector<double> weight, sum;

  public:
    void init(int n) {
      n_ = n;
      weight.resize(n_);
      sum.resize(n_);
    }

    void push(int i, double y, double w) {
      if (Rcpp::NumericVector::is_na(y)) return;

      weight[i] += w;
      sum[i] += y * w;
    }

    Rcpp::List outColumns() const {
      return Rcpp::List::create(
        Rcpp::_["count_"] = weight,
        Rcpp::_["sum_"] = sum
      );
    }
};

class CondenseCount {
    int n_;
    std::vector<double> weight;

  public:
    void init(int n) {
      n_ = n;
      weight.resize(n_);
    }

    void push(int i, double y, double w) {
      if (Rcpp::NumericVector::is_na(y)) return;

      weight[i] += w;
    }

    Rcpp::List outColumns() const {
      return Rcpp::List::create(
        Rcpp::_["count_"] = weight
      );
    }
};


class CondenseMedian {
  // Needs to be mutable because median modifies in place for performance
  // But external interface is still immutable
  int n_;
  mutable std::vector<std::vector<double> > ys;

  public:
    void init(int n) {
      n_ = n;
      ys.resize(n_);
    }

    void push(int i, double y, double w) {
      if (Rcpp::NumericVector::is_na(y)) return;

      ys[i].push_back(y);
    }

    Rcpp::List outColumns() const {
      Rcpp::NumericVector out(n_);
      for (int i = 0; i < n_; ++i) {
        out[i] = median(&ys[i]);
      }

      return Rcpp::List::create(Rcpp::_["median_"] = out);
    }
};
