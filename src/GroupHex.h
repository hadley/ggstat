/*
 * Translated from
 * https://github.com/d3/d3-plugins/blob/master/hexbin/hexbin.js
 *
 * Copyright (C) 2013 Hadley Wickham
 * Copyright (C) 2012 Mike Bostock (mbostock at gmail dot com)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 *
 * * The name Michael Bostock may not be used to endorse or promote products
 *   derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL MICHAEL BOSTOCK BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
 * EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include <Rcpp.h>
#include "Group-1d.h"
SEXP restore_(SEXP old_, SEXP new_);

class GroupHex {
    const double width_x_, width_y_;
    const double min_x_,   min_y_;
    const double max_x_,   max_y_;

  public:
    GroupHex(double width_x, double width_y,
             double min_x,   double min_y,
             double max_x,   double max_y)
       : width_x_(width_x), width_y_(width_y),
         min_x_(min_x), min_y_(min_y),
         max_x_(max_x), max_y_(max_y) {
    }

    int bin(double x, double y) const {
      double py = findBin(y, width_y_, min_y_, max_y_);
      int pj = py;
      double py1 = py - pj;

      double px = findBin(x, width_x_, min_x_, max_x_) - (pj % 2 ? 0.5 : 0);
      int pi = px;

      if (px <= 0 || py <= 0)
        return 0;

      // If in corners, need to adjust
       if (std::abs(py1) * 3 > 1) {
        double px1 = px - pi,
               pi2 = pi + (px < pi ? -1 : 1) / 2,
               pj2 = pj + (py < pj ? -1 : 1),
               px2 = px - pi2,
               py2 = py - pj2;
        if (px1 * px1 + py1 * py1 > px2 * px2 + py2 * py2) {
          pi = pi2 + (pj % 2 ? 1 : -1) / 2;
          pj = pj2;
        }
      }
      return pi + pj * nbinsX();
    }

    double left(int x, int y) const {
      if (x < 0) return(NA_REAL);
      return ((x - 1) + (y % 2 ? 0.5 : 0)) * width_x_ + min_x_;
    }
    double bottom(int bin) const {
      if (bin < 0) return(NA_REAL);
      return (bin - 1) * width_y_ + min_y_;
    }

    Rcpp::List outColumns(SEXP x_template, SEXP y_template) const {
      int nx = nbinsX(), ny = nbinsY(), n = nx * ny;
      Rcpp::NumericVector x(n), y(n);

      int idx = 0;
      for (int i = 0; i < nx; ++i) {
        for (int j = 0; j < ny; ++j) {
          x[idx] = i == 0 ? NA_REAL : left(i, j);
          // xmax[idx] = i == 0 ? NA_REAL : left(i + 1);
          y[idx] = j == 0 ? NA_REAL : bottom(j);
          // ymax[idx] = j == 0 ? NA_REAL : bottom(j + 1);
          idx++;
        }
      }

      return Rcpp::List::create(
        Rcpp::_["x_"] = restore_(x_template, x),
        Rcpp::_["y_"] = restore_(y_template, y)
        // Rcpp::_["ymin_"] = restore_(x, ymin),
        // Rcpp::_["ymax_"] = restore_(x, ymax)
      );
    }

  int nbins() const {
    return nbinsX() * nbinsY();
  }

private:
  int nbinsX() const {
    return (max_x_ - min_x_) / width_x_ + 2;
  }
  int nbinsY() const {
    return (max_y_ - min_y_) / width_y_ + 2;
  }

};
