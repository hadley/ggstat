#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export("`as.data.frame!`")]]
void as_data_frame(List x, int nrow) {
   x.attr("class") = "data.frame";
   x.attr("row.names") = IntegerVector::create(NA_INTEGER, -nrow);
   if (Rf_isNull(Rf_getAttrib(x, R_NamesSymbol))) {
      stop("List must have 'names' attribute set");
   }
}

SEXP restore_(SEXP old_, SEXP new_) {
  Function restore("restore");
  return restore(old_, new_);
}

List makeDf(const List& x, const List& y, int n) {
  int p = x.size() + y.size();
  List both(p);
  CharacterVector
    xNames = x.attr("names"),
    yNames = y.attr("names"),
    bothNames(p);

  for (int i = 0; i < x.size(); ++i) {
    if (Rf_length(x[i]) != n)
      stop("Invalid length x col %i", i + 1);
    both[i] = x[i];
    bothNames[i] = xNames[i];
  }
  for (int i = 0; i < y.size(); ++i) {
    if (Rf_length(y[i]) != n)
      stop("Invalid length y col %i", i + 1);
    both[i + x.size()] = y[i];
    bothNames[i + x.size()] = yNames[i];
  }

  both.attr("names") = bothNames;
  both.attr("class") = CharacterVector::create("tbl_df", "tbl", "data.frame");
  both.attr("row.names") = IntegerVector::create(NA_INTEGER, -n);

  return both;
}
