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
