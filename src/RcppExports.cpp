// This file was generated by Rcpp::compileAttributes
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/ggstat.h"
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

// condense_count
List condense_count(const NumericVector& x, double origin, double width, bool pad, bool right_closed, const NumericVector& w);
static SEXP ggstat_condense_count_try(SEXP xSEXP, SEXP originSEXP, SEXP widthSEXP, SEXP padSEXP, SEXP right_closedSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type origin(originSEXP);
    Rcpp::traits::input_parameter< double >::type width(widthSEXP);
    Rcpp::traits::input_parameter< bool >::type pad(padSEXP);
    Rcpp::traits::input_parameter< bool >::type right_closed(right_closedSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type w(wSEXP);
    __result = Rcpp::wrap(condense_count(x, origin, width, pad, right_closed, w));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP ggstat_condense_count(SEXP xSEXP, SEXP originSEXP, SEXP widthSEXP, SEXP padSEXP, SEXP right_closedSEXP, SEXP wSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(ggstat_condense_count_try(xSEXP, originSEXP, widthSEXP, padSEXP, right_closedSEXP, wSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// condense_sum
List condense_sum(const NumericVector& x, double origin, double width, bool pad, bool right_closed, const NumericVector& z, const NumericVector& w);
static SEXP ggstat_condense_sum_try(SEXP xSEXP, SEXP originSEXP, SEXP widthSEXP, SEXP padSEXP, SEXP right_closedSEXP, SEXP zSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type origin(originSEXP);
    Rcpp::traits::input_parameter< double >::type width(widthSEXP);
    Rcpp::traits::input_parameter< bool >::type pad(padSEXP);
    Rcpp::traits::input_parameter< bool >::type right_closed(right_closedSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type z(zSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type w(wSEXP);
    __result = Rcpp::wrap(condense_sum(x, origin, width, pad, right_closed, z, w));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP ggstat_condense_sum(SEXP xSEXP, SEXP originSEXP, SEXP widthSEXP, SEXP padSEXP, SEXP right_closedSEXP, SEXP zSEXP, SEXP wSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(ggstat_condense_sum_try(xSEXP, originSEXP, widthSEXP, padSEXP, right_closedSEXP, zSEXP, wSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// condense_moments
List condense_moments(const NumericVector& x, double origin, double width, bool pad, bool right_closed, const NumericVector& z, const NumericVector& w, int moments);
static SEXP ggstat_condense_moments_try(SEXP xSEXP, SEXP originSEXP, SEXP widthSEXP, SEXP padSEXP, SEXP right_closedSEXP, SEXP zSEXP, SEXP wSEXP, SEXP momentsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type origin(originSEXP);
    Rcpp::traits::input_parameter< double >::type width(widthSEXP);
    Rcpp::traits::input_parameter< bool >::type pad(padSEXP);
    Rcpp::traits::input_parameter< bool >::type right_closed(right_closedSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type z(zSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type w(wSEXP);
    Rcpp::traits::input_parameter< int >::type moments(momentsSEXP);
    __result = Rcpp::wrap(condense_moments(x, origin, width, pad, right_closed, z, w, moments));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP ggstat_condense_moments(SEXP xSEXP, SEXP originSEXP, SEXP widthSEXP, SEXP padSEXP, SEXP right_closedSEXP, SEXP zSEXP, SEXP wSEXP, SEXP momentsSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(ggstat_condense_moments_try(xSEXP, originSEXP, widthSEXP, padSEXP, right_closedSEXP, zSEXP, wSEXP, momentsSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// condense_median
List condense_median(const NumericVector& x, double origin, double width, bool pad, bool right_closed, const NumericVector& z, const NumericVector& w);
static SEXP ggstat_condense_median_try(SEXP xSEXP, SEXP originSEXP, SEXP widthSEXP, SEXP padSEXP, SEXP right_closedSEXP, SEXP zSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type origin(originSEXP);
    Rcpp::traits::input_parameter< double >::type width(widthSEXP);
    Rcpp::traits::input_parameter< bool >::type pad(padSEXP);
    Rcpp::traits::input_parameter< bool >::type right_closed(right_closedSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type z(zSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type w(wSEXP);
    __result = Rcpp::wrap(condense_median(x, origin, width, pad, right_closed, z, w));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP ggstat_condense_median(SEXP xSEXP, SEXP originSEXP, SEXP widthSEXP, SEXP padSEXP, SEXP right_closedSEXP, SEXP zSEXP, SEXP wSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(ggstat_condense_median_try(xSEXP, originSEXP, widthSEXP, padSEXP, right_closedSEXP, zSEXP, wSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// count_lgl
List count_lgl(LogicalVector x, NumericVector w);
RcppExport SEXP ggstat_count_lgl(SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< LogicalVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    __result = Rcpp::wrap(count_lgl(x, w));
    return __result;
END_RCPP
}
// count_factor
List count_factor(IntegerVector x, NumericVector w);
RcppExport SEXP ggstat_count_factor(SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    __result = Rcpp::wrap(count_factor(x, w));
    return __result;
END_RCPP
}
// count_numeric
List count_numeric(NumericVector x, NumericVector w);
RcppExport SEXP ggstat_count_numeric(SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    __result = Rcpp::wrap(count_numeric(x, w));
    return __result;
END_RCPP
}
// count_string
List count_string(CharacterVector x, NumericVector w);
RcppExport SEXP ggstat_count_string(SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< CharacterVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    __result = Rcpp::wrap(count_string(x, w));
    return __result;
END_RCPP
}
// density
List density(NumericVector x, NumericVector w, double bw, double width, double from, double to, bool reflect);
RcppExport SEXP ggstat_density(SEXP xSEXP, SEXP wSEXP, SEXP bwSEXP, SEXP widthSEXP, SEXP fromSEXP, SEXP toSEXP, SEXP reflectSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    Rcpp::traits::input_parameter< double >::type bw(bwSEXP);
    Rcpp::traits::input_parameter< double >::type width(widthSEXP);
    Rcpp::traits::input_parameter< double >::type from(fromSEXP);
    Rcpp::traits::input_parameter< double >::type to(toSEXP);
    Rcpp::traits::input_parameter< bool >::type reflect(reflectSEXP);
    __result = Rcpp::wrap(density(x, w, bw, width, from, to, reflect));
    return __result;
END_RCPP
}
// frange_
NumericVector frange_(const NumericVector& x, const bool finite);
RcppExport SEXP ggstat_frange_(SEXP xSEXP, SEXP finiteSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const bool >::type finite(finiteSEXP);
    __result = Rcpp::wrap(frange_(x, finite));
    return __result;
END_RCPP
}
// frange_list
NumericVector frange_list(const ListOf<NumericVector>& x, const bool finite);
RcppExport SEXP ggstat_frange_list(SEXP xSEXP, SEXP finiteSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const ListOf<NumericVector>& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const bool >::type finite(finiteSEXP);
    __result = Rcpp::wrap(frange_list(x, finite));
    return __result;
END_RCPP
}
// abs_max_
double abs_max_(const NumericVector& x, const bool finite);
RcppExport SEXP ggstat_abs_max_(SEXP xSEXP, SEXP finiteSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
    Rcpp::traits::input_parameter< const bool >::type finite(finiteSEXP);
    __result = Rcpp::wrap(abs_max_(x, finite));
    return __result;
END_RCPP
}
// mt
NumericVector mt(NumericVector x, double lambda);
static SEXP ggstat_mt_try(SEXP xSEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    __result = Rcpp::wrap(mt(x, lambda));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP ggstat_mt(SEXP xSEXP, SEXP lambdaSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(ggstat_mt_try(xSEXP, lambdaSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// inv_mt
NumericVector inv_mt(NumericVector x, double lambda);
static SEXP ggstat_inv_mt_try(SEXP xSEXP, SEXP lambdaSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< double >::type lambda(lambdaSEXP);
    __result = Rcpp::wrap(inv_mt(x, lambda));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP ggstat_inv_mt(SEXP xSEXP, SEXP lambdaSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(ggstat_inv_mt_try(xSEXP, lambdaSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// smooth_linear
NumericVector smooth_linear(const NumericVector& x_in, const NumericVector& z_in, const NumericVector& w_in, const NumericVector& x_out, const double h);
static SEXP ggstat_smooth_linear_try(SEXP x_inSEXP, SEXP z_inSEXP, SEXP w_inSEXP, SEXP x_outSEXP, SEXP hSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const NumericVector& >::type x_in(x_inSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type z_in(z_inSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type w_in(w_inSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type x_out(x_outSEXP);
    Rcpp::traits::input_parameter< const double >::type h(hSEXP);
    __result = Rcpp::wrap(smooth_linear(x_in, z_in, w_in, x_out, h));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP ggstat_smooth_linear(SEXP x_inSEXP, SEXP z_inSEXP, SEXP w_inSEXP, SEXP x_outSEXP, SEXP hSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(ggstat_smooth_linear_try(x_inSEXP, z_inSEXP, w_inSEXP, x_outSEXP, hSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// smooth_robust
NumericVector smooth_robust(const NumericVector& x_in, const NumericVector& z_in, const NumericVector& w_in, const NumericVector& x_out, const double h, int iterations);
static SEXP ggstat_smooth_robust_try(SEXP x_inSEXP, SEXP z_inSEXP, SEXP w_inSEXP, SEXP x_outSEXP, SEXP hSEXP, SEXP iterationsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const NumericVector& >::type x_in(x_inSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type z_in(z_inSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type w_in(w_inSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type x_out(x_outSEXP);
    Rcpp::traits::input_parameter< const double >::type h(hSEXP);
    Rcpp::traits::input_parameter< int >::type iterations(iterationsSEXP);
    __result = Rcpp::wrap(smooth_robust(x_in, z_in, w_in, x_out, h, iterations));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP ggstat_smooth_robust(SEXP x_inSEXP, SEXP z_inSEXP, SEXP w_inSEXP, SEXP x_outSEXP, SEXP hSEXP, SEXP iterationsSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(ggstat_smooth_robust_try(x_inSEXP, z_inSEXP, w_inSEXP, x_outSEXP, hSEXP, iterationsSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// smooth_mean
NumericVector smooth_mean(const NumericVector& x_in, const NumericVector& z_in, const NumericVector& w_in, const NumericVector& x_out, const double h);
static SEXP ggstat_smooth_mean_try(SEXP x_inSEXP, SEXP z_inSEXP, SEXP w_inSEXP, SEXP x_outSEXP, SEXP hSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::traits::input_parameter< const NumericVector& >::type x_in(x_inSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type z_in(z_inSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type w_in(w_inSEXP);
    Rcpp::traits::input_parameter< const NumericVector& >::type x_out(x_outSEXP);
    Rcpp::traits::input_parameter< const double >::type h(hSEXP);
    __result = Rcpp::wrap(smooth_mean(x_in, z_in, w_in, x_out, h));
    return __result;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP ggstat_smooth_mean(SEXP x_inSEXP, SEXP z_inSEXP, SEXP w_inSEXP, SEXP x_outSEXP, SEXP hSEXP) {
    SEXP __result;
    {
        Rcpp::RNGScope __rngScope;
        __result = PROTECT(ggstat_smooth_mean_try(x_inSEXP, z_inSEXP, w_inSEXP, x_outSEXP, hSEXP));
    }
    Rboolean __isInterrupt = Rf_inherits(__result, "interrupted-error");
    if (__isInterrupt) {
        UNPROTECT(1);
        Rf_onintr();
    }
    Rboolean __isError = Rf_inherits(__result, "try-error");
    if (__isError) {
        SEXP __msgSEXP = Rf_asChar(__result);
        UNPROTECT(1);
        Rf_error(CHAR(__msgSEXP));
    }
    UNPROTECT(1);
    return __result;
}
// as_data_frame
void as_data_frame(List x, int nrow);
RcppExport SEXP ggstat_as_data_frame(SEXP xSEXP, SEXP nrowSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< List >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type nrow(nrowSEXP);
    as_data_frame(x, nrow);
    return R_NilValue;
END_RCPP
}
// weightedQuantile
NumericVector weightedQuantile(NumericVector x, IntegerVector w, NumericVector probs);
RcppExport SEXP ggstat_weightedQuantile(SEXP xSEXP, SEXP wSEXP, SEXP probsSEXP) {
BEGIN_RCPP
    Rcpp::RObject __result;
    Rcpp::RNGScope __rngScope;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type w(wSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type probs(probsSEXP);
    __result = Rcpp::wrap(weightedQuantile(x, w, probs));
    return __result;
END_RCPP
}

// validate (ensure exported C++ functions exist before calling them)
static int ggstat_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("List(*condense_count)(const NumericVector&,double,double,bool,bool,const NumericVector&)");
        signatures.insert("List(*condense_sum)(const NumericVector&,double,double,bool,bool,const NumericVector&,const NumericVector&)");
        signatures.insert("List(*condense_moments)(const NumericVector&,double,double,bool,bool,const NumericVector&,const NumericVector&,int)");
        signatures.insert("List(*condense_median)(const NumericVector&,double,double,bool,bool,const NumericVector&,const NumericVector&)");
        signatures.insert("NumericVector(*mt)(NumericVector,double)");
        signatures.insert("NumericVector(*inv_mt)(NumericVector,double)");
        signatures.insert("NumericVector(*smooth_linear)(const NumericVector&,const NumericVector&,const NumericVector&,const NumericVector&,const double)");
        signatures.insert("NumericVector(*smooth_robust)(const NumericVector&,const NumericVector&,const NumericVector&,const NumericVector&,const double,int)");
        signatures.insert("NumericVector(*smooth_mean)(const NumericVector&,const NumericVector&,const NumericVector&,const NumericVector&,const double)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP ggstat_RcppExport_registerCCallable() { 
    R_RegisterCCallable("ggstat", "ggstat_condense_count", (DL_FUNC)ggstat_condense_count_try);
    R_RegisterCCallable("ggstat", "ggstat_condense_sum", (DL_FUNC)ggstat_condense_sum_try);
    R_RegisterCCallable("ggstat", "ggstat_condense_moments", (DL_FUNC)ggstat_condense_moments_try);
    R_RegisterCCallable("ggstat", "ggstat_condense_median", (DL_FUNC)ggstat_condense_median_try);
    R_RegisterCCallable("ggstat", "ggstat_mt", (DL_FUNC)ggstat_mt_try);
    R_RegisterCCallable("ggstat", "ggstat_inv_mt", (DL_FUNC)ggstat_inv_mt_try);
    R_RegisterCCallable("ggstat", "ggstat_smooth_linear", (DL_FUNC)ggstat_smooth_linear_try);
    R_RegisterCCallable("ggstat", "ggstat_smooth_robust", (DL_FUNC)ggstat_smooth_robust_try);
    R_RegisterCCallable("ggstat", "ggstat_smooth_mean", (DL_FUNC)ggstat_smooth_mean_try);
    R_RegisterCCallable("ggstat", "ggstat_RcppExport_validate", (DL_FUNC)ggstat_RcppExport_validate);
    return R_NilValue;
}