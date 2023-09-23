// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// get_angle
double get_angle(double x0, double y0, double x1, double y1);
RcppExport SEXP _recmap_get_angle(SEXP x0SEXP, SEXP y0SEXP, SEXP x1SEXP, SEXP y1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x0(x0SEXP);
    Rcpp::traits::input_parameter< double >::type y0(y0SEXP);
    Rcpp::traits::input_parameter< double >::type x1(x1SEXP);
    Rcpp::traits::input_parameter< double >::type y1(y1SEXP);
    rcpp_result_gen = Rcpp::wrap(get_angle(x0, y0, x1, y1));
    return rcpp_result_gen;
END_RCPP
}
// place_rectangle
DataFrame place_rectangle(double x0, double y0, double dx0, double dy0, double dx1, double dy1, double alpha);
RcppExport SEXP _recmap_place_rectangle(SEXP x0SEXP, SEXP y0SEXP, SEXP dx0SEXP, SEXP dy0SEXP, SEXP dx1SEXP, SEXP dy1SEXP, SEXP alphaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type x0(x0SEXP);
    Rcpp::traits::input_parameter< double >::type y0(y0SEXP);
    Rcpp::traits::input_parameter< double >::type dx0(dx0SEXP);
    Rcpp::traits::input_parameter< double >::type dy0(dy0SEXP);
    Rcpp::traits::input_parameter< double >::type dx1(dx1SEXP);
    Rcpp::traits::input_parameter< double >::type dy1(dy1SEXP);
    Rcpp::traits::input_parameter< double >::type alpha(alphaSEXP);
    rcpp_result_gen = Rcpp::wrap(place_rectangle(x0, y0, dx0, dy0, dx1, dy1, alpha));
    return rcpp_result_gen;
END_RCPP
}
// recmap
DataFrame recmap(DataFrame V, Rcpp::Nullable<DataFrame> E);
RcppExport SEXP _recmap_recmap(SEXP VSEXP, SEXP ESEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type V(VSEXP);
    Rcpp::traits::input_parameter< Rcpp::Nullable<DataFrame> >::type E(ESEXP);
    rcpp_result_gen = Rcpp::wrap(recmap(V, E));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_recmap_get_angle", (DL_FUNC) &_recmap_get_angle, 4},
    {"_recmap_place_rectangle", (DL_FUNC) &_recmap_place_rectangle, 7},
    {"_recmap_recmap", (DL_FUNC) &_recmap_recmap, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_recmap(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
