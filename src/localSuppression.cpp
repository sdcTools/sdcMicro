#include <Rcpp.h>
#include <R.h>

RcppExport SEXP comp(SEXP vec, SEXP val) {
  BEGIN_RCPP
  Rcpp::IntegerVector values(vec);
  Rcpp::IntegerVector compVal(val);
  int lenV = values.length();

  Rcpp::IntegerVector res(lenV);

  for ( int i=0; i<lenV; i++ ) {
    if ( values[i] != compVal[0] ) {
      res[i] = 1;
    }
  }

  return Rcpp::List::create(
      Rcpp::Named("result") = res
  );
  END_RCPP
}

