#include <Rcpp.h>
using namespace Rcpp;

/*
 * calculates distances in a matrix and returns
 * indices with minimal distance > 0
 * used in kAnon() and localSuppression()
*/

// [[Rcpp::export]]
List cpp_calcSuppInds(NumericMatrix inp, NumericVector checkVals) {
  int N=inp.nrow();
  int nrKeys=checkVals.size();
  int minDist=nrKeys;
  int fk=0;
  NumericVector dists(N);
  NumericVector vn(1);
  LogicalVector naVals=is_na(checkVals);
  for (int i=0; i<N; i++) {
    int counter=0;
    for (int j=0; j<nrKeys; j++) {
      vn[0]=inp(i,j);
      bool res=any(is_na(vn));
      bool res_cV=naVals[j];
      bool res_all=all(is_na(vn));
      if ((res==false) and (res_cV==false) and (res_all==false) and (vn[0] != checkVals[j])) {
        counter=counter+1;
      }
    }
    /* update minimalDistance */
    if ((counter < minDist) and counter > 0) {
      minDist=counter;
    }
    /* update fk */
    if ( counter==0 ) {
      fk=fk+1;
    }
    dists[i]=counter;
  }

  IntegerVector v=seq_len(N);
  IntegerVector ids=v[dists==minDist];
  return Rcpp::List::create(
    Rcpp::Named("fk")=fk,
    //Rcpp::Named("dists")=dists,
    Rcpp::Named("ids")=ids);
}

