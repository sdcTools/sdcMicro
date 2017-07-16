#include <Rcpp.h>
using namespace Rcpp;

/*
 * calculates distances in a matrix and returns
 * indices with minimal distance > 0
 * used in kAnon() and localSuppression()
*/

// [[Rcpp::export]]
List cpp_calcSuppInds(NumericMatrix inp, NumericVector checkVals, List params) {
  int N=inp.nrow();
  int nrKeys=checkVals.size();
  int minDist=nrKeys;
  double fk=0;
  double alpha=params["alpha"];
  int checkValsId=params["id"];

  NumericVector dists(N);
  NumericVector vn(1);
  LogicalVector naVals=is_na(checkVals);
  bool naInCompKey=false;
  for (int i=0; i<N; i++) {
    int counter=0;
    for (int j=0; j<nrKeys; j++) {
      vn=inp(i,j);
      bool res=is_na(vn)[0];
      if (res==true) {
        naInCompKey=true;
      }
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
    if (counter==0) {
      if (naInCompKey==false) {
        fk=fk+1;
      } else {
        fk=fk+alpha;
      }
    }
    dists[i]=counter;
  }

  IntegerVector v=seq_len(N);
  IntegerVector ids=v[dists==minDist];

  // some special case which may occur (only) if alpha!=1
  // we can have a key with no missing value and all matching keys (dist=0) have at least one NA!
  if ((sum(dists)==0) & (alpha<1)) {
    ids=checkValsId;
  }
  return Rcpp::List::create(
    Rcpp::Named("fk")=fk,
    //Rcpp::Named("dists")=dists,
    Rcpp::Named("ids")=ids);
}

