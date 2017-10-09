#include <Rcpp.h>
#include "RankSwap.h"
using namespace Rcpp;

// [[Rcpp::export]]
List rankSwap_argus_cpp(NumericMatrix inp, IntegerVector perc) {
  long nr_obs=inp.nrow();
  long nr_cols=inp.ncol();
  long v_perc=perc[0];
  long progress = 0;

  double** inputdata;
  inputdata = new double*[nr_obs];
  for (long i=0; i<nr_obs; i++)  {
    inputdata[i] = new double[nr_cols];
    for (long j=0; j<nr_cols; j++)  {
      inputdata[i][j] = inp(i,j);
    }
  }

  CRankSwap oRankSwap;
  long result=oRankSwap.rankswap(inputdata, nr_obs, nr_cols, v_perc, &progress);
  if (result == RNK_OK) {
    /* update matrix */
    for (long i=0; i<nr_obs; i++)  {
      for (long j=0; j<nr_cols; j++)  {
        inp(i,j)=inputdata[i][j];
      }
    }
  }
  return List::create(_["ret_code"]=result, _["inp"]=inp);
}