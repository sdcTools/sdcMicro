#include <Rcpp.h>
#include "microAggregation.h"
using namespace Rcpp;

// [[Rcpp::export]]
List microaggregation_argus_cpp(NumericMatrix inp, NumericVector k, NumericVector useOptimal) {

  //long Do_Opt_Mic ( long n_el, long n_var, long k, double **out_data)
  long n_el=inp.nrow();
  long n_var=inp.ncol();
  long elms_p_group=k[0];

  double** inputdata;
  inputdata = new double*[n_el];
  for (long i=0; i<n_el; i++)  {
    inputdata[i] = new double[n_var];
    for (long j=0; j<n_var; j++)  {
      inputdata[i][j] = inp(i,j);
    }
  }

  /* optimal variant */
  long optim=useOptimal[0];

  /* group_var */
  /* for now, microaggregation cannot be applied per group;
   * only in such a case, the variables below are relevant!
  */
  long group_var=1;
  long how_many=0;
  long var=0;

  CMicroAggregation oMicroaggregation;

  long result=oMicroaggregation.Microaggregation(n_var, n_el, elms_p_group, group_var, &how_many, &var, inputdata, optim);
  if (result == MIC_OK) {
    /* update matrix */
    for (long i=0; i<n_el; i++)  {
      for (long j=0; j<n_var; j++)  {
        inp(i,j)=inputdata[i][j];
      }
    }
  }
  //long result=1;
  return List::create(_["ret_code"]=result, _["inp"]=inp);
}