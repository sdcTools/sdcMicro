#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include "RankSwap.h"
#include <Rcpp.h>

long my_rand() {
  // a random number between 0 and RAND_MAX (including)
  long maxval = RAND_MAX;
  long x = R::runif(0, 1)*(maxval+1);
  return(x);
}

/* ========================================================================= */
/* Masks the data array which has n_regs records with n_columns variables  */
/* each. Uses the rankswap method with parameter 'percent'                   */
/* ========================================================================= */
long CRankSwap::rankswap(double **data,long n_regs,long n_columns,long percent, long *prog)
{
  long col,i;
  double *vector;
  long *sort_info;

  if ((percent<0)||(percent>100)) return RNK_ERR_PER;
  if (n_regs<2) return RNK_ERR_RGS;
  if (n_columns<1) return RNK_ERR_COL;


  vector=(double *)malloc(n_regs*sizeof(double));
  sort_info=(long *)malloc(n_regs*sizeof(long));

  /*--- For each column ---*/
  for(col=0 ; col<n_columns ; col++)
  {
    /*--- Copy column to a temporary vector ---*/
    for(i=0;i<n_regs;i++)
    {     	  vector[i]=data[i][col];
    }

    /*--- Sort the vector ---*/
    for(i=0;i<n_regs;i++)
      sort_info[i]=i;
    quicksort_with_info(vector,0,n_regs-1,sort_info);

    /*--- Rankswap the sorted vector ---*/
    rankswap_vector(vector, n_columns, n_regs,percent, prog);

    /*--- Unsort the vector ---*/
    unsort_with_info(vector,n_regs,sort_info);

    /*--- After rankswapping vector is copied to data array ---*/
    for(i=0;i<n_regs;i++)
    {
      data[i][col]=vector[i];
    }
  }

  free(vector);
  free(sort_info);
  return RNK_OK;
}


/* ========================================================================= */
/* Some information is kept in 'vector' in order to allow the inverse        */
/* sorting procedure                                                         */
/* ========================================================================= */
void CRankSwap::quicksort_with_info(double *vector,long inf,long sup,long *sort_info)
{
  long k;
  if(inf <=sup)
  {
    partition_with_info(vector,inf+1,sup,vector[inf],&k,sort_info);

    swap_f(&(vector[inf]),&(vector[k]));
    swap_i(&(sort_info[inf]),&(sort_info[k]));

    quicksort_with_info(vector,inf,k-1,sort_info);
    quicksort_with_info(vector,k+1,sup,sort_info);
  }
}

/* ========================================================================= */
/*                                    Swapping                               */
/* ========================================================================= */
void CRankSwap::swap_f(double *a, double *b)
{
  double temp;
  temp=*a;
  *a=*b;
  *b=temp;
}

void CRankSwap::swap_i(long *a, long *b)
{
  long temp;
  temp=*a;
  *a=*b;
  *b=temp;
}


/* ========================================================================= */
/*                                    Partition                              */
/* ========================================================================= */
void CRankSwap::partition_with_info(double *vector,long inf,long sup, double x, long *k, long *sort_info)
{
  long k2;

  *k=inf-1;
  k2=sup+1;
  while(k2!=(*k+1))
  {
    if(vector[*k+1]<=x)
      (*k)++;
    else if( vector[k2-1]>=x)
      k2--;
    else
    {
      swap_f(&(vector[*k+1]),&(vector[k2-1]));
      swap_i(&(sort_info[*k+1]),&(sort_info[k2-1]));
      (*k)++;
      k2--;
    }
  }
}

/* ========================================================================= */
void CRankSwap::rankswap_vector(double vector[], long n_columns, long n_regs, long percent, long *prog)
{
  double *temp;
  long i;
  long *swap;

  swap=(long *)malloc(n_regs*sizeof(long));
  temp=(double *)malloc(n_regs*sizeof(double));

  for(i=0;i<n_regs;i++)
    temp[i]=vector[i];

  generate_swap(swap,n_columns, n_regs,percent, prog);

  for(i=0;i<n_regs;i++)
    vector[i]=temp[swap[i]];

  free(swap);
  free(temp);
}

/* ========================================================================= */
void CRankSwap::generate_swap(long swap[],long n_columns, long n_regs,long percent, long *prog) {
  long i, j, k;
  long max_dist;

  //srand(time(NULL));

  /*--- Initialize, (-1 = not modified) ---*/
  for (i = 0; i < n_regs; i++)
    swap[i] = -1;

  max_dist = (n_regs * percent) / 100;

  for (i = 0; i < n_regs; i++) {
    if (swap[i] == -1) {
      long randNumber = my_rand();
      k = 1 + ((long) ((double) max_dist * randNumber / (RAND_MAX + 1.0))) + i;
      j = long_min(k, n_regs - 1);
      while ((swap[j] >= 0) && ((i + 1) < j)) /* if record modified then look left */
  j--;
      if (swap[j] >= 0) /* if not found then look right */ {
        j = long_min(k, n_regs - 1);
        while ((swap[j] >= 0) && (j < (n_regs - 1))&& (j < (i + max_dist)))
          j++;
      }
      if (swap[j] == -1 && j < n_regs) /* if pair found then swap */ {
        swap[i] = j;
        swap[j] = i;
      } else {
        /* do not swap because number of record is not equal */
        swap[i] = i;
      }
    }
    *prog = *prog + 1;
    //	  printf ("\nProgress %.2f percent", ((*prog)*100.0)/(n_regs*n_columns));
  }
}

/* ========================================================================= */
void CRankSwap::unsort_with_info(double vector[], long n_regs, long sort_info[]) {
  double *temp;
  long i;

  temp = (double *) malloc(n_regs * sizeof (double));

  for (i = 0; i < n_regs; i++)
    temp[i] = vector[i];

  for (i = 0; i < n_regs; i++)
    vector[sort_info[i]] = temp[i];

  free(temp);
}

/* ========================================================================= */
/*                                   Min                                     */
/* ========================================================================= */
long CRankSwap::long_min(long a,long b)
{
  if(a<b)
    return(a);
  else
    return(b);
}

/* ========================================================================= */
/*                                   Max                                     */
/* ========================================================================= */
long CRankSwap::long_max(long a,long b)
{
  if(a>b)
    return(a);
  else
    return(b);
}

