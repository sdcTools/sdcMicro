#if !defined RankSwap_h
#define RankSwap_h


#define RNK_OK	        1       // Rankswap ended OK
#define RNK_ERR_PER 	0         // Percent value must be in [0, 100]
#define RNK_ERR_RGS     -1      // Number of registers wrongly defined
#define RNK_ERR_COL     -2      // Number of columns wrongly defined

class CRankSwap  {
  public:
  long rankswap(double **data,long n_regs,long n_columns,long percent, long *prog);
  private:
  void quicksort_with_info(double *vector,long inf,long sup,long *sort_info);
  void swap_f(double *a, double *b);
  void swap_i(long *a, long *b);
  void partition_with_info(double *vector,long inf,long sup, double x, long *k, long *sort_info);
  void rankswap_vector(double vector[],long n_columns, long n_regs,long percent, long *prog);
  void generate_swap(long swap[],long n_columns, long n_regs,long percent, long *prog );
  void unsort_with_info(double vector[],long n_regs,long sort_info[]);
  long long_min(long a,long b);
  long long_max(long a,long b);
};

#endif
