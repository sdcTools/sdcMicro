#include <R.h>
#include <math.h>

void f2(int *dim, int *mat, int *fk, double *Fk, double *w) {
	int N = dim[0];
	int S = dim[1];
	int i,a,j;
	int sum=0;
 
	for (i=0; i < N; i++) {				
		Fk[i] = w[i];
	}

	for (i=0; i < N; i++) {	
		for (j=(i+1); j < N; j++) {
			sum = 0;
			for (a = 0; a < S; a++) {
				if (mat[i*S+a] != mat[j*S+a]) {
					if (mat[i*S+a] != -999999 && mat[j*S+a] != -999999 ) {
						sum ++;
					}
				}
			}

			if (sum==0) { 
				fk[i] = fk[i] + 1;
	      			fk[j] = fk[j] + 1;
	      			Fk[i] = Fk[i] + w[j];
        			Fk[j] = Fk[j] + w[i];
			}			
		}
	}


	i = 0;
	for (i=0; i<N; i++)
	{
		fk[i] += 1 ;
		if (Fk[i] == 0.0) {
			Fk[i] = w[i];
		}
	}			
}


