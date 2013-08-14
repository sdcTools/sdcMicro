#include <stdio.h>
#include <stdlib.h>
#include <math.h>
void standardise(double *vec, int *lenVec, double *mean, double *sd) {
    int i;
    for (i=0; i < lenVec[0]; i++) {
        vec[i] = (vec[i] - mean[0]) / sd[0];
    }
}

void restandardise(double *vec, int *lenVec, double *mean, double *sd) {
    int i;
    for (i=0; i < lenVec[0]; i++) {
        vec[i] = (vec[i] * sd[0]) + mean[0] ;
    }
}