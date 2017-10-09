/*
* Argus Open Source
* Software to apply Statistical Disclosure Control techniques
*
* Copyright 2014 Statistics Netherlands
*
* This program is free software; you can redistribute it and/or
* modify it under the terms of the European Union Public Licence
* (EUPL) version 1.1, as published by the European Commission.
*
* You can find the text of the EUPL v1.1 on
* https://joinup.ec.europa.eu/software/page/eupl/licence-eupl
*
* This software is distributed on an "AS IS" basis without
* warranties or conditions of any kind, either express or implied.
*/


// =========================================================================
// -------------------------------------------------------------------------
//                       MULTIVARIATE MICROAGGREGATION
//                                      Author: Josep M. Mateo Sanz
//                                      	Josep Domingo-Ferrer
//                                      	Antoni Martinez-Balleste
//                                      	Francesc Sebe
//                                      	Angel Torres
//                                      	Narcis Macia
//                                              Universitat Rovira i Virgili,
//                                              2002
// -------------------------------------------------------------------------
#if !defined MicroAggregation_h
#define MicroAggregation_h


// =========================================================================

#include <stdio.h>
#include <stdlib.h>
//#include "CatalaanCtrl.h"

// =========================================================================
//                     The MICROAGGREGATION ROUTINE
// =========================================================================
// See 'readme.txt' for more information

// RETURN VALUES
// Returns the integer MIC_OK whenever everything is OK. If some kind of
// error has ocurred (i.e. a parameter definition mistake), the
// microaggregation procedure returns one of the following values:

#define MIC_OK        1      // Everything is OK
#define MIC_OUT_MEM   0      // The routine has run out of memory
#define MIC_ERR_NEL   -1     // Number of records wrongly defined
#define MIC_ERR_NVAR  -2     // Number of variables wrongly defined
#define MIC_ERR_MEG   -3     // Minimum number of records in a group wrongly defined
#define MIC_ERR_GOV   -4     // Number of groups of variables wrongly defined
#define MIC_ERR_VPG   -5     // Grouping of variables is wrongly defined
#define MIC_ERR_COL   -6     // Bad definition of column sorting
#define MIC_ERR_STD   -7     // There was an error while calculating STDEV

// Anyway, when an error has ocurred, the error message is also printed.
// =========================================================================
class CMicroAggregation
{
public:


/*int Microaggregation(long  n_var, long n_elements,
					 long elms_p_group, long group_var,
					 long *how_many,long *var,
					 double **out_data, int optim,
					 long *prog);
*/

long Microaggregation(long  n_var, long n_elements,
					 long elms_p_group, long group_var,
					 long *how_many,long *var,
					 double **out_data, long optim);

private:
// =========================================================================
//               Procedures used for the optimal microaggregation
// =========================================================================
typedef struct
{
  double   *costs;
  double   cost;
  long      link;
} t_node;

typedef struct
{
  long      n_nodes;
  t_node   *nodes;
} t_graph;

long Do_Opt_Mic ( long n_el, long n_var, long k, double **out_data);

    void Quick_Sort(double *vector, long *bo, long inf, long sup);
//long Opt_Mic ( t_graph g, long k, long *bo, double *v, double **out_data, long var, long vars, long *prog);
    long Opt_Mic ( t_graph g, long k, long *bo, double *v, double **out_data, long var, long vars);
    long Graph ( long k, long n, double *v, long *bo, t_graph *g);
    double Sum_Quad_Err ( double *fp, long y, long n);
    double Mean ( double *fp, long y, long n);
    void swap(double *a, double *b);
    void swap_i(long *a, long *b);
    void partition(double *vector, long *bo, long inf, long sup, float x, long *k);
};
#endif

