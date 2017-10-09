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

#include "microAggregation.h"
#include <fstream>
#include <math.h>
#include <float.h>

#include <iostream>

long CMicroAggregation::Microaggregation(long  n_var, long n_elements,
		     long elms_p_group, long group_var, long *how_many,
		     long *var, double **out_data, long optim) {
  long max_group, utemp;
  long *beg_order=NULL, i=0, ii, j, k, h, r=0, s=0, first, remain, loop;
  double max_dist, distance, min_dist, dtemp, data_add, sqr_add;
  double *mean_var=NULL, **mean=NULL, *all_mean=NULL, *all_stdev=NULL;
  long *partition=NULL, beg_var, end_var;
  long *var_order=NULL, *used_vars;
  long res;

  // =========================================================================
  // Parameter control
  // =========================================================================
  if (n_elements<3)
    {
        return MIC_ERR_NEL;
    }
  if (n_var<1)
    {
      return MIC_ERR_NVAR;
    }

  if (elms_p_group<2)
    {
      return MIC_ERR_MEG;
    }

  if (group_var<1)
    {

      return MIC_ERR_GOV;
    }

  var_order = new long [n_var];

  if (!var_order)
    {
         return MIC_OUT_MEM;
    }


  // Read how many elements are in each group of variables

  if (group_var == 1) {
    how_many[0] = n_var;
    var[0] = var_order[0] = 1;
  }


  if (group_var  > 1) {
    long add_hm = 0;

    for (i=0 ; i<group_var ; i++)
      {


        if (how_many[i]<1)
          {
			return MIC_ERR_VPG;
          }
        add_hm+= how_many[i];
      }

    if (add_hm!=n_var)
      {
	       return MIC_ERR_VPG;
      }

    used_vars = new long[n_var];

    for (i=0 ; i<n_var ; i++)
      {
	used_vars[i] = var[i];


	for (j=0; j<i; j++)
	  if (used_vars[j] == var[i])
	    {
              return MIC_ERR_COL;
	    }

	if (var[i]> n_var)
	  {
            return MIC_ERR_COL;
	  }
      }


    for (i=0 ; i<n_var ; i++)
      var[i] = var[i]-1;

    for (i=0 ; i<n_var ; i++)
      var_order[var[i]] = i;
  }

  // =========================================================================
  // If different groups are defined data must be sorted in order to form
  // the groups
  // =========================================================================
  if (group_var>1) {
    for (j=0; j<n_var; j++)
      for (k=j+1; k<n_var; k++)
	if (var_order[j] > var_order[k])
	  {
	    for (i=0 ; i<n_elements ; i++)
	      {
		dtemp = out_data[i][j];
		out_data[i][j] = out_data[i][k];
		out_data[i][k] = dtemp;
	      }
	    utemp = var_order[j];
	    var_order[j] = var_order[k];
	    var_order[k] = utemp;
	  }
  }

  // =========================================================================
  // If only a variable is defined, the optimal microaggregation is performed
  // using the Hansen-Mukherjee polynomial exact method
  // =========================================================================
  if ((n_var==group_var)&&(optim)) {
	  res = Do_Opt_Mic ( n_elements, n_var, elms_p_group, out_data);
    if (!res) // An error has ocurred
      return res;
    }
    else /* Mateo-Domingo method */
    {
      // =========================================================================
      // Mean and standard deviation are calculated in order to standardize the
      // the data, so distance calculation between elements will be more accurate.
      // =========================================================================
      all_mean = new double [n_var];
      if (!all_mean)
	{
	     return MIC_OUT_MEM;
	}
      all_stdev = new double [n_var];
      if (!all_stdev)
	{
		return MIC_OUT_MEM;
	}

      for (j=0; j<n_var; j++)
	{
	  for (i=0, data_add=0.0, sqr_add=0.0; i<n_elements; i++)
	    {
	      data_add += out_data[i][j];
	      sqr_add += out_data[i][j]*out_data[i][j];
	    }
	  all_mean[j] = data_add/(double)n_elements;
	  all_stdev[j] = (sqr_add-n_elements*all_mean[j]*all_mean[j])/((double)n_elements-1);
	  all_stdev[j] = sqrt(all_stdev[j]);
	  if (all_stdev[j]==0.0)
	    {
			return MIC_ERR_STD;
	    }
	}

      // =========================================================================
      // Standardized data are calculated and kept.
      // =========================================================================
      for (j=0; j<n_var; j++)
	for (i=0; i<n_elements; i++)
	  out_data[i][j] = (out_data[i][j]-all_mean[j])/all_stdev[j];

      // =========================================================================
      // The initial sorting of the data is kept.
      // =========================================================================
      beg_order = new long [n_elements];
      if (!beg_order)
	{
	  	return MIC_OUT_MEM;
	}

      for (i=0 ; i<n_elements ; i++)
	beg_order[i] = i;

      // =====================================================================
      // Fixed size multivariate microaggregation is performed (data projection
      // according to distance between elements)
      // =====================================================================

      // The number of groups of a k-partition with k = elms_p_group
      max_group = n_elements/elms_p_group;
      partition = new long [max_group];

      if (!partition)
	{

	  return MIC_OUT_MEM;
	}

      // Initial partition is performed in a trivial way
      for (i=0 ; i<max_group-1 ; i++)
	      partition[i] = elms_p_group;

      partition[max_group-1] = n_elements-i*elms_p_group;

      // ===================================================================
      // The two most outstanding polongs are computed in the following way:
      // the 1st polong, so-called 'r',  is determined as the one farthest
      // from the average of the data set polongs and the 2nd polong, 's', is
      // determined as the one farthest from the first polong.
      // ====================================================================
     // *prog = 0; prlongf ("\n");
      for (ii=0, beg_var=0, end_var=0; ii<group_var; ii++)
	{
	  end_var += how_many[ii];
	  remain = n_elements;
	  loop = 0;
	  mean_var = new double[end_var-beg_var];

    while (remain >= 3*elms_p_group)
	    {
            //  *prog = (n_elements - remain)+ii*n_elements;
             // prlongf ("\rProgress: \t%.2f percent",((double)(*prog)*100.0)/(group_var*n_elements));
	      first = loop*elms_p_group;
	      for(k=beg_var; k<end_var; k++)
		      mean_var[k-beg_var]=0.0;

	      // The mean for each variable is calculated, using the
	      // elements not yet used
	      for(k=beg_var; k<end_var; k++)
	      {
		     for (j=first; j<n_elements; j++ )
			     mean_var[k-beg_var]+=out_data[j][k];
		     mean_var[k-beg_var]=mean_var[k-beg_var]/(n_elements-first);
	      }

	      // 'r' is calculated
	      for (j=first, max_dist=0.0; j<n_elements; j++)
		  {
		    for (k=beg_var, distance=0.0; k<end_var; k++)
		      distance += (out_data[j][k]-mean_var[k-beg_var])*(out_data[j][k]-mean_var[k-beg_var]);
		    if (distance > max_dist)
		      {
			max_dist = distance;
			r = j;
		      }
		  }

	      // 's' is calculated
	      for (j=first, max_dist=0.0; j<n_elements; j++)
		  {
		    for (k=beg_var, distance=0.0; k<end_var; k++)
		      distance += (out_data[j][k]-out_data[r][k])*(out_data[j][k]-out_data[r][k]);
		    if (distance > max_dist)
		      {
			max_dist = distance;
			s = j;
		      }
		  }
              // ============================================================
              // 'r' will be always less than 's'
	      // ============================================================
	      if (s < r)
		{
		  utemp = r;
		  r = s;
		  s = utemp;
		}

	      // ============================================================
	      // The element 'r' is the first element and it is placed
	      // in the first free position of the microaggregation array.
	      // ============================================================
	      if (r > first)
		{
		  for (j=beg_var; j<end_var; j++)
		    {
		      dtemp = out_data[r][j];
		      out_data[r][j] = out_data[first][j];
		      out_data[first][j] = dtemp;
		    }
		  utemp = beg_order[r];
		  beg_order[r] = beg_order[first];
		  beg_order[first] = utemp;
		}

	      // ========================================================
	      // Element 's' is placed in the first free position
	      // that will appear when 'r' group is completed.
	      // ========================================================
	      if (s != first+elms_p_group)
		{
		  for (j=beg_var; j<end_var; j++)
		    {
		      dtemp = out_data[s][j];
		      out_data[s][j] = out_data[first+elms_p_group][j];
		      out_data[first+elms_p_group][j] = dtemp;
		    }
		  utemp = beg_order[s];
		  beg_order[s] = beg_order[first+elms_p_group];
		  beg_order[first+elms_p_group] = utemp;
		}

	      // ===========================================================
	      // Look for the other elms_p_group-1 elements closer to
	      // element 'r' in order to form a group. Elements are sorted
	      // so they are stored together.
	      // ===========================================================
	      for (i=1; i<elms_p_group; i++)
		{
		  for (j=first+i, min_dist=max_dist; j<n_elements; j++)
		    {
		      for (k=beg_var, distance=0.0; k<end_var; k++)
			distance += (out_data[j][k]-out_data[first][k])*(out_data[j][k]-out_data[first][k]);
		      if (distance < min_dist)
			{
			  min_dist = distance;
			  r = j;
			}
		    }
		  if (r > first+i)
		    {
		      for (k=beg_var; k<end_var; k++)
			{
			  dtemp = out_data[r][k];
			  out_data[r][k] = out_data[first+i][k];
			  out_data[first+i][k] = dtemp;
			}
		      utemp = beg_order[r];
		      beg_order[r] = beg_order[first+i];
		      beg_order[first+i] = utemp;
		    }
		}

              first += elms_p_group;
	      // ===========================================================
	      // Look for the other elms_p_group-1 elements which are closer to
	      // element 'r' in order to form a group. Elements are sorted
	      // so they are stored together.
	      // ===========================================================
	      for (i=1 ; i<elms_p_group ; i++)
		{
		  for (j=first+i, min_dist=max_dist; j<n_elements; j++)
		    {
		      for (k=beg_var, distance=0.0; k<end_var; k++)
			distance += (out_data[j][k]-out_data[first][k])*(out_data[j][k]-out_data[first][k]);
		      if (distance < min_dist)
			{
			  min_dist = distance;
			  r = j;
			}
		    }
		  if (r > first+i)
		    {
		      for (k=beg_var; k<end_var; k++)
			{
			  dtemp = out_data[r][k];
			  out_data[r][k] = out_data[first+i][k];
			  out_data[first+i][k] = dtemp;
			}
		      utemp = beg_order[r];
		      beg_order[r] = beg_order[first+i];
		      beg_order[first+i] = utemp;
		    }
		}

	      loop += 2;
	      remain = remain-2*elms_p_group;
	    }

	  // =================================================================
	  // If there are less than 2*elms_p_group unclassified elements,
	  // they will form a group: the last one. If there are 2*elms_p_group
	  // or more elements, another iteration is performed. In that case,
	  // the two most outstanding elements are taken.
	  // One of its elements is chosen (call it 'r') and is grouped
	  // with the nearest 2*elms_p_group-1 elements to form a group.
	  // The remaining elements will form the last group.
	  // ===============================================================
	  if (remain >= 2*elms_p_group) {
	      first = loop*elms_p_group;

	      for(k=beg_var; k<end_var; k++)
		mean_var[k-beg_var]=0.0;

	      // The mean for each variable is calculated, using the
	      // elements not yet used
	      for(k=beg_var; k<end_var; k++)
	      {
		 for (j=first; j<n_elements; j++ )
		     mean_var[k-beg_var]+=out_data[j][k];
		 mean_var[k-beg_var]=mean_var[k-beg_var]/(n_elements-first);
	      }

	      // 'r' is calculated
	      for (j=first, max_dist=0.0; j<n_elements; j++)
		  {
		    for (k=beg_var, distance=0.0; k<end_var; k++)
		      distance += (out_data[j][k]-mean_var[k-beg_var])*(out_data[j][k]-mean_var[k-beg_var]);
		    if (distance > max_dist)
		      {
			max_dist = distance;
			r = j;
		      }
		  }

	      // 's' is calculated
	      for (j=first, max_dist=0.0; j<n_elements; j++)
		  {
		    for (k=beg_var, distance=0.0; k<end_var; k++)
		      distance += (out_data[j][k]-out_data[r][k])*(out_data[j][k]-out_data[r][k]);
		    if (distance > max_dist)
		      {
			max_dist = distance;
			s = j;
		      }
		  }

              // ============================================================
              // 'r' will be always less than 's'
	      // ============================================================
	      if (s < r)
		{
		  utemp = r;
		  r = s;
		  s = utemp;
		}

	      if (r > first)
		{
		  for (j=beg_var; j<end_var; j++)
		    {
		      dtemp = out_data[r][j];
		      out_data[r][j] = out_data[first][j];
		      out_data[first][j] = dtemp;
		    }
		  utemp = beg_order[r];
		  beg_order[r] = beg_order[first];
		  beg_order[first] = utemp;
		}

	      // Look for the nearest element to the last element taken,
	      // and sort the rest of elements following the same criterion
	      for (i=1; i<elms_p_group; i++)
		{
		  for (j=first+i, min_dist=max_dist; j<n_elements; j++)
		    {
		      for (k=beg_var, distance=0.0; k<end_var; k++)
			distance += (out_data[j][k]-out_data[first][k])*(out_data[j][k]-out_data[first][k]);
		      if (distance < min_dist)
			{
			  min_dist = distance;
			  r = j;
			}
		    }
		  if (r > first+i)
		    {
		      for (k=beg_var; k<end_var; k++)
			{
			  dtemp = out_data[r][k];
			  out_data[r][k] = out_data[first+i][k];
			  out_data[first+i][k] = dtemp;
			}
		      utemp = beg_order[r];
		      beg_order[r] = beg_order[first+i];
		      beg_order[first+i] = utemp;
		    }
		}
	 }

	 delete [] mean_var;

	  // The mean of each group is calculated and stored
	  mean = new double* [max_group];
	  if (!mean) {
      return MIC_OUT_MEM;
	  }
	  for (i=0; i<max_group; i++) {
	    mean[i] = new double [how_many[ii]];
	    if (!mean[i]) {
		    return MIC_OUT_MEM;
		  }
	  }
	  for (k=0; k<max_group; k++)
	    for (j=beg_var; j<end_var; j++) {
    		for (h=0, data_add=0.0, i=k*elms_p_group; h<partition[k]; h++,i++)
    		  data_add += out_data[i][j];
    		mean[k][j-beg_var] = data_add/(double)partition[k];
	     }
	    for (k=0, i=0; k<max_group; k++)
	      for (h=0; h<partition[k]; h++) {
		      for (j=beg_var; j<end_var; j++)
		        out_data[i][j] = mean[k][j-beg_var];
		      i++;
	       }

    	  for (i=0; i<max_group; i++)
          delete [] mean[i];
        delete [] mean;

	  // ==============================================================
	  // Data are 'destandardized' and stored
	  // ==============================================================
	  for (j=beg_var; j<end_var; j++)
	    for (i=0; i<n_elements; i++)
	      out_data[i][j] = out_data[i][j]*all_stdev[j]+all_mean[j];


	  // ===============================================================
	  // Modified data are re-sorted so as to restore the
	  // order of original data
	  // ===============================================================
	  for (i=0 ; i<n_elements ; i++)
	    for (k=i+1; k<n_elements; k++)
	      if (beg_order[i] > beg_order[k]) {
  		  for (j=beg_var; j<end_var; j++) {
  		      dtemp = out_data[i][j];
  		      out_data[i][j] = out_data[k][j];
  		      out_data[k][j] = dtemp;
  		    }
  		  utemp = beg_order[i];
  		  beg_order[i] = beg_order[k];
  		  beg_order[k] = utemp;
		  }
	  beg_var = end_var;
	}

      // ===================================================================
      // If groups were defined, variables are re-sorted so as to restore the
      // initial order of variables
      // ===================================================================


  if (group_var>1) {
	for (j=0; j<n_var; j++)
	  for (k=j+1; k<n_var; k++)
	    if (var[j] > var[k])
	      {
		for (i=0; i<n_elements; i++)
		  {
		    dtemp = out_data[i][j];
		    out_data[i][j] = out_data[i][k];
		    out_data[i][k] = dtemp;
		  }
		utemp = var[j];
		var[j] = var[k];
		var[k] = utemp;
	      }
      }
      delete [] partition;
    }

  delete [] beg_order;
  delete [] all_stdev;
  delete [] all_mean;
  delete [] var_order;
  /* needs to be commented out, otherwise segfault
   * perhaps problem with var() in R?
  */
  //delete [] var;
  return MIC_OK;  // Everything is OK!
}




// =========================================================================
//      Takes the data in 'data' and performs an optimal microaggregation
// =========================================================================
//long Do_Opt_Mic ( long n_el, long n_var, long k, double **out_data, long *prog ) {
long CMicroAggregation::Do_Opt_Mic ( long n_el, long n_var, long k, double **out_data) {
  double *values;
  long *beg_order;
  t_graph g;
  long i, v;
  long res;

  values = new double[n_el];
  if (!values)
    {
            return MIC_OUT_MEM;
    }

  beg_order = new long[n_el];

  if (!beg_order)
    {
           return MIC_OUT_MEM;
    }

  // Performing optimal microaggregation for each variable
  for (v=0; v<n_var; v++) {
  // Keep the original sorting of data
    for ( i=0; i<n_el; i++)
      {
      	beg_order[i] = i;
      	values[i] =out_data[i][v];
      }

     // Construct the graph
     res = Graph ( k, n_el, values, beg_order, &g);
     if (!res) // An error has ocurred
       return res;

     // Make the optimal microaggregation and store
     // the results in the output file
     //res = Opt_Mic ( g, k, beg_order, values, out_data, v, n_var, prog);
	 res = Opt_Mic ( g, k, beg_order, values, out_data, v, n_var);
     delete [] g.nodes;

     if (!res) // An error has ocurred
       return res;
  }
  delete [] values;
  delete [] beg_order;
  return 1;
}

// =========================================================================
//                                Graph
// =========================================================================
long CMicroAggregation::Graph ( long k, long n, double *v, long *bo, t_graph *g) {
  long i, j;
  double sse;

  // Graph creation
  g->n_nodes = n+1; // A node '0' is added

  g->nodes = new t_node[n+1];
  if (!g->nodes)
    {

      return MIC_OUT_MEM;
    }

  for ( i=0; i<n+1; i++ )
    {
      g->nodes[i].costs = new double[n+1];
      if (!g->nodes[i].costs)
	{

	  return MIC_OUT_MEM;
	}
      for ( j=0; j< n+1; j++ )
        g->nodes[i].costs[j]=DBL_MAX;
      g->nodes[i].link = -1;
      g->nodes[i].cost = DBL_MAX;
    }

  // Sort the values in the vector
  Quick_Sort(v, bo, 0, n-1);

  // Fill the costs
  for (i=0; i<= n-k; i++)
    {
      if (i==1) i = k;
      for ( j=i+k; (j <= n) && (j < i+2*k); j++ )
	{
	  sse = Sum_Quad_Err ( v, i, j-i);
	  /* Set the costs */
	  g->nodes[i].costs[j-(i+k)] = sse;
	}
    }
  return 1;
}

// =========================================================================
//                         Optimal Microaggregation
// =========================================================================
/*long Opt_Mic ( t_graph g, long k, long *bo, double *v, double **out_data,
		long var, long vars, long *prog) {
 */
long CMicroAggregation::Opt_Mic ( t_graph g, long k, long *bo, double *v, double **out_data,
		long var, long vars) {
  long cp, l, i, j, n = g.n_nodes-1;
  double cost = 0.0;
  double *means, mean;

  // Find the shortest path
  g.nodes[0].cost = 0.0;
  g.nodes[0].link = 0;
//  *prog=n*(n-k)*var;

  for ( i=0; i<= n-k; i++ )
    {
      if (i == 1) i = k;
      for ( j=i+k, cp = 0; cp != n; ++j, ++cp )
	{  cost = g.nodes[i].cost + g.nodes[i].costs[cp];
//	*prog = *prog + 1;
//	prlongf ("\rProgress %.2f percent", ((double)(*prog)*100.0)/(n*(n-k)*vars));
	if ( cost < g.nodes[j].cost )
	  {
	    g.nodes[j].cost = cost;
	    g.nodes[j].link = i;
	  }
	}
    }

  for ( i=0; i<n+1; i++ )
	  delete [] g.nodes[i].costs;

  // Means are used to show if an element belongs to a class
  means = new double[n];

  if (!means)
    {

      return MIC_OUT_MEM;
    }

  // Make the classes
  for ( j=n; j!=0; j=i )
    {
      i = g.nodes[j].link;

      // The mean for that class
      mean = Mean ( v, i, j-i );
      for ( l=i+1; l<=j; l++)
        means[bo[l-1]]=mean;
    }

  // Resulting microaggregated data are written in the column 'var'
  for (i=0; i<n; i++)
    out_data[i][var]= means[i];

  delete [] means;
  return 1;
}


// =========================================================================
//                                Quick sort
// =========================================================================
void CMicroAggregation::swap(double *a, double *b)
{
  float temp;
  temp=*a;
  *a=*b;
  *b=temp;
}

void CMicroAggregation::swap_i(long *a, long *b)
{
  long temp;
  temp=*a;
  *a=*b;
  *b=temp;
}

void CMicroAggregation::partition(double *vector, long *bo, long inf, long sup, float x, long *k)
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
	  swap(&(vector[*k+1]),&(vector[k2-1]));
	  swap_i(&(bo[*k+1]),&(bo[k2-1]));
	  (*k)++;
	  k2--;
	}
    }
}

void CMicroAggregation::Quick_Sort(double *vector, long *bo, long inf, long sup)
{
  long k;
  if(inf <=sup)
    {
      partition(vector,bo,inf+1,sup,vector[inf],&k);

      swap(&(vector[inf]),&(vector[k]));
      swap_i(&(bo[inf]),&(bo[k]));
      Quick_Sort(vector,bo,inf,k-1);
      Quick_Sort(vector,bo,k+1,sup);
    }
}

// =========================================================================
//                             Some statistics
// =========================================================================
double CMicroAggregation::Mean ( double *fp, long y, long n) {

  double sum = 0.0;
  long i;
  for (i=y; i<n+y; i++)
    sum += fp[i];
  return (sum/(1.0*n));
}

double CMicroAggregation::Sum_Quad_Err ( double *fp, long y, long n) {

  long i;
  double sum = 0.0;
  double m = Mean ( fp, y, n);
  for (i=y; i<n+y; ++i)
    sum += (fp[i]-m)*(fp[i]-m);
  return (sum);
}





