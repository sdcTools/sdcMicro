
/**
 * Stata plugin for computing hierarchical (household) level risk of data disclosure
 *
 * @author: Pascal heus (pascal.heus@gmail.com)
 *
 * adapted for R by Bernd Prantner and Alexander Kowarik
 * Developed with the financial and technical support of the
 * International Household Survey Network
 * http://www.surveynetwork.org
 *
 * Copyright 2006, 2007 Pascal Heus (pascal.heus@gmail.com)
 *
 * 	This program is free software; you can redistribute it and/or modify it under the terms of the
 * 	GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 	2.1 of the License, or (at your option) any later version.
 *
 * 	This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * 	without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 	See the GNU Lesser General Public License for more details.
 *
 *  The full text of the license is available at http://www.gnu.org/copyleft/lesser.html
 */

/*
 * HOW IT WORKS:
 * This plugin takes the hierarchical (household) identification variables and
 * the observation (individual) risk as input paramneters.   *
 * It outputs the hierarchical risk based usings Boole's formula a described in
 * micro-argus manual.
 *
 * Note that
 * 1) *** THE DATASET MUST BE SORTED BY THE KEY IDENTIFIERS PRIOR TO THE CALL ***
 * 2) *** THE OBSERVATION (INDIVIDUAL) RISK MUST BE COMPUPTED BEFORE ***
 * 3) The order of the variables provide as input parameters is crucial.
 *    the routine assume that the last two variables are respectively the
 *    observation (individual) level risk and the output computed
 *    hierarchical (household) risk.
 * 4) All the variables must be numeric
 *
 */

double boole_combine(double value, int index, int level, double list[], int list_size)
{
  double sum = 0;
  double combination;
  int i;

  for (i = index; i < list_size; i++)
  {
    combination = value * list[i];  // unsigned

    // check if more combination are possible (higher indexes exists in the list)
    if (i < list_size - 1)
    {
      // call this routine recursively
      sum += boole_combine(combination, i + 1, level + 1, list, list_size);
    }

    // add this combination to the sum (signed)
    sum += pow(-1.0f, level + 1.0f) * combination;
  }

  return sum;
}

/**
 * Boole formula
 *
 * @param list[] array of values to combine
 * @param list_size number of elements in the array
 */
double boole(double list[], int list_size)
{
  double sum = 0;
  int i;

  if (list_size == 1)
  {
    // soltuion is trivial
    sum = list[0];
  }
  else
  {
    // loop over each case
    for (i = 0; i < list_size; i++)
    {
      sum += list[i];         // add this value to the sum
      sum += boole_combine(list[i], i + 1, 2, list, list_size); // combine this value with all the other ones
    }
  }

  return sum;
}

char buf_hierachical[1024];
// character buf_hierachicalfer to display messages
int is_same_key(double key1[], double key2[], int key_size)
{
  int i;
  int rc = 1;
  for (i = 0; i < key_size; i++)
  {
    if (key1[i] != key2[i])
    {
      rc = 0;
      break;
    }
  }

  return rc;
}

/*=====================*/
/* Main Stata function */
/*=====================*/
RcppExport SEXP measure_hierachical(SEXP data)
{
  BEGIN_RCPP

  Rcpp::NumericMatrix Mat(data);

  int NbRow = Mat.rows();
//  int NbCol = Mat.cols();

  Rcpp::NumericVector Res(Mat.rows());
  //	int NbRow = g_pDataset->GetNbRow();

  // display number of observations
  int n_key_vars = 1;//NbCol - 2;
  double *group_key = new double[n_key_vars];
  double *obs_key = new double[n_key_vars];
  long current_obs;

  double group_risk;

  double hier_risk_ER = 0.0; //< The expected number of re-identification
  double hier_risk = 0.0;		//< The re-identification rate or global risk
  long group_count = 1;
  group_count = group_count - 1; // to avoid notes in CRAN checks; 
  long obs_count = 0;
  double obs_risk[256];		// supports up to 256 observations per key
  int group_size;
  int i;
  // get first observation
  current_obs = 0;//SF_GetRowStart();
  do
  {
    // read household id
    group_count++;
    // set group  key
    for (i = 0; i < n_key_vars; i++)
    group_key[i] = Mat(current_obs, i);
    //g_pDataset->GetValue(i, current_obs, &group_key[i]);
    group_size = 0;

    // read all observations for this group
    do
    {
      obs_count++;

      // read obs risk
      obs_risk[group_size] = Mat(current_obs, n_key_vars);
      //g_pDataset->GetValue(n_key_vars, current_obs, &obs_risk[group_size]);

      // next
      group_size++;
      current_obs++;

      if (current_obs >= NbRow)
        break;

      // read next group key
      for (i = 0; i < n_key_vars; i++)
      obs_key[i] = Mat(current_obs, i);
      //g_pDataset->GetValue(i, current_obs, &obs_key[i]);

    }
    while (is_same_key(group_key, obs_key, n_key_vars));
    // compute risk for this household
    group_risk = boole(obs_risk, group_size);
    // UPDATE STATA
    for (i = current_obs - group_size; i < current_obs; i++)
    {
      // Write value back to stata file for all observations in the household
      Res(i) = group_risk;
      //g_pDataset->SetValue(n_key_vars + 1, i, group_risk);

      // add to ER
      hier_risk_ER += group_risk;
    }
  }
  while (current_obs < NbRow);
  // compute hierarchical risk and store in stata scalars
  hier_risk = hier_risk_ER / obs_count;


  CleanDeleteT(group_key);
  CleanDeleteT(obs_key);
  return Rcpp::List::create(
      Rcpp::Named( "Res" ) = Res,
      Rcpp::Named( "hier_risk_ER" ) = hier_risk_ER,
      Rcpp::Named( "hier_risk" ) = hier_risk,
      Rcpp::Named( "hier_risk_pct" ) = hier_risk * 100
  );


  END_RCPP
}
