
/**
 * Stata plugin for computing risk of data disclosure
 *
 * @author: Pascal heus (pascal.heus@gmail.com)
 *
 * adapted for R by Bernd Prantner and Alexander Kowarik
 * Developed with the financial and technical support of the
 * International Household Survey Network
 * http://www.surveynetwork.org
 *
 * Copyright 2006, 2007 Pascal Heus (pascal.heus@gmail.com),
 * Organisation For Economic Co-Operation And Development
 *
 *  This program is free software; you can redistribute it and/or modify it under the terms of the
 *  GNU Lesser General Public License as published by the Free Software Foundation; either version
 *  2.1 of the License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 *  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *  See the GNU Lesser General Public License for more details.
 *
 *  The full text of the license is available at http://www.gnu.org/copyleft/lesser.html
 */

/*
 * HOW IT WORKS:
 * This plugin takes the identification variables and optionaly the weight variable as data input.
 * The output is a level of risk stored in the specifed risk variable.
 *
 * For weighted data, the risk is computed based on the micro-argus methodology
 * For unweighted data, the risk is the inverse of the frequency
 *
 * Note that
 * 1) *** THE DATASET MUST BE SORTED BY THE KEY IDENTIFIERS PRIOR TO THE CALL ***
 * 2) The order of the variables provide as input parameters is crucial.
 *    the routine assume that the last two variables are respectively the
 *    weight and the output risk.
 * 3) All the variables must be numeric
 *
 */
/*
 * Version of measure_risk optimized for fast frequencies Calculations
 * adapted by Alexander Kowarik
 */


/*======*/
/* MAIN */
/*======*/
// int argc, char *argv[]
RcppExport SEXP ffc(SEXP data, SEXP weighted_R, SEXP n_key_vars_R, SEXP missing_value_R)
{
  int i;


  Rcpp::NumericMatrix MatX(data);

  Rcpp::NumericMatrix Res(MatX.rows(), 2);
  int NbRow = MatX.rows();
  int NbCol = MatX.cols();

  g_Config.Nb_QuasiId_Var = Rcpp::as<int>(n_key_vars_R);
  g_Config.Nb_Sensitive_Var = 0;
  g_Config.weight_var_pos = 0;
  g_Config.risk_var_pos = 0;
  g_Config.missing_value = Rcpp::as<double>(missing_value_R);

  Rcpp::NumericVector indMiss;

  double weightSum = Rcpp::sum(MatX(Rcpp::_,MatX.cols()-1));


//  for (i = 0; i < MAX_SENSITIVE_VAR; i++)
//    init_var(&g_Config.Sensitive_Var[i]);

  // g_Config.Ldiversity_Recursivity_Constant = 1.0f;

  bool weighted = Rcpp::as<bool>(weighted_R);
  g_Config.is_weighted = (weighted) ? 1 : 0;

//  g_Config.Ldiversity_Recursivity_Constant = 2;
//  double *weightvec=new double[MatX.rows()-1];
//  if(weighted){
//    ForLoop(i,MatX.rows()){
//      weightvec[i]=MatX(i,MatX.cols()-1);
//    }
//  }

  // setup g_Config
  g_Config.weight_var_pos = NbCol-1;

  //display_config();
  /*=========*/
  /* PROCESS */
  /*=========*/
  //if (g_pDataset->GetNbVar() < 2)


    double *group_key = new double[g_Config.Nb_QuasiId_Var];  //< the current group identification key
    double *obs_key = new double[g_Config.Nb_QuasiId_Var];
	double *miss_key = new double[g_Config.Nb_QuasiId_Var];

    long current_obs;
    long obs_count = 0;         // total number of observations
    double obs_weight;        //< weight of the current observation
//    double obs_value;         //< generic value
    long group_count = 0;       //< total number of groups
    long group_missing;         //< used to detect missing values in group key
    int group_size, group_size2;
    double group_weight;        //< the sum of the weights for this group
    //double group_risk;


    // get first observation
    current_obs = 0;//SF_GetRowStart();
    do
    {
      // Init group
      group_count++;

      group_size = 0;
      group_weight = 0.0;
      group_missing = 0;


      // read the group key
      for (i = 0; i < g_Config.Nb_QuasiId_Var; i++)
      {
        group_key[i] = MatX(current_obs, i);
        //g_pDataset->GetValue(i, current_obs, &group_key[i]);
        if (SF_IsMissing(group_key[i])) {
          group_missing++;
        }
      }
      if (group_missing == g_Config.Nb_QuasiId_Var)
      {
      	indMiss.push_back(current_obs);
        //
        // CASE 1: ALL MISSING VALUES IN KEY (risk is zero)
        //
        //group_risk = 0;

        // UPDATE STATA
        do
        {
        	Res(current_obs, 0) = NbRow;
			Res(current_obs, 1) = weightSum;
          obs_count++;
          current_obs++;

          if (current_obs >= NbRow)
            break;

          // read next obs key
          for (i = 0; i < g_Config.Nb_QuasiId_Var; i++)
            obs_key[i] = MatX(current_obs, i);
            //g_pDataset->GetValue(i, current_obs, &obs_key[i]);
        }
        while (is_same_key_Risk1(group_key, obs_key, g_Config.Nb_QuasiId_Var));
      }
      else if (group_missing > 0)
      {
      	indMiss.push_back(current_obs);
        //
        // CASE 2: SOME MISSING VALUES IN KEY
        //

        // The weights and count of the observations with the same
        // non-missing key values need to be used for this group
        // This requires a full scan of the dataset
        int i, j;           // local definitions to prevent conflicts
        double value;

//        for (i = SF_GetRowStart(); i <= SF_GetRowEnd(); i++)
        ForLoop (i, NbRow)
        {
          // if (g_pDataset->IsRowSelected(i)) - Always TRUE
          // {
            // compare partial keys
            for (j = 0; j < g_Config.Nb_QuasiId_Var; j++)
            {
            	value = MatX(i, j);
            	//printf("i|j|groupKey|value:%d|%d|%f|%f ... ", i,j,group_key[j], value);
              // if this variable is a missing component of the current key, ignore it
              if (SF_IsMissing(group_key[j])) {
                continue;
               }

              // read this variable value
              //value = MatX(i, j);
              // g_pDataset->GetValue(j, i, &value);

              if (SF_IsMissing(value)) {
                continue;
               }

              // if not equal to the current key, this is not a match
              if (value != group_key[j]) {
                break;
               }
            }

            if (j == g_Config.Nb_QuasiId_Var)
            {
              // we didn't hit the break, this is a match
              group_size++;
              if (g_Config.is_weighted)
              {
                //value = weightvec[i];//MatX(i, g_Config.weight_var_pos);
                value = MatX(i, g_Config.weight_var_pos);
                //g_pDataset->GetValue(g_Config.weight_var_pos, i, &value);
                group_weight += value;
              }

            }
          // }
        }

        // compute risk
        do
        {
          // add this observation contribution to the global risk
          if (g_Config.is_weighted){
            Res(current_obs, 0) = group_size;
            Res(current_obs, 1) = group_weight;
          }else{
            Res(current_obs, 0) = group_size;
            Res(current_obs, 1) = group_size;
          }
          // if unweighted data and sensitive variables exist

          obs_count++;
          current_obs++;

          if (current_obs >= NbRow)
            break;

          // read next obs key
          for (i = 0; i < g_Config.Nb_QuasiId_Var; i++)
            obs_key[i] = MatX(current_obs, i);
            //g_pDataset->GetValue(i, current_obs, &obs_key[i]);

        }
        while (is_same_key_Risk1(group_key, obs_key, g_Config.Nb_QuasiId_Var));
      }
      else
      {
        //
        // CASE 3: FULL KEY (NORMAL CASE)
        //
        // read all observations for this group
        do
        {
          obs_count++;
          if (g_Config.is_weighted)
          {
            // add to group weight
            //obs_weight = weightvec[current_obs];//MatX(current_obs, g_Config.weight_var_pos);
            obs_weight = MatX(current_obs, g_Config.weight_var_pos);
            group_weight += obs_weight;
          }

          // add to group frequency
          group_size++;

          // next
          current_obs++;

          if (current_obs >= NbRow)
            break;

          // read next obs key
          for (i = 0; i < g_Config.Nb_QuasiId_Var; i++)
            obs_key[i] = MatX(current_obs, i);

        }
        while (is_same_key_Risk1(group_key, obs_key, g_Config.Nb_QuasiId_Var));
        
        group_size2 = group_size;
        for ( int k=0; k < indMiss.size(); k++) {
        
        	for (i = 0; i < g_Config.Nb_QuasiId_Var; i++)
            	miss_key[i] = MatX(indMiss[k], i);
        
        	if ( is_same_key_Risk2(miss_key, group_key, g_Config.Nb_QuasiId_Var) ) {
        		group_size2++;
            	group_weight += MatX(indMiss[k], g_Config.weight_var_pos);       		
        	}	
        }
        
        for (i = current_obs - group_size; i < current_obs; i++)
        {
          if (g_Config.is_weighted){
            Res(i, 0) = group_size2;
            Res(i, 1) = group_weight;
          }else{
            Res(i, 0) = group_size2;
            Res(i, 1) = group_size2;
          }
        }

      }
    }
    while (current_obs < NbRow);

    delete[] group_key;
    delete[] obs_key;
    return Rcpp::List::create(
      Rcpp::Named( "Res") = Res
    );
  return 0;
}
