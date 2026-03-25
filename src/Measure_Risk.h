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

#include <Rcpp.h>
#include <math.h>

/* Helper for NA-aware key comparison to match R's order(..., na.last=TRUE!) */
bool items_match_safe(double a, double b) {
  if (Rcpp::NumericVector::is_na(a) && Rcpp::NumericVector::is_na(b)) return true;
  if (Rcpp::NumericVector::is_na(a) || Rcpp::NumericVector::is_na(b)) return false;
  return (std::abs(a - b) < 1e-9);
}

bool is_same_key_Safe(double *key1, double *key2, int n) {
  for (int i = 0; i < n; i++) {
    if (!items_match_safe(key1[i], key2[i])) {
      return false;
    }
  }
  return true;
}

/*=============*/
/* L-DIVERSITY */
/*=============*/
/**
 * Computes the l-diversity values for the current group
 */
inline void compute_group_ldiversity(int group_size, SVariable *pSensitive_Var, int NbVar = MAX_SENSITIVE_VAR) {
  int i, j, k;
  SCategory *pCat;
  int count;
  double entropy_ratio;
  double entropy;

  for (i = 0; i < MAX_SENSITIVE_VAR; i++) {
    SVariable &Var = pSensitive_Var[i];
    if (!Var.Require_Ldiversity) continue;

    count = 0;
    entropy = 0.0;
    pCat = Var.pFirstCategory;

    while (pCat != NULL) {
      if (pCat->group_freq > 0) {
        count++;
        entropy_ratio = (double)pCat->group_freq / (double)group_size;
        entropy += entropy_ratio * log(entropy_ratio);
      }
      pCat = pCat->pNext;
    }

    if (Var.Nb_Missing_Value_In_Group > 0 && count > 0) {
      count++;
      entropy_ratio = (double)Var.Nb_Missing_Value_In_Group / (double)group_size;
      entropy += entropy_ratio * log(entropy_ratio);
    }

    Var.Group_Distinct_Ldiversity = (double)count;
    Var.Group_Entropy_Ldiversity = exp(-entropy);

    int NbUsedCategory = 0;
    pCat = Var.pFirstCategory;
    while (pCat) {
      if (pCat->group_freq > 0) NbUsedCategory++;
      pCat = pCat->pNext;
    }

    if (NbUsedCategory == 0) continue;

    SCategory **ppUsedCat = (SCategory **)malloc(sizeof(SCategory *) * NbUsedCategory);
    pCat = Var.pFirstCategory;
    j = 0;
    while (pCat) {
      if (pCat->group_freq > 0) {
        ppUsedCat[j++] = pCat;
      }
      pCat = pCat->pNext;
    }

    for (j = 0; j < NbUsedCategory; ++j) {
      for (k = j + 1; k < NbUsedCategory; ++k) {
        if (ppUsedCat[j]->group_freq < ppUsedCat[k]->group_freq) {
          Swap(ppUsedCat[k], ppUsedCat[j]);
        }
      }
    }

    if (NbUsedCategory > 1) {
      float c = g_Config.Ldiversity_Recursivity_Constant;
      float Sum = 0.0f;
      for (j = 1; j < NbUsedCategory; ++j) {
        Sum += (float)ppUsedCat[j]->group_freq;
      }

      Sum *= c;
      int rank = 1;
      for (j = 1; j < NbUsedCategory && Sum > (float)ppUsedCat[0]->group_freq; ++j) {
        Sum -= c * (float)ppUsedCat[j]->group_freq;
        rank++;
      }
      Var.Group_Recursive_Ldiversity = rank;
    } else {
      Var.Group_Recursive_Ldiversity = 1;
    }
    free(ppUsedCat);
  }
}

/*===================*/
/* Multi L-DIVERSITY */
/*===================*/
/**
 * Computes the multi l-diversity values for the current group
 */
void Compute_Multi_LDiversity(int Obs, int GroupSize, Rcpp::NumericMatrix Mat, Rcpp::NumericVector indexSensVar) {
  int i, j, k, l;
  const int NbVar = g_Config.Nb_Sensitive_Var;
  double *pSet = new double[GroupSize * NbVar];
  int *pSetIndex = new int[GroupSize];
  BOOL First = TRUE;

  for (i = 0; i < GroupSize; i++) {
    for (j = 0; j < NbVar; j++) {
      pSet[i * NbVar + j] = Mat(Obs + i, (int)indexSensVar(j) - 1);
    }
    pSetIndex[i] = i;
  }

  for (i = 0; i < NbVar; i++) {
    SVariable &VarI = g_Config.Sensitive_Var[i];
    if (!VarI.Require_Ldiversity) continue;

    for (j = 0; j < GroupSize - 1; j++) {
      for (k = j + 1; k < GroupSize; k++) {
        BOOL DoSwap = FALSE;
        for (l = 0; l < NbVar; l++) {
          if (l == i) continue;
          if (pSet[pSetIndex[k] * NbVar + l] > pSet[pSetIndex[j] * NbVar + l]) {
            DoSwap = TRUE;
            break;
          }
          if (pSet[pSetIndex[k] * NbVar + l] < pSet[pSetIndex[j] * NbVar + l]) break;
        }
        if (DoSwap) {
          Swap(pSetIndex[j], pSetIndex[k]);
        }
      }
    }

    int i1 = 0;
    SVariable Var;
    init_var(&Var);

    std::vector<float> subEntropies;
    std::vector<int> subRecursives;

    for (j = 1; j <= GroupSize; j++) {
      add_var_cat_value(&Var, pSet[pSetIndex[j - 1] * NbVar + i]);

      bool endOfSub = (j == GroupSize);
      if (!endOfSub) {
        for (l = 0; l < NbVar; l++) {
          if (l == i) continue;
          if (pSet[pSetIndex[j] * NbVar + l] != pSet[pSetIndex[i1] * NbVar + l]) {
            endOfSub = true;
            break;
          }
        }
      }

      if (endOfSub) {
        compute_group_ldiversity(j - i1, &Var, 1);
        subEntropies.push_back((float)Var.Group_Entropy_Ldiversity);
        subRecursives.push_back((int)Var.Group_Recursive_Ldiversity);
        i1 = j;
        free_var(&Var);
        init_var(&Var);
      }
    }

    float minEnt = subEntropies[0];
    int minRec = subRecursives[0];
    for (size_t s = 1; s < subEntropies.size(); s++) {
      minEnt = Min(minEnt, subEntropies[s]);
      minRec = Min(minRec, subRecursives[s]);
    }

    if (First) {
      g_Config.Group_MultiEntropy_Ldiversity = minEnt;
      g_Config.Group_MultiRecursive_Ldiversity = (float)minRec;
      First = FALSE;
    } else {
      g_Config.Group_MultiEntropy_Ldiversity = Min(minEnt, (float)g_Config.Group_MultiEntropy_Ldiversity);
      g_Config.Group_MultiRecursive_Ldiversity = Min((float)minRec, (float)g_Config.Group_MultiRecursive_Ldiversity);
    }
    free_var(&Var);
  }
  delete[] pSet;
  delete[] pSetIndex;
}

/*======*/
/* RISK */
/*======*/
/* simplified; removing never-used argus implementation */
inline double compute_risk(int freq, double weight) {
  // Calculate base pk
  double pk = (double)freq / weight;

  pk = pk - 0.0001;
  if (freq > 2) {
    return pk / (freq - (1.0 - pk));
  }

  if (freq == 2) {
    double ratio = pk / (1.0 - pk);
    return ratio - pow(ratio, 2.0) * log(1.0 / pk);
  }

  if (freq == 1) {
    return (pk / (1.0 - pk)) * log(1.0 / pk);
  }
  // Default fallback if freq is 0 or negative
  return 0.0;
}

/*======*/
/* MAIN */
/*======*/
// [[Rcpp::export]]
RcppExport SEXP measure_risk_cpp(SEXP data, SEXP weighted_R, SEXP n_key_vars_R, SEXP l_recurs_c_R, SEXP ldiv_index_R, SEXP missing_value_R) {
  Rcpp::NumericMatrix Mat(data);
  Rcpp::NumericMatrix Res(Mat.rows(), 3);
  int NbRow = Mat.rows();
  int NbCol = Mat.cols();

  g_Config.Nb_QuasiId_Var = Rcpp::as<int>(n_key_vars_R);
  g_Config.missing_value = Rcpp::as<double>(missing_value_R);
  g_Config.is_weighted = Rcpp::as<bool>(weighted_R) ? 1 : 0;
  g_Config.Ldiversity_Recursivity_Constant = Rcpp::as<float>(l_recurs_c_R);
  if (g_Config.Ldiversity_Recursivity_Constant <= 0) {
    g_Config.Ldiversity_Recursivity_Constant = 1.0f;
  }

  Rcpp::NumericVector ldiv_index_RR(ldiv_index_R);
  int n_ldiv = (ldiv_index_RR(0) > 0) ? ldiv_index_RR.length() : 0;
  Rcpp::NumericMatrix Mat_Risk(NbRow, n_ldiv * 3 + 2);

  for (int i = 0; i < MAX_SENSITIVE_VAR; i++) {
    init_var(&g_Config.Sensitive_Var[i]);
    g_Config.Sensitive_Var[i].Require_Ldiversity = FALSE;
  }

  if (n_ldiv > 0) {
    for (int i = 0; i < n_ldiv; i++) {
      g_Config.Sensitive_Var[i].Require_Ldiversity = TRUE;
      g_Config.Sensitive_Var[i].position = (int)ldiv_index_RR(i) - 1;
    }
  }

  double *group_key = new double[g_Config.Nb_QuasiId_Var];
  double *obs_key = new double[g_Config.Nb_QuasiId_Var];
  int current_obs = 0;
  int group_count = 0;
  double global_risk_ER = 0.0;

  while (current_obs < NbRow) {
    group_count++;
    int block_start = current_obs;
    int group_size = 0;
    double group_weight = 0.0;

    for (int i = 0; i < g_Config.Nb_QuasiId_Var; i++) {
      group_key[i] = Mat(current_obs, i);
    }

    // Reset sensitive vars and missing counters for each group
    for (int i = 0; i < n_ldiv; i++) {
      free_var(&g_Config.Sensitive_Var[i]);
      init_var(&g_Config.Sensitive_Var[i]);
      g_Config.Sensitive_Var[i].Nb_Missing_Value_In_Group = 0;
      g_Config.Sensitive_Var[i].Require_Ldiversity = TRUE;
      g_Config.Sensitive_Var[i].position = (int)ldiv_index_RR(i) - 1;
    }

    do {
      if (g_Config.is_weighted) {
        group_weight += Mat(current_obs, NbCol - 1);
      }

      for (int i = 0; i < n_ldiv; i++) {
        add_var_cat_value(&g_Config.Sensitive_Var[i], Mat(current_obs, g_Config.Sensitive_Var[i].position));
      }
      group_size++;
      current_obs++;
      if (current_obs >= NbRow) break;
      for (int i = 0; i < g_Config.Nb_QuasiId_Var; i++) {
        obs_key[i] = Mat(current_obs, i);
      }
    } while (is_same_key_Safe(group_key, obs_key, g_Config.Nb_QuasiId_Var));

    double group_risk = g_Config.is_weighted ? compute_risk(group_size, group_weight) : 1.0 / group_size;
    global_risk_ER += group_size * group_risk;

    if (n_ldiv > 0) {
      compute_group_ldiversity(group_size, g_Config.Sensitive_Var, n_ldiv);
      if (n_ldiv >= 2) {
        Compute_Multi_LDiversity(block_start, group_size, Mat, ldiv_index_RR);
      }
    }

    for (int i = block_start; i < current_obs; i++) {
      Res(i, 0) = group_count;
      Res(i, 1) = group_risk;
      Res(i, 2) = group_size;
      for (int v = 0; v < n_ldiv; v++) {
        Mat_Risk(i, v * 3) = g_Config.Sensitive_Var[v].Group_Distinct_Ldiversity;
        Mat_Risk(i, v * 3 + 1) = g_Config.Sensitive_Var[v].Group_Entropy_Ldiversity;
        Mat_Risk(i, v * 3 + 2) = g_Config.Sensitive_Var[v].Group_Recursive_Ldiversity;
      }
      if (n_ldiv >= 2) {
        Mat_Risk(i, Mat_Risk.cols() - 2) = (double)g_Config.Group_MultiEntropy_Ldiversity;
        Mat_Risk(i, Mat_Risk.cols() - 1) = (double)g_Config.Group_MultiRecursive_Ldiversity;
      }
    }
  }

  delete[] group_key;
  delete[] obs_key;
  for (int i = 0; i < MAX_SENSITIVE_VAR; i++) free_var(&g_Config.Sensitive_Var[i]);

  double obs_count_final = (double)NbRow;
  double final_global_risk = global_risk_ER / obs_count_final;
  return Rcpp::List::create(
    Rcpp::Named("global_risk_ER") = global_risk_ER,
    Rcpp::Named("global_risk") = final_global_risk,
    Rcpp::Named("global_risk_pct") = final_global_risk * 100.0,
    Rcpp::Named("Res") = Res,
    Rcpp::Named("Mat_Risk") = Mat_Risk
  );
}
