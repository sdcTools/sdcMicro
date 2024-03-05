 /**
 * Stata plugin for computing a risk threshold level based on global risk
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
  * This plugin computes a risk threshold based on a global risk value provided as an input
  * It is based on the methodology described in the micro-argus 4.1 manual section 2.3.4.1 p.24
  *
  * Note that
  * 1) *** THE DATASET MUST BE SORTED BY AND DESCENDING RISK PRIOR TO THE CALL ***
  * 2) *** THE OBSERVATION (INDIVIDUAL) RISK MUST BE COMPUPTED BEFORE ***
  * 3) The inputs consist risk variable followed by global risk value provided as argument
  */


char buf_treshold[1024];
// character buf_tresholdfer to display messages

/*=====================*/
/* Main Stata function */
/*=====================*/
//int argc, char *argv[]
// [[Rcpp::export]]
RcppExport SEXP measure_threshold(SEXP data, SEXP global_risk_R)
{
	// int rc = 0;

	Rcpp::NumericVector risk_var(data);
	int NbRow = risk_var.length();

	int j;
		double ER = 0.0;					//<the expected number or re-identificatiuo
		size_t sum_freq = 0;					//< the running sum of the frequency
		double sum_risk = 0.0;			//< the running sum of the risk (represent left member of equation 17)
		double current_risk = 0.0;		//< the current risk (start with
		double sum = 0.0;					//< the sum of safe+unsafe risks
		double risk = 0.0;				//< the running sum of the risk
		double threshold = 0.0;			//< the computed observation level threshold

		// read global risk argument
		double global_risk = Rcpp::as<double>(global_risk_R);

		//double global_risk = atof(argv[0]);					//< the global risk

		if (global_risk <= 0.0 || global_risk >= 1.0)
		{
			// rc = -1;
			return NULL;
		}
		else
		{
			// count number of observations and compute sum of all risks
			size_t n_obs = 0;

//			for (j = SF_GetRowStart(); j <= SF_GetRowEnd(); j++)
			ForLoop (j, NbRow)
			{
				// Always returns TRUE?
				//if (g_pDataset->IsRowSelected(j))
				//{
					n_obs++;
					current_risk = risk_var(j);
					//g_pDataset->GetValue(0, j, &current_risk);

					// sum of all risks
					sum_risk += current_risk;
				//}
			}

			// compute ER (expected number of re-identification)
			ER = global_risk * n_obs;

 			// init current risk as value of first observation

//			for (j = SF_GetRowStart(); j <= SF_GetRowEnd(); j++)
			ForLoop (j, NbRow)
			{
				// Always returns TRUE?
				//if (g_pDataset->IsRowSelected(j))
				//{
					current_risk = risk_var(j);
					//g_pDataset->GetValue(0, j, &current_risk);
					break;
				//}
			}

			// quick check if the file level of risk is actually safe
/* Pascal's Bug		if (current_risk * n_obs < ER) */

			if (sum_risk < ER)
			{
				// return this as threshold value
				threshold = current_risk;
			}
			else
			{
				// loop over all observations (high risk to low risk)
//				for (j = SF_GetRowStart(); j <= SF_GetRowEnd(); j++)
				ForLoop (j, NbRow)
				{
					// Always returns TRUE?
					//if (g_pDataset->IsRowSelected(j))
					//{
						// read individual risks
						risk = risk_var(j);
						//g_pDataset->GetValue(0, j, &risk);

						// check if the risk of this observation has changed (has a lower level)
						if (risk != current_risk)
						{
							// check if we have reach the threshold
							sum = sum_risk + (risk * sum_freq); // this is equation 17 in relation to ER
							if (sum < ER)
							{
								// found
								threshold = risk;
								break;
							}

							// keep going
							current_risk = risk;
						}

						// add this obs to the unsafe side
						sum_freq++;

						// remove this risk from the remaining sum (left member of equation 17)
						sum_risk -= risk;
					//}
				}
			}

			// Return the threshold as scalars in Stata
			//SF_ScalarSave("global_threshold", threshold);
			//SF_ScalarSave("global_threshold_safe", threshold);
			//SF_ScalarSave("global_threshold_unsafe", current_risk);
			
			// end
			
			return Rcpp::List::create(
			  Rcpp::Named( "global_threshold" ) = threshold,
			  Rcpp::Named( "global_threshold_safe" ) = threshold,
			  Rcpp::Named( "global_threshold_unsafe" ) = current_risk
			);
		}
		// // end
		// time(&end);
		
	//}

	//return rc;
}
