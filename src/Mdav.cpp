/**
 * MDAV [Fixed length Micro-aggregation]
 *
 * copyright: Organisation For Economic Co-Operation And Development
 * adapted for R usage by Alexander Kowarik, data-analysis OG
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

//* ======================================================================== *
//*                           Enum & Globals
//* ======================================================================== *

#include <Rcpp.h>
using namespace Rcpp;

#include "Framework.h"
#include "Mdav.h"

// [[Rcpp::export]]
RcppExport SEXP Mdav(SEXP data,SEXP data2,SEXP g_MissingValue_R,SEXP weights_R,SEXP g_K_R) {
  BEGIN_RCPP
  Rcpp::NumericMatrix Mat(data);  // creates Rcpp matrix from SEXP
  Rcpp::NumericMatrix Res(data2); // creates Rcpp matrix from SEXP
//  Rcpp::NumericMatrix DD(dists); // creates Rcpp matrix from SEXP
  Rcpp::NumericVector weights(weights_R); // creates Rcpp matrix from SEXP
  g_K = Rcpp::as<int>(g_K_R);
  g_NbRow = Mat.rows();
  g_NbVar = Mat.cols();
  int i, j, k, l,ii,
  FirstIndex = 0,NbRowLeft = g_NbRow,
  *pNextIndex = new int[g_NbRow], // To link the Rows that are still not averaged
  *pPrevIndex = new int[g_NbRow],
  *pNextInHash = new int[g_NbRow],  // To link the Rows for the Hash sorting
  *pNbNonMissingValue = new int[g_NbVar];
  int HashList[es_NbHashListXX];
  TData *pCenter = new TData[g_NbVar];
  TDist *pDist = new TDist[g_NbRow];
  TData *pVar1 = new TData[g_NbVar];
  TData *pVar2 = new TData[g_NbVar];
  g_pWeight = new TData[g_NbVar];
  g_pMissingValueDist = new TDist[g_NbVar];
  float g_MissingValue_temp=Rcpp::as<float>(g_MissingValue_R);
  g_MissingValue=g_MissingValue_temp;
  ForLoop (i, g_NbVar){
      g_pWeight[i] = 1;
  }
  //=== Compute Var Variances & Divide Weight by them
  ForLoop (i, g_NbVar){
    //=== Compute Average & Squared Sum
    double Avr = 0.0,
      SquaredSum = 0.0;
    int NbNonMissingRow = 0;
    TData MinV = 0.0, MaxV = 0.0;
    BOOL First = TRUE;
    
    ForLoop (j, g_NbRow){
      TData Value = Mat(j, i);
      
      if (Value == g_MissingValue){
            continue;
      }
      if (First){
        MinV = MaxV = Value;
        First = FALSE;
      }else{
        MinV = Min(MinV, Value);
        MaxV = Max(MaxV, Value);
      }
      ++NbNonMissingRow;
      Avr += Value;
      SquaredSum += Squared(Value);
    }
    
    if (NbNonMissingRow){
      Avr /= NbNonMissingRow;
      SquaredSum /= NbNonMissingRow;
    }
    double Variance = SquaredSum - Squared(Avr);
    if (Variance){
      g_pWeight[i] /= Variance;
    }
    g_pMissingValueDist[i] = (TDist) (Squared(MaxV - MinV) * g_pWeight[i]);
  }


  //=== Build list of active Rows
  ForLoop (i, g_NbRow-1){
    pNextIndex[i] = i + 1;
  }
  pNextIndex[g_NbRow-1] = -1;

  ForLoop (i, g_NbRow-1){
    pPrevIndex[i+1] = i;
  }
  pPrevIndex[0] = -1;

    //=== Core Loop: It always removes to groups of observations of size k therefore it has to stop, when only NbRowLeft-g_K*2>=g_K
  int Loop = 1;
  Loop = Loop - 1; // to avoid notes on CRAN checks;
  int ngroups; // ngroups introduced to fix the bug that groups smaller than k remain in the last aggregation step after the while loop
//  while (NbRowLeft- g_K * 2>=g_K)
  while(NbRowLeft- g_K >=g_K)
  {
    if(NbRowLeft- g_K * 2<g_K)
      ngroups=1;
    else
      ngroups=2;
//    Rprintf("NbRowLeft %d \n",NbRowLeft);
//    Rprintf("g_K %d \n \n",g_K);
    ++Loop;
    //=== Calculate Center
    ClearMemT(pCenter, g_NbVar);
    ClearMemT(pNbNonMissingValue, g_NbVar);

    for (i = FirstIndex; i >= 0; i = pNextIndex[i]){
      ForLoop (ii, g_NbVar){
                pVar1[ii]=Mat(i,ii);
      }
      AddRow(pCenter, pVar1, pNbNonMissingValue);
    }

    ForLoop (j, g_NbVar){
      if (pNbNonMissingValue[j])
        pCenter[j] /= pNbNonMissingValue[j];
      else
        pCenter[j] = g_MissingValue;

    }

      //=== Find Farthest Row from Center
    int CurrentFarthest = FirstIndex;

//    k = 0;

    for (i = FirstIndex; i >= 0; i = pNextIndex[i]){
      ForLoop (ii, g_NbVar){
          pVar1[ii]=Mat(i,ii);
      }
      pDist[i] = DistXX(pVar1, pCenter);
      if (pDist[i] > pDist[CurrentFarthest])
        CurrentFarthest = i;

//      ++k;
    }

//    ASSERT(k == NbRowLeft);

      // Find the K closest rows & average them, both from Farthest Row & Opposite Farthest Row
    ForLoop (l, ngroups){
        //=== Calculate Distances from Farthest Row
      int Farthest = FirstIndex, Closest = FirstIndex;

      for (i = FirstIndex; i >= 0; i = pNextIndex[i]){
        ForLoop (ii, g_NbVar){
            pVar1[ii]=Mat(i,ii);
            pVar2[ii]=Mat(CurrentFarthest,ii);
        }
        pDist[i] = Dist(pVar1, pVar2);
        if (pDist[i] > pDist[Farthest])
          Farthest = i;
        else if (pDist[i] < pDist[Closest])
          Closest = i;
      }

      CurrentFarthest = Farthest;


        //=== Hash Sort from Closest to Farthest
      TDist MinD = pDist[Closest];
      TDist DeltaD = pDist[Farthest] - MinD;

      if (!DeltaD)
        ++DeltaD;
      DeltaD *= (es_NbHashListXX + 1.0) / es_NbHashListXX;

      ClearMemV(HashList, -1);

      for (i = FirstIndex; i >= 0; i = pNextIndex[i]){
        TDist Dist = pDist[i];
        int HashIndex = (int) (es_NbHashListXX * (Dist - MinD) / DeltaD);

        int Index = HashList[HashIndex];

        if (Index >= 0 && pDist[Index] < Dist){
          while (pNextInHash[Index] >= 0 && pDist[pNextInHash[Index]] < Dist){
            Index = pNextInHash[Index];
          }

          int NextIndex = pNextInHash[Index];
          pNextInHash[Index] = i;
          pNextInHash[i] = NextIndex;
        }else{
          pNextInHash[i] = Index;
          HashList[HashIndex] = i;
        }
      }

        //=== Get the K closest Rows, average them & remove them from the set
      ClearMemT(pCenter, g_NbVar);
      ClearMemT(pNbNonMissingValue, g_NbVar);
      k = 0;

      ForLoop (i, es_NbHashListXX){
        int Index = HashList[i];

        while (Index >= 0){
          ForLoop (ii, g_NbVar){
                    pVar1[ii]=Mat(Index,ii);
          }
          AddRow(pCenter, pVar1, pNbNonMissingValue);
          ++k;
          if (k >= g_K){
            i = es_NbHashListXX;
            break;
          }

          Index = pNextInHash[Index];
        }
      }

      ForLoop (j, g_NbVar){
        if (pNbNonMissingValue[j])
          pCenter[j] /= pNbNonMissingValue[j];
        else
          pCenter[j] = g_MissingValue;
      }

      k = 0;

      ForLoop (i, es_NbHashListXX){
        int Index = HashList[i];

        while (Index >= 0){

          ForLoop (j, g_NbVar){
            Res(Index,j)=pCenter[j];
          }

            //=== Remove from List
          if (FirstIndex == Index)
            FirstIndex = pNextIndex[Index];

          if (pPrevIndex[Index] >= 0)
            pNextIndex[pPrevIndex[Index]] = pNextIndex[Index];

          if (pNextIndex[Index] >= 0)
            pPrevIndex[pNextIndex[Index]] = pPrevIndex[Index];

          ++k;
          if (k >= g_K)
          {
            i = es_NbHashListXX;
            break;
          }

          Index = pNextInHash[Index];
        }
      }

    }

    NbRowLeft -= g_K * ngroups;

  }
  if (NbRowLeft)
  {
//    Rprintf("FINAL NbRowLeft %d \n",NbRowLeft);
    ClearMemT(pCenter, g_NbVar);
    ClearMemT(pNbNonMissingValue, g_NbVar);

    k = 0;

    for (i = FirstIndex; i >= 0; i = pNextIndex[i])
    {
      ForLoop (ii, g_NbVar){
                pVar1[ii]=Mat(i,ii);
      }
      AddRow(pCenter, pVar1, pNbNonMissingValue);
      ++k;
    }

    ForLoop (j, g_NbVar){
      if (pNbNonMissingValue[j]){
        pCenter[j] /= NbRowLeft;
      }else{
        pCenter[j] = g_MissingValue;
      }
    }

    for (i = FirstIndex; i >= 0; i = pNextIndex[i]){
      ForLoop (j, g_NbVar){
        Res(i, j )=pCenter[j];
      }
    }
  }
//  /*funktionieren distanzen?!??!*/
//  ForLoop(i,g_NbRow){
//  ForLoopD(iii,g_NbRow){
//  ForLoop (ii, g_NbVar){
//      pVar1[ii]=Mat(i,ii);
//      pVar2[ii]=Mat(iii,ii);
//  }
//  DD(i,iii) = Dist(pVar1, pVar2);
//  }
//  }


  //=== Uninit
  CleanDeleteT(g_pMissingValueDist);
  CleanDeleteT(g_pWeight);
  CleanDeleteT(pNextIndex);
  CleanDeleteT(pPrevIndex);
  CleanDeleteT(pNextInHash);
  CleanDeleteT(pCenter);
  CleanDeleteT(pDist);
  CleanDeleteT(pVar1);
  CleanDeleteT(pVar2);
  CleanDeleteT(pNbNonMissingValue);
  return Rcpp::List::create(
      Rcpp::Named( "Res" ) = Res
  ) ;
//  return Rcpp::List::create(
//        Rcpp::Named( "DD" ) = DD
//    ) ;

  END_RCPP
}


