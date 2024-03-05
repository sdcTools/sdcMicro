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

#pragma once

#include "Framework.h"

//Mdav
enum{
   es_NbHashBitXX  = 16,
   es_NbHashListXX = 1 << es_NbHashBitXX
 };

typedef double TData;
typedef float TDist;
inline int g_NbRow, g_NbVar, g_K;
inline TData g_MissingValue = -1.0f;
inline TData *g_pWeight = NULL;
inline TDist *g_pMissingValueDist = NULL;


#define es_Var_Type e_Var_Double    //Important: es_Var_Type much match TData..!

inline TDist DistXX(TData *Valuevs, TData *pT)
{
  TDist Dist = 0.0;
    ForLoopD (i, g_NbVar){
      TData Value = Valuevs[i];//GetValue(i, s);

      if ((Value == g_MissingValue) ^ (pT[i] == g_MissingValue)){
        Dist += g_pMissingValueDist[i];
      }else{
        Dist += (TDist) (Squared(Value - pT[i]) * g_pWeight[i]);
      }
    }
  return Dist;
}


inline TDist Dist(TData *Value1v, TData *Value2v)
{
  TDist Dist = 0.0;
  ForLoopD (i,g_NbVar){
    TData Value1 = Value1v[i],
        Value2 = Value2v[i];

    if ((Value1 == g_MissingValue) ^ (Value2 == g_MissingValue)){
      Dist += g_pMissingValueDist[i];
    }else{
      Dist += (TDist) (Squared(Value1 - Value2) * g_pWeight[i]);
    }
  }

  return Dist;
}

inline void AddRow(TData *pTotal, TData *Value1v, int *pNbNonMissingValue)
{
    ForLoopD (j, g_NbVar){
      TData Value = Value1v[j];//GetValue(j, Row);

      if (Value != g_MissingValue){
        pTotal[j] += Value;
        ++pNbNonMissingValue[j];
      }
    }
}
