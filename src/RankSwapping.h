/**
 * RankSwapping
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
RcppExport SEXP RankSwap(SEXP data,SEXP data2,SEXP g_MissingValue_R,
SEXP g_TopRatio_R,SEXP g_BottomRatio_R,SEXP g_K0_R,SEXP g_R0_R,SEXP g_P_R,SEXP seed_R) {
  BEGIN_RCPP
  int i, j, k;
  Rcpp::NumericMatrix Mat(data);  // creates Rcpp matrix from SEXP
  Rcpp::NumericMatrix Res(data2); // creates Rcpp matrix from SEXP
  int g_NbRow = Mat.rows();
  int g_NbVar = Mat.cols();
  int seed = Rcpp::as<int>(seed_R);

  float g_TopRatio = Rcpp::as<double>(g_TopRatio_R);
  float g_BottomRatio = Rcpp::as<double>(g_BottomRatio_R);
  float g_K0 = Rcpp::as<double>(g_K0_R);
  float g_R0 = Rcpp::as<double>(g_R0_R);
  float g_P = Rcpp::as<double>(g_P_R);
  if(seed==-1)
    SetRandSeed(TimeGetMilliSecond());
  else
    SetRandSeed(seed);
  double g_MissingValue = Rcpp::as<double>(g_MissingValue_R);
  double *pNewValue = new double[g_NbRow],
      *pOrderedValue = new double[g_NbRow];
  double MinV=0 ,MaxV=0;
  /*float g_TopRatio = 0.0f, g_BottomRatio = 0.0f,  g_R0 = -1.0f,g_K0 = -1.0f,  g_P = 0.0f;*/

  int HashList[es_NbHashList+1];
    int *pFromIndex = new int[g_NbRow],
    *pIndexR = new int[g_NbRow],
    *pListNext = new int[g_NbRow];
  BOOL *pSwapFlag = new BOOL[g_NbRow],
    *pOrderedSwapFlag = new BOOL[g_NbRow];
  g_TopRatio = Bound(g_TopRatio * 0.01f, 0.0f, 1.0f);
  g_BottomRatio = Bound(g_BottomRatio * 0.01f, 0.0f, 1.0f);
  ForLoop (i, g_NbVar)
  {
    BOOL First = TRUE;
    int NbMissing = 0;

        //=== Clear SwapFlag & init NewValue
    ClearMemT(pSwapFlag, g_NbRow);
    ForLoop (j, g_NbRow)
    {
      double Value = Mat(j, i);
      pNewValue[j] = Value;

      if (Value != g_MissingValue)
      {
        if (First)
        {
          First = FALSE;
          MinV = MaxV = Value;
        }
        else
        {
          MinV = Min(MinV, Value);
          MaxV = Max(MaxV, Value);
        }
      }
    }

      // === Hash sort the data by Var_i from highest to lowest value
      // (with missing value being the lowest value possible)
    if (MaxV == MinV)
      ++MaxV;

    double DeltaV = MaxV - MinV;
    DeltaV *= (es_NbHashList + 1.0) / es_NbHashList;

//    ClearMemVT(pListNext, -1, g_NbRow);
    ClearMemV(HashList, -1);
    ForLoop (k, g_NbRow)
    {
      double Value = pNewValue[k];

      if (Value == g_MissingValue)
      {
        pListNext[k] = HashList[es_NbHashList];
        HashList[es_NbHashList] = k;
        continue;
      }

      int HashIndex = (int) (es_NbHashList * (MaxV - Value) / DeltaV);

      int Index = HashList[HashIndex];

      if (Index >= 0 && pNewValue[Index] > Value)
      {
        while (pListNext[Index] >= 0 && pNewValue[pListNext[Index]] > Value)
          Index = pListNext[Index];

        int NextIndex = pListNext[Index];
        pListNext[Index] = k;
        pListNext[k] = NextIndex;
      }
      else
      {
        pListNext[k] = Index;
        HashList[HashIndex] = k;
      }
    }

    int OrderedIndex = 0;
    ForLoop (k, es_NbHashList+1)
    {
      int Index = HashList[k];

      while (Index >= 0)
      {
        pOrderedValue[OrderedIndex] = pNewValue[Index];
        pFromIndex[OrderedIndex] = Index;

        if (k == es_NbHashList)   // Flag missing as swapped
        {
          pSwapFlag[OrderedIndex] = TRUE;
          ++NbMissing;
        }

        ++OrderedIndex;
        Index = pListNext[Index];
      }
    }

    ASSERT(OrderedIndex == g_NbRow);

    Swap(pOrderedValue, pNewValue);

      //=== Top and bottom code Var_i and then flag as swapped
    int NbNonMissing = g_NbRow - NbMissing;
    int Top = (int) (NbNonMissing * g_TopRatio),
      Bottom = NbNonMissing - (int) floor(NbNonMissing * g_BottomRatio);

        //=== Top coding
    double Sum = 0.0;
    First = TRUE;

    ForLoop (j, Top)
      Sum += pNewValue[j];

    double TopValue = Top ? pNewValue[Top - 1] : pNewValue[Top],
        Avr = Top ? Sum / Top : 0.0;

    ForLoop (j, Top)
    {
      pNewValue[j] = Avr;
      pSwapFlag[j] = TRUE;

      if (First)
      {
        First = FALSE;
        MinV = MaxV = Avr;
      }
      else
      {
        MinV = Min(MinV, Avr);
        MaxV = Max(MaxV, Avr);
      }
    }

         //=== Bottom coding
    Sum = 0.0;
    for (j = Bottom; j < NbNonMissing; ++j)
      Sum += pNewValue[j];

    double BottomValue = (NbNonMissing - Bottom) ? pNewValue[Bottom] : pNewValue[Bottom-1];
    Avr = Sum ? Sum / (NbNonMissing - Bottom) : 0.0;

    for (j = Bottom; j < NbNonMissing; ++j)
    {
      pNewValue[j] = Avr;
      pSwapFlag[j] = TRUE;

      if (First)
      {
        First = FALSE;
        MinV = MaxV = Avr;
      }
      else
      {
        MinV = Min(MinV, Avr);
        MaxV = Max(MaxV, Avr);
      }
    }

      //=== Hash sort the data by SwapFlag & NewValue (from highest to lowest value)
    if (MaxV == MinV)
      ++MaxV;

    DeltaV = MaxV - MinV;
    DeltaV *= (es_NbHashList + 1.0) / es_NbHashList;

//    ClearMemVT(pListNext, -1, g_NbRow);
    ClearMemV(HashList, -1);

    ForLoop (k, g_NbRow)
    {
      double Value = pNewValue[k];

      if (Value == g_MissingValue)
      {
        pListNext[k] = HashList[es_NbHashList];
        HashList[es_NbHashList] = k;
        continue;
      }

      int HashIndex = ((int) (es_NbHashList * (MaxV - Value) / DeltaV)) >> 1;
      if (pSwapFlag[k])
        HashIndex |= 1 << (es_NbHashBit - 1);

      int Index = HashList[HashIndex];

      if (Index >= 0 && pNewValue[Index] > Value)
      {
        while (pListNext[Index] >= 0 && pNewValue[pListNext[Index]] > Value)
          Index = pListNext[Index];

        int NextIndex = pListNext[Index];
        pListNext[Index] = k;
        pListNext[k] = NextIndex;
      }
      else
      {
        pListNext[k] = Index;
        HashList[HashIndex] = k;
      }
    }

    OrderedIndex = 0;

    ForLoop (k, es_NbHashList+1)
    {
      int Index = HashList[k];

      while (Index >= 0)
      {
        pOrderedValue[OrderedIndex] = pNewValue[Index];
        pIndexR[OrderedIndex] = pFromIndex[Index];
        pOrderedSwapFlag[OrderedIndex] = pSwapFlag[Index];

        ++OrderedIndex;
        Index = pListNext[Index];
      }
    }

    Swap(pOrderedValue, pNewValue);
    Swap(pIndexR, pFromIndex);    // Use pIndexR to save the memory allocation of a 'pOrderedFromIndex'
    Swap(pOrderedSwapFlag, pSwapFlag);

      //=== Calculate some parameters
    int NbSwapped = 0;

    ForLoop (j, g_NbRow)
      NbSwapped += pSwapFlag[j];

    int NbNotSwapped = g_NbRow - NbSwapped;

    Sum = 0.0;
    double SumSquared = 0.0;

    ForLoop (j, NbNotSwapped)
    {
      Sum += pNewValue[j];
      SumSquared += Squared(pNewValue[j]);
    }

    Avr = NbNotSwapped ? Sum / NbNotSwapped : 0.0;
    float Variance = (float) (NbNotSwapped ? SumSquared / NbNotSwapped - Squared(Avr) : 0.0f);
//    float R0, K0, P;
    float P;

    double DeltaValue = Abs(BottomValue - TopValue);

    if (g_R0 >= 0.0f)
    {
//      R0 = g_R0;
//      K0 = (float) (Avr ? sqrt(3.0f / 4.0f * (1.0f - g_R0) * Variance) / Abs(Avr) : 0.0f);
      P = (float) (DeltaValue ? sqrt(2.0f * Variance * (1.0f - g_R0)) / DeltaValue : 0.0f);
    }
    else if (g_K0 >= 0.0f)
    {
//      R0 = (float) (Variance ? (1.0f - 4.0f / 3.0f * Squared(Avr) * Squared(g_K0) / Variance) : 0.0f);
//      K0 = g_K0;
      P = (float) (DeltaValue ? sqrt(8.0f/3.0f) * g_K0 * Abs(Avr) / DeltaValue : 0.0f);
    }
    else
    {
//      K0 = (float) (Avr ? sqrt(3.0f/8.0f) * g_P * Abs(BottomValue - TopValue) / Abs(Avr) : 0.0f);
//      R0 = (float) (Variance ? (1.0f - 0.5f * Squared((BottomValue - TopValue) * g_P )/ Variance) : 0.0f);
      P = g_P;
    }


    ForLoop (j, NbNotSwapped)
    {
      if (pSwapFlag[j])
        continue;

      int r = Min(NbNotSwapped, (int) (j + NbNotSwapped * P)),
        NbIndexR = 0;

      for (k = j + 1; k < r; ++k)
      {
        if (!pSwapFlag[k])
          pIndexR[NbIndexR++] = k;
      }

      if (NbIndexR)
      {
        int PrimeJ = pIndexR[Random(NbIndexR)];
        Swap(pNewValue[j], pNewValue[PrimeJ]);
        pSwapFlag[j] = pSwapFlag[PrimeJ] = TRUE;
      }
    }

    ForLoop (j, g_NbRow)
      Res(pFromIndex[j], i)=pNewValue[j];
  }





  return Rcpp::List::create(
      Rcpp::Named( "Res" ) = Res
  ) ;

  END_RCPP

}

