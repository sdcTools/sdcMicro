#include "edmonds.h"

BOOL *CData::m_Numerical = NULL;
float *CData::m_Weight = NULL;
int CData::m_NbVariable = 0, CData::m_NbAncestor = 0, CData::m_CategoryCountVar = 0;
CData::SAncestor *CData::m_Ancestor = NULL;
TDist *CData::m_pAllDist = NULL;
TValue_LocalRec *CData::m_pMissingValueDist = NULL;
TValue_LocalRec g_MissingValue_LocalRec = -1.0f;

int CData :: Init(int NbVariable, SEXP weights)
{
  Rcpp::NumericMatrix weight(weights);

  if (NbVariable <= 0)
  {
    return 0;
  }

  int i;

  m_NbVariable = NbVariable;
  m_Numerical = new BOOL[m_NbVariable];
  m_Weight = new float[m_NbVariable];

  if (m_Ancestor == NULL)
  {
    m_Ancestor = new SAncestor[NbVariable];
    ClearMemT(m_Ancestor, NbVariable);
  }

  ForLoop (i, CData::m_NbVariable)
  {
    if (weight(i,1))
    {
      m_Weight[i] = (float) weight(i,0);
      m_Numerical[i] = FALSE;
    }
    else
    {
      m_Weight[i] = (float) weight(i,0);
      m_Numerical[i] = (m_Ancestor[i].Nb == 0);
    }
  }

  return m_NbVariable;
}

int CData :: InitAncestors(int NbVariable, SEXP ancestors)
{
  int VarPos = NbVariable;
  Rcpp::NumericMatrix ancestor(ancestors);
  int ancestorLen = ancestor.cols();

  if (m_Ancestor == NULL)
  {
    m_Ancestor = new SAncestor[NbVariable];
    ClearMemT(m_Ancestor, NbVariable);
  }
  m_NbAncestor = 0;

  for (int i = 0; i < ancestorLen; i++)
  {
    int VarNum = ancestor(i,0),
      NbAncestor = ancestor(i,1);

    m_Ancestor[VarNum].Nb = NbAncestor;
    m_Ancestor[VarNum].Index = VarPos;

    if (m_Numerical)
      m_Numerical[VarNum] = FALSE;

    VarPos += NbAncestor;
    m_NbAncestor += NbAncestor;
  }

  return m_NbAncestor;
}


void CData :: Uninit_LocalRec(void)
{
  m_NbVariable = m_NbAncestor = m_CategoryCountVar = 0;
  CleanDeleteT(m_Numerical);
  CleanDeleteT(m_Weight);
  CleanDeleteT(m_Ancestor);
  CleanDeleteT(m_pMissingValueDist);
}


int LoadData(int NbRow, double SV_MissingValue, CData *pData, SEXP Mat)
{
  int i, j;
  Rcpp::NumericMatrix Mat_LocalRec(Mat);

  for (i = 0; i < NbRow; ++i)
  {
    pData[i].m_Index = i;

    ForLoop (j, CData::m_NbVariable)
      pData[i].m_Value[j] = Mat_LocalRec(i, j);
      // g_pDataset->GetValue(j, i, pData[i].m_Value + j);

    ForLoop (j, CData::m_NbAncestor)
    {
      pData[i].m_Value[j + CData::m_NbVariable] = Mat_LocalRec(i, j + CData::m_NbVariable);
      // g_pDataset->GetValue(j + CData::m_NbVariable * 2, i,
            // pData[i].m_Value + j + CData::m_NbVariable);
    }

    if (CData::m_CategoryCountVar)
    {
      CData::m_CategoryCountVar = CData::m_NbVariable + CData::m_NbAncestor;
      pData[i].m_Value[CData::m_CategoryCountVar] = Mat_LocalRec(i, CData::m_NbVariable + CData::m_CategoryCountVar);
      // g_pDataset->GetValue(CData::m_NbVariable + CData::m_CategoryCountVar, i,
            // pData[i].m_Value + CData::m_CategoryCountVar);

      if (pData[i].m_Value[CData::m_CategoryCountVar] <= 0)
      {
        pData[i].m_Value[CData::m_CategoryCountVar] = 1;
      }
    }
  }

  //=== Compute Ranges
  CData::m_pMissingValueDist = new TValue_LocalRec[CData::m_NbVariable];
  g_MissingValue_LocalRec = (TValue_LocalRec) SV_MissingValue;

  ForLoop (j, CData::m_NbVariable)
  {
    double MinV = 0, MaxV = 0;
    BOOL First = TRUE;

    ForLoop (i, NbRow)
    {
      TValue_LocalRec Value = pData[i].m_Value[j];

      if (Value == g_MissingValue_LocalRec)
        continue;

      if (First)
      {
        MinV = MaxV = Value;
        First = FALSE;
      }
      else
      {
        MinV = Min(MinV, Value);
        MaxV = Max(MaxV, Value);
      }
    }

    float Range = (float) (MaxV - MinV);

    if (Range)
    {
      CData::m_pMissingValueDist[j] = CData::m_Weight[j];
      CData::m_Weight[j] /= Range;
    }
    else
      CData::m_pMissingValueDist[j] = 0.0f;
  }

    //=== Compute Categories
  if (!CData::m_CategoryCountVar)
  {
    int CatNum = 1;
    CatNum = CatNum - 1; // to avoid notes in CRAN checks; 
    int CatSize = 0;
    CatSize = CatSize - 1; // to avoid notes in CRAN checks; 
    CData::m_CategoryCountVar = CData::m_NbVariable + CData::m_NbAncestor;

    ForLoop (i, NbRow)
    {
      if (i)
      {
        ForLoop (j, CData::m_NbVariable)
        {
          if (pData[i].m_Value[j] != pData[i-1].m_Value[j])
            break;
        }

        if (j < CData::m_NbVariable // New Category ?
          || i == NbRow - 1)
        {
          ForLoop (j, CatSize)
            pData[i-j-1].m_Value[CData::m_CategoryCountVar] = CatSize;

          ++CatNum;
          CatSize = 0;

          if (i == NbRow - 1)
            pData[i].m_Value[CData::m_CategoryCountVar] = 1;
        }
      }

      ++CatSize;
    }
  }

  return i;
}

//TDist g_MinDist = 1000000.0f;

TDist dist(CData *x, CData *y)
{
  if (CData::m_pAllDist)
  {
    ASSERT(x->m_Index != y->m_Index);

    if (x->m_Index > y->m_Index)
      return y->m_pDist[x->m_Index - y->m_Index];

    return x->m_pDist[y->m_Index - x->m_Index];//-1];   // -1 already applied to m_pDist
  }

  TDist s = 0.0;

  ForLoopD (i, CData::m_NbVariable)
  {
    if (!CData::m_Numerical[i])
    {
      if (x->m_Value[i] != y->m_Value[i])
        s += CData::m_Weight[i];
    }
    else
    {
      if ((x->m_Value[i] == g_MissingValue_LocalRec) ^ (y->m_Value[i] == g_MissingValue_LocalRec))
      {
        s += CData::m_pMissingValueDist[i];
      }
      else
        s += (TDist) Abs(x->m_Value[i] - y->m_Value[i]) * CData::m_Weight[i];
    }
  }

//  if (s)
//    g_MinDist = Min(g_MinDist, s);

  //return (int) (s + 0.5);
  return s;
}
