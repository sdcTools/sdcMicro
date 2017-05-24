/*
   An Implementation of the Edmonds' weighted matching algorithm
   Dec. 1997
   Nakamura Daishin
   daishin@im.uec.ac.jp
*/

//#include "edmonds.h"
#include "edmonds.cpp"

// Command lines :
// .NbRow 10 .k 3 .File example.dat .NbVar_LocRec 4 0.20 1 0.01 -2
// .NbRow 10 .File example.dat .NbVar_LocRec 4 0.20 1 0.01 -2

// 12012200150601
// 12075200157403

extern TDist g_Diameter, g_ShiftBound;

int g_NbRow_LocRec = 0;
float g_Epsilon;
vertex_type *g_Vertex = NULL;
adj_type *g_AdjType = NULL;
CData *g_Data = NULL;
int *g_Match = NULL;
EOutput g_Output;
BOOL g_Debug_LocalRec;

//extern TDist g_MinDist;
//extern TDist g_MinGreater = 99999999.0f,
//        g_MinZero = 99999999.0f;

void Uninit_LocalRec()
{
  CData::Uninit_LocalRec();

  CleanDeleteT(g_AdjType);
  CleanDeleteT(g_Match);
  CleanDeleteT(g_Vertex);
  CleanDeleteT(g_Data);
  CleanDeleteT(CData::m_pAllDist);
}


enum EMatch
{
  e_Match_Complete,
  e_Match_Kneib
};


RcppExport SEXP LocalRecProg_cpp(SEXP data, SEXP K_Level_R, SEXP FindLowestK_R, SEXP ancestor_R, SEXP weight_R, SEXP range_R, SEXP categoryCount_R, SEXP LowMemory_R, SEXP g_MissingValue_R)
{
BEGIN_RCPP
  int i, j;
  int c2;
  int K_Level;
  //uint TimeStart = TimeGetMilliSecond(), TimeMid;
  EMatch Match = e_Match_Complete;
  BOOL FindLowestK = FALSE, LowMemory = FALSE;

    //============= Init
  Uninit_LocalRec();

  g_Debug_LocalRec = FALSE;
  g_Output = e_Output_Average;
  g_Epsilon = es_Epsilon;

  Rcpp::NumericMatrix Mat_LocalRec(data);  // creates Rcpp matrix from SEXP
  g_NbRow_LocRec = Mat_LocalRec.rows();
  int NbVar_LocRec = Mat_LocalRec.cols();
  // NbVar_LocRec = NbTotalVar / 2;
  //=============init Parameters
  K_Level = Rcpp::as<int>(K_Level_R);
  //  printf("klevel %d \n",K_Level);
  if(K_Level >= 1) Match = e_Match_Kneib;
  if(Rcpp::as<bool>(range_R)) g_Output = e_Output_Range;
  FindLowestK = Rcpp::as<bool>(FindLowestK_R);
  LowMemory = Rcpp::as<bool>(LowMemory_R);
  double g_MissingValue_LocalRec = Rcpp::as<double>(g_MissingValue_R);
  if(g_MissingValue_LocalRec == 0) g_MissingValue_LocalRec = -1.0f;
  // Ancestors
  // 1. Column: Position of the variable with ancestor variables (0-based)
  // 2. Column: number of ancestor variables
  Rcpp::NumericMatrix ancestors(ancestor_R);
  int ancestorLen = ancestors.rows();
  int ancestorCol = ancestors.cols();
  if(ancestorLen > 1 && ancestorCol>1)
  {
    for(j=0; j < ancestorLen; j++)
    {
      NbVar_LocRec -= ancestors(j,1);
    }
    // NbVar_LocRec = NbTotalVar / 2;
    // NbVar_LocRec: position of the first ancestor variable
    // ancestors: ancestor array
    CData::InitAncestors(NbVar_LocRec, ancestors);
  }
  Rcpp::NumericMatrix res(g_NbRow_LocRec, NbVar_LocRec); // result matrix that is returned

  // weight-vector:
  // 1. Column: Value of the weight for the variable
  // 2. Column: TRUE/FALSE if variable is of type categorial
  Rcpp::NumericMatrix weight(weight_R);
  if(NbVar_LocRec == weight.rows()) {
    CData::Init(NbVar_LocRec, weight);
  } else {
    Uninit_LocalRec();
    return Rcpp::wrap("Error: Not enough weights specified");
  }

  bool categoryCount = Rcpp::as<bool>(categoryCount_R);
  if(categoryCount) {
    CData::m_CategoryCountVar = TRUE;
    --NbVar_LocRec;
  }
  //printf("NbVar_LocRec %d \n",NbVar_LocRec);
  if (g_NbRow_LocRec <= 0 || CData::m_NbVariable <= 0)
  {
    Uninit_LocalRec();
    return Rcpp::wrap(-3);
  }

  if (Match == e_Match_Kneib
    && (K_Level >= g_NbRow_LocRec || K_Level < 1))
  {
    Uninit_LocalRec();
    return Rcpp::wrap(-4);
  }
    //============= Data Allocation & Loading
  g_Data = new CData[g_NbRow_LocRec];

  if (LoadData(g_NbRow_LocRec, g_MissingValue_LocalRec, g_Data, Mat_LocalRec) != g_NbRow_LocRec)
  {
    Uninit_LocalRec();
    return Rcpp::wrap(-5);
  }

  if (!LowMemory && g_NbRow_LocRec > 1 && CData::m_NbVariable > 2)
  {
    TDist *pAllDist = new TDist[g_NbRow_LocRec * (g_NbRow_LocRec - 1) / 2];
    int NbEntry = 0;

    ForLoop (i, g_NbRow_LocRec)
    {
      g_Data[i].m_pDist = pAllDist + NbEntry;

      int NbDist = g_NbRow_LocRec - i - 1;

      ForLoop (j, NbDist)
        g_Data[i].m_pDist[j] = dist(g_Data + i, g_Data + i + j + 1);

      --g_Data[i].m_pDist;    //Optim: avoid to have to do it each time dist() is called
      NbEntry += NbDist;
    }

    CData::m_pAllDist = pAllDist;

  }
  g_Vertex = MALLOC(vertex_type, (((long)g_NbRow_LocRec) * 3) / 2);
  for (i = 0; i < g_NbRow_LocRec; ++i)
    g_Vertex[i].item = (g_Data + i);
  g_Match = MALLOC(int, g_NbRow_LocRec);
    //============= Main Algo
  int K0 = 1,
    Km = 1;

  while (1)
  {
    if (Match == e_Match_Kneib)
    {
      CleanDeleteT(g_AdjType);
      g_AdjType = MALLOC(adj_type, 2 * ((long)g_NbRow_LocRec) * K_Level);
      //printf("\n===> Kneib Match with nearest %d vertices\n", K_Level);
    }//else
    //printf("\n===> Complete Match\n");

    g_Diameter = diameter(g_Data, g_NbRow_LocRec);
    //printf("Diameter: %g\n", g_Diameter);
    if (Match == e_Match_Kneib)
    {
      //printf("Adjacency list (nearest %d vertices)\n",
//                    K_Level);
      make_adj(g_Vertex, g_NbRow_LocRec, K_Level, g_AdjType);
      //printf("2\n");
    }
    //printf("*** Weighted ***\n");
    if (Match == e_Match_Kneib)
      g_ShiftBound = SHIFT(UPPERBOUND);
    else
      g_ShiftBound = 0;
    //printf("Bound: %d\n", UPPERBOUND);
    if (Match == e_Match_Kneib){
      NSKneib::weighted(g_Vertex, g_NbRow_LocRec, g_Vertex + g_NbRow_LocRec);
    }else{
      NSComplete::weighted(g_Vertex, g_NbRow_LocRec, g_Vertex + g_NbRow_LocRec);
    }
    if (Match != e_Match_Kneib || !FindLowestK)
      break;
    j = 0;

    ForLoop (i, g_NbRow_LocRec)
    {
      if (g_Vertex[i].partner == NULL)
        ++j;
    }
    //printf("Nb Unmatched Vertices: %d\n", j);

    int PrevK = K_Level;

    if (j > 1)
    {
      K0 = K_Level;

      if (Km == 1)
        K_Level *= 5;
      else
        K_Level = (Km + K_Level + 1) / 2;
    }
    else
    {
      if (Km > K_Level)
        Km = K_Level;
      K_Level = (K0 + K_Level + 1) / 2;
    }
    //printf("w8\n");
    if (K_Level < K0 + 1
      || K_Level == PrevK)
    {
      //printf("\n===> Found Smallest k value = %d\n\n", K_Level);
      break;
    }
    //printf("w9\n");
  }
  //printf("after while(1) \n");
  BOOL Done = FALSE;

  //printf("XX1 \n");
  if (Match == e_Match_Kneib)
  {
    //printf("XX11 \n");
    Done = NSKneib::match_check(g_Vertex, g_NbRow_LocRec) >= 0
      && NSKneib::dual_check(g_Vertex, g_NbRow_LocRec, g_Vertex + g_NbRow_LocRec);
    //printf("XX111 \n");
  }
  else
  {
    //printf("XX22 \n");
    Done = NSComplete::match_check(g_Vertex, g_NbRow_LocRec) >= 0
      && NSComplete::dual_check(g_Vertex, g_NbRow_LocRec, g_Vertex + g_NbRow_LocRec);
    //printf("XX222 \n");
  }
  //printf("XX2 \n");
  if (Done)
  //if (1)
  {
    //printf("DONE \n");
    c2 = get_matching(g_Vertex, g_NbRow_LocRec, g_Match);
    sum_matching(g_Vertex, g_Match, c2);
    write_matching(g_Vertex, g_NbRow_LocRec, g_Match, c2, res);
  }
  //printf("XX3 \n");
    //============= Uninit_LocalRec
  Uninit_LocalRec();
  //printf("XX4 \n");
  return Rcpp::List::create(
    Rcpp::Named( "Res" ) = res
  ) ;

  END_RCPP
}
