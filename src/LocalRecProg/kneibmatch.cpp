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

