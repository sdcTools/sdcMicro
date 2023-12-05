//*                              CEntry
//* ======================================================================== *

typedef int TValue;
typedef int TEntryIndex;
typedef int TItemIndex;

//*============================ DEBUG TRUE/FALSE =========================== *
BOOL g_Debug = FALSE;
//*============================ DEBUG TRUE/FALSE =========================== *



class CEntry : public CChainedList
{
public:
  static TValue *m_pVarStack;
  static int *m_pNbMsuStack;
  static double *m_pContributionStack;
  TValue *m_pValue;
  int m_Index, *m_pNbMsu;
  double *m_pContribution;

  ushort m_Hash;
  BOOL m_Ignore;
  double m_SudaScore, m_DisSudaScore;

  CEntry *m_pNextDuplicate, *m_pNextIdenticalVar;

public:
  CEntry(void) { m_pValue = NULL; }
  void Init(int Index);
  void InitValue(void);
};


//* ======================================================================== *
//*                              CItem
//* ======================================================================== *
enum{
  es_NotIndexed = -1
};

class CItem
{
public:
  int m_NbEntry, m_NbEntryOut;
  int m_EntryCrc;
  TItemIndex m_FromItem, m_ToItem;
  TValue m_VarValue;
  CItem *m_pNext;
  uchar m_VarNum, Pad[1];
  TEntryIndex m_pEntry[1];

public:
  static CItem *New(int NbEntry)
  {
    return (CItem *) new char[sizeof(CItem) + sizeof(TEntryIndex) * (NbEntry-1)];
  }

  static CItem *New(int VarNum, TValue VarValue, int NbEntry, int NbEntryOut);
};


//* ======================================================================== *
//*                              CSudaMsu
//* ======================================================================== *
template <class T> class CList;

class CSudaMsu
{
public:
  struct SVar
  {
    TItemIndex FromItem;
  };

  int m_Entry;
  CSudaMsu *m_pNext;
  uchar m_NbVar, Pad[1];

  SVar m_Var[1];

public:
  static CSudaMsu *New(int NbVar)
  {
    return (CSudaMsu *) new char[sizeof(CSudaMsu) + sizeof(SVar) * (NbVar-1)];
  }

  static CSudaMsu *New(int NbVar, int Entry)
  {
    CSudaMsu *pMsu = New(NbVar);

    pMsu->m_NbVar = NbVar;
    pMsu->m_Entry = Entry;

    return pMsu;
  }

  static CSudaMsu *New(CSudaMsu &Other)
  {
    CSudaMsu *pMsu = New(Other.m_NbVar);

    *pMsu = Other;
    if (Other.m_NbVar > 1)
      memcpy(pMsu->m_Var + 1, Other.m_Var + 1, sizeof(pMsu->m_Var[0]) * (Other.m_NbVar - 1));
    pMsu->m_pNext = NULL;

    return pMsu;
  }

};



//* ======================================================================== *
//*                              CCorrelation
//* ======================================================================== *

class CCorrelation
{
public:
  TItemIndex m_ItemNumA, m_ItemNumB;
  int m_NbEntryOut;
  CCorrelation *m_pNext;
  TEntryIndex m_pEntryOut[1];

public:
  static CCorrelation *New(int NbEntryOut)
  {
    return (CCorrelation *) new char[sizeof(CCorrelation) + sizeof(TEntryIndex) * (NbEntryOut-1)];
  }

  static CCorrelation *New(TItemIndex ItemNumA, TItemIndex ItemNumB, int NbEntryOut)
  {
    CCorrelation *pCorr = New(NbEntryOut);

    pCorr->m_ItemNumA = ItemNumA;
    pCorr->m_ItemNumB = ItemNumB;
    pCorr->m_NbEntryOut = NbEntryOut;

    return pCorr;
  }
};


//* ======================================================================== *
//*                              CList
//* ======================================================================== *

template <class T>
class CList
{
public:
  T *m_pFirst;//, *m_pLast;
  int m_NbElement;
  T **m_pIndexed;

public:
  CList(void) { m_pFirst = NULL; m_NbElement = 0; m_pIndexed = NULL; }
  ~CList() { Empty(); }

  void Empty(void)
  {
    T *Ptr = m_pFirst;

    while (Ptr)
    {
      T *Tmp = (T *) Ptr->m_pNext;
      CleanDeleteT(Ptr);
      Ptr = Tmp;
    }
    m_pFirst = NULL;
    m_NbElement = 0;
    CleanDeleteT(m_pIndexed);
  }

  T &operator[] (uint Index)
  {
    return *m_pIndexed[Index];
  }

  void CreateIndex(void);
  void Add(T *t)
  {
    t->m_pNext = m_pFirst;
    m_pFirst = t;
    ++m_NbElement;
  }

  void Swap(int i, int j);
};

template <class T>
void CList<T> :: CreateIndex(void)
{
  CleanDeleteT(m_pIndexed);

  if (m_NbElement)
  {
    m_pIndexed = new T *[m_NbElement];

    T *t;
    int i = 0;

    for (t = m_pFirst; t; t = (T *) t->m_pNext, ++i)
      m_pIndexed[i] = t;

    //ASSERT(i == m_NbElement);
  }
}

template <class T>
void CList<T> :: Swap(int i, int j)
{
  ::Swap(m_pIndexed[i], m_pIndexed[j]); // ==> Cannot re-create Index if we remove elements later, coz it won't be sorted !
}


enum{
  es_HashListBit      = 16,
  es_HashListSize   = 1 << es_HashListBit,
  es_HashListMask   = es_HashListSize - 1,
  es_HashListHiBit    = 1 << (es_HashListBit - 1),
  es_NbMaxVar     = 1 << 8
};

CChainedList g_HashList[es_HashListSize];
int g_NbPerHash[es_HashListSize];
int g_NbMsu = 0, g_NbMsuN[es_NbMaxVar];
int g_NbVarALEX = 0,
  g_NbEntry = 0,
  g_ValueMax = 0;

CEntry *g_pEntry = NULL;
int *g_pNbMsuPerVariable, *g_pNbCorrelated;
TValue g_MissingValueALEX = -1;


double *g_ab = NULL;

Rcpp::NumericMatrix *Mat2;

//* ======================================================================== *
//*                              CEntry
//* ======================================================================== *

TValue *CEntry::m_pVarStack = NULL;
int *CEntry::m_pNbMsuStack = NULL;
double *CEntry::m_pContributionStack = NULL;

void CEntry :: Init(int Index)
{

  Rcpp::NumericMatrix Mat = *Mat2;

  m_pValue = m_pVarStack + Index * g_NbVarALEX;
  m_pNbMsu = m_pNbMsuStack + Index * g_NbVarALEX;
  m_Index = Index;
  m_pNextDuplicate = m_pNextIdenticalVar = NULL;
  m_Ignore = FALSE;
  m_pContribution = NULL;

  if (m_pContributionStack)
    m_pContribution = m_pContributionStack + Index * g_NbVarALEX;

  int j=1;
  ForLoop (j, g_NbVarALEX){
    m_pValue[j] = (TValue) Mat(Index, j);

  }

  InitValue();
}


void CEntry :: InitValue(void)
{
  int j;

  m_Hash = 0;

  ForLoop (j, g_NbVarALEX){
    int CarryFlag = m_Hash & es_HashListHiBit;
    m_Hash <<= 1;
    m_Hash += m_pValue[j];
    if (CarryFlag)
      ++m_Hash;

    m_Hash &= es_HashListMask;

    g_ValueMax = Max(g_ValueMax, m_pValue[j]);
  }
}


//* ======================================================================== *
//*                              CItem
//* ======================================================================== *

CItem *CItem :: New(int VarNum, TValue VarValue, int NbEntry, int NbEntryOut)
{
  CItem *pItem = New(NbEntry + NbEntryOut);

  pItem->m_NbEntry = NbEntry;
  pItem->m_NbEntryOut = NbEntryOut;
  pItem->m_VarNum = VarNum;
  pItem->m_VarValue = VarValue;

  return pItem;
}


int g_MaxK;


//* ======================================================================== *
//*                              AddCorrelatedMsu
//* ======================================================================== *

void AddCorrelatedMsu(CSudaMsu &Msu, CList<CSudaMsu> &MsuList,
              CList<CCorrelation> &CorrelationList, int MaxK, int FirstK = 0)
{
  if (g_Debug){
    Rprintf("AddCorrelatedMsu\n");
  }
  for (int k = FirstK; k < Msu.m_NbVar; ++k){
    CCorrelation *pCorrelated = CorrelationList.m_pFirst;

    while (pCorrelated){
      if (Msu.m_Var[k].FromItem == pCorrelated->m_ItemNumA){
        CSudaMsu *pCloneMsu = CSudaMsu::New(Msu);
        pCloneMsu->m_Var[k].FromItem = pCorrelated->m_ItemNumB;
        if (g_Debug){
          Rprintf("New MSU\n");
        }
        MsuList.Add(pCloneMsu);
        AddCorrelatedMsu(*pCloneMsu, MsuList, CorrelationList, MaxK, k + 1);
      }

      pCorrelated = (CCorrelation *) pCorrelated->m_pNext;
    }
  }
}


//* ======================================================================== *
//*                              CorrelateItem
//* ======================================================================== *

inline CCorrelation *CorrelateItem(CList<CItem> &ItemList, CList<CCorrelation> &CorrelationList,
                  int *pEntry, int NbEntry, int EntryCrc, int FromItem, int NbTotalEntry)
{
  int k;

  for (CItem *pItem = ItemList.m_pFirst; pItem; pItem = (CItem *) pItem->m_pNext){
    if (pItem->m_NbEntry != NbEntry || pItem->m_EntryCrc != EntryCrc)
      continue;

    ForLoop (k, NbEntry){
      if (pItem->m_pEntry[k] != pEntry[k])
        break;
    }

    if (k == NbEntry){   // same Entries ?
      CCorrelation *pCorrelation = CCorrelation::New(pItem->m_FromItem, FromItem, NbTotalEntry);
      CorrelationList.Add(pCorrelation);


      return pCorrelation;
    }
  }

  return NULL;
}

//* ======================================================================== *
//*                              FindMsu
//* ======================================================================== *

int *g_pEntryCache = NULL, *g_pEntryCacheOut = NULL;
int g_BiggestSize = 0, g_NbCall = 0;

void FindMsu(CList<CSudaMsu> &MsuList, CList<CCorrelation> &CorrelationList, CList<CItem> &ItemList, CList<CItem> &CorItemList,
                          int NbEntry, int MaxK, CItem *pFromItem = NULL)
{
  int i, j, k, l, m;
  if (g_Debug){
    Rprintf("BEGIN FindMsu\n");
  }
  ++g_NbCall;


    //=== Create ItemList Rank => Bubble sort by NbEntry, VarNum ( & VarValue)
  ForLoop (i, ItemList.m_NbElement - 1){
    BOOL Swapped = FALSE;

    for (j = 1; j < ItemList.m_NbElement; ++j){
      CItem &ItemI = ItemList[j-1];
      CItem &ItemJ = ItemList[j];

      if (ItemI.m_NbEntry > ItemJ.m_NbEntry){
          //|| (ItemI.m_NbEntry == ItemJ.m_NbEntry
          //  && (ItemI.m_VarNum > ItemJ.m_VarNum)))
        ItemList.Swap(j-1, j);
        Swapped = TRUE;
      }
    }
    if (!Swapped)
      break;
  }
    //=== Create Subsets
  ForLoop (i, ItemList.m_NbElement ){
    CItem &ItemI = ItemList[i];

    CList<CItem> ItemListI;
    CList<CItem> CorItemListI;
    CList<CSudaMsu> MsuListI;
    CList<CCorrelation> CorrelationListI;

      //=== Find all Entries of ItemJ that intersects with ItemI and stock them in ItemListI
    for (j = i + 1; j < ItemList.m_NbElement; ++j){
      CItem &ItemJ = ItemList[j];
      if(g_Debug){
        CItem &ItemI = ItemList[i];
        Rprintf("ItemI.m_VarNum %d ItemI.m_NbEntry %d - ItemI.m_VarValue %d \n",ItemI.m_VarNum,ItemI.m_NbEntry, ItemI.m_VarValue);
        ForLoop (k, ItemI.m_NbEntry){
          Rprintf("  %d \n",ItemI.m_pEntry[k]);
        }
        Rprintf("ItemJ.m_VarNum %d ItemJ.m_NbEntry %d - ItemJ.m_VarValue %d \n",ItemJ.m_VarNum,ItemJ.m_NbEntry, ItemJ.m_VarValue);
        ForLoop (k, ItemJ.m_NbEntry){
          Rprintf("  %d \n",ItemJ.m_pEntry[k]);
        }
      }
      if (ItemJ.m_VarNum == ItemI.m_VarNum) // Can happen only during 1st call
        continue;

      if (ItemJ.m_pEntry[ItemJ.m_NbEntry - 1] < ItemI.m_pEntry[0]
        || ItemJ.m_pEntry[0] > ItemI.m_pEntry[ItemI.m_NbEntry-1]){
        continue; // No intersection at all
      }

      int NbSameEntry = 0, NbDiffEntry = 0, EntryCrc = 0;
      l = 0;

      ForLoop (k, ItemI.m_NbEntry){
        while (ItemI.m_pEntry[k] > ItemJ.m_pEntry[l]){
          g_pEntryCacheOut[NbDiffEntry++] = ItemJ.m_pEntry[l];
          ++l;

          if (l == ItemJ.m_NbEntry)
            goto EndLoopK;
        }

        if (ItemI.m_pEntry[k] == ItemJ.m_pEntry[l]){
          g_pEntryCache[NbSameEntry] = ItemI.m_pEntry[k];
          EntryCrc += ItemI.m_pEntry[k];
          ++NbSameEntry;
          ++l;

          if (l == ItemJ.m_NbEntry)
            break;
        }
      }

    EndLoopK:

      if (NbSameEntry && NbSameEntry < ItemI.m_NbEntry){
        if (g_Debug){
          Rprintf("MaxK = %d \n", MaxK);
        }
        if (NbSameEntry == 1){   // 1-MSU ?
          if (g_Debug){
            Rprintf("New MSU-1\n");
          }
          if (MaxK < g_MaxK){  // In a Subset ?
            CSudaMsu *pNewMsu = CSudaMsu::New(2, g_pEntryCache[0]);
            pNewMsu->m_Var[0].FromItem = ItemI.m_FromItem;
            pNewMsu->m_Var[1].FromItem = ItemJ.m_FromItem;

            MsuList.Add(pNewMsu);

            if (CorrelationList.m_NbElement)
              AddCorrelatedMsu(*pNewMsu, MsuList, CorrelationList, MaxK);
          }else{              // In Main Dataset
            int n = 1;
            m = 1;

              //=== Count how many correlated MSUs we have
            CCorrelation *pCorrelated = CorrelationList.m_pFirst;
            CEntry &Entry = g_pEntry[g_pEntryCache[0]];
            int ic = 0;
            while (pCorrelated){
              if(g_Debug){
                Rprintf("pCorrelated A %d B %d- ItemI.m_FromItem %d - ItemJ.m_FromItem %d \n",
                        pCorrelated->m_ItemNumA,pCorrelated->m_ItemNumB,ItemI.m_FromItem,ItemJ.m_FromItem);
              }
              if (pCorrelated->m_ItemNumA == ItemJ.m_FromItem){
                CItem &ItemX = CorItemList[ic];
                g_pNbMsuPerVariable[ItemX.m_VarNum] += 1;
                if (Entry.m_pContribution){
                  Entry.m_pContribution[ItemX.m_VarNum] += g_ab[1];
                }
                if(g_Debug){
                  Rprintf("ItemJHit - VarNum:%d\n",ItemX.m_VarNum);
                }
                ++m;
              }else if (pCorrelated->m_ItemNumA == ItemI.m_FromItem){
                CItem &ItemX = CorItemList[ic];
                g_pNbMsuPerVariable[ItemX.m_VarNum] += 1;
                if (Entry.m_pContribution){
                  Entry.m_pContribution[ItemX.m_VarNum] += g_ab[1];
                }
                if(g_Debug){
                  Rprintf("ItemIHit - VarNum:%d\n",ItemX.m_VarNum);
                }
                ++n;
              }
              ic++;
              pCorrelated = (CCorrelation *) pCorrelated->m_pNext;
            }
            if(g_Debug){
              Rprintf("Add Number of NbMsu Per Variable Var0%d:%d - VarValue %d\n", ItemI.m_VarNum+1, m, ItemI.m_VarValue);
              Rprintf("Add Number of NbMsu Per Variable Var0%d:%d - VarValue %d\n", ItemJ.m_VarNum+1, n, ItemJ.m_VarValue);
            }
            g_pNbMsuPerVariable[ItemI.m_VarNum] += m;
            g_pNbMsuPerVariable[ItemJ.m_VarNum] += n;



            if (Entry.m_pContribution){
              Entry.m_pContribution[ItemI.m_VarNum] += m * g_ab[1];
              Entry.m_pContribution[ItemJ.m_VarNum] += n * g_ab[1];
            }
            m *= n;
            if(g_Debug){
              Rprintf("c) New MSUs %d \n",m);
            }
            g_NbMsu += m;
            g_NbMsuN[1] += m;
            Entry.m_pNbMsu[1] += m;

          }
        }else if (MaxK >= 3){ // If MaxK == 2, then MaxK - 1 == 1, so no need to process
                     // coz when MaxK == 1, we only look for 1-MSUs in the subset
          if (g_Debug){
            Rprintf("MaxK >=3 : %d \n", MaxK);
          }
          int *pEntryOut;
          int NbDiffEntryTotal = NbDiffEntry + (ItemJ.m_NbEntry - l);

          CCorrelation *pCorrelation = CorrelateItem(ItemListI, CorrelationListI, g_pEntryCache,
                                      NbSameEntry, EntryCrc, j, NbDiffEntryTotal);

          if (pCorrelation == NULL){   // Not correlated ?
            CItem *pItem = CItem::New(ItemJ.m_VarNum, ItemJ.m_VarValue,
                                  NbSameEntry, NbDiffEntryTotal);
            ItemListI.Add(pItem);

            memcpy(pItem->m_pEntry, g_pEntryCache, sizeof(pItem->m_pEntry[0]) * NbSameEntry);
            memcpy(pItem->m_pEntry + NbSameEntry, g_pEntryCacheOut,
                                      sizeof(pItem->m_pEntry[0]) * NbDiffEntry);
            pItem->m_FromItem = j;
            pItem->m_EntryCrc = EntryCrc;
            pEntryOut = pItem->m_pEntry + NbSameEntry;
          }else{
            memcpy(pCorrelation->m_pEntryOut, g_pEntryCacheOut,
                            sizeof(pCorrelation->m_pEntryOut[0]) * NbDiffEntry);
            pEntryOut = pCorrelation->m_pEntryOut;
          }

            //=== Keep a track of entries that are inside ItemJ,
            // but not inside the new Item
          pEntryOut += NbDiffEntry;
          while (l < ItemJ.m_NbEntry)
            *pEntryOut++ = ItemJ.m_pEntry[l++];
        }
      }
    }

    if (ItemListI.m_NbElement){    // Not Empty list ?
        //=== getting MSUs of MaxK-1 subset, MsuListI is a list of MaxK-1 MSUs
      ItemListI.CreateIndex();
      FindMsu(MsuListI, CorrelationListI, ItemListI, CorItemListI, ItemI.m_NbEntry, MaxK - 1, &ItemI);
    }

    if (MsuListI.m_NbElement){   // Got any MSU candidate ?
        //=== set ToItem of ItemList to Items in ItemListI
      ForLoop (j, ItemListI.m_NbElement){
        int From = ItemListI[j].m_FromItem;
        ItemList[From].m_ToItem = j;
      }

        //=== set ToItem of ItemList to Items in CorrelationListI
      CorrelationListI.CreateIndex();

      ForLoop (j, CorrelationListI.m_NbElement){
        int From = CorrelationListI[j].m_ItemNumB;
        ItemList[From].m_ToItem = -2 - j;
      }

      CSudaMsu *pMsu = MsuListI.m_pFirst;
      MsuListI.m_pFirst = NULL;

        //=== Checking for each MSU in pMsuListI if it's a MSU in the current MaxK subset
      ForLoop (j, MsuListI.m_NbElement){
        CSudaMsu &Msu = *pMsu;
        BOOL NewMsu = FALSE;

          //=== Check Entries of the Item in ItemList corresponding to Msu.Item[0]
        if (Msu.m_NbVar > 2){
          int MinEntry = -1,
            MinNbEntry=0;

          ForLoop (l, Msu.m_NbVar){
            int ToItem = ItemList[Msu.m_Var[l].FromItem].m_ToItem;
            int NbEntry;

            if (ToItem >= 0){
              CItem &Item = ItemListI[ToItem];
              NbEntry = Item.m_NbEntryOut;
            }else{
              CCorrelation &Item = CorrelationListI[-ToItem-2];
              NbEntry = Item.m_NbEntryOut;
            }

            if (MinEntry == -1 || NbEntry < MinNbEntry){
              MinEntry = l;
              MinNbEntry = NbEntry;
            }
          }

          if (MinEntry != 0)
            Swap(Msu.m_Var[0], Msu.m_Var[MinEntry]);
        }

        int ToItem = ItemList[Msu.m_Var[0].FromItem].m_ToItem;
        int NbEntry;
        int *pEntrySrc;

        if (ToItem >= 0){
          CItem &Item = ItemListI[ToItem];
          NbEntry = Item.m_NbEntryOut;
          pEntrySrc = Item.m_pEntry + Item.m_NbEntry;
        }else{
          CCorrelation &CorrItem = CorrelationListI[-ToItem-2];
          NbEntry = CorrItem.m_NbEntryOut;
          pEntrySrc = CorrItem.m_pEntryOut;
        }

          //=== looking in entries outside ItemListI
          // if MSU is in at least 1 entry there,
          // then Msu+ItemI is a MSU in the current MaxK subset
        switch (Msu.m_NbVar)
        {
          case 2:
          {
            CItem &MsuItem = ItemList[Msu.m_Var[1].FromItem];

            for (k = 0; k < NbEntry; ++k)
            {
              TValue *pVar = g_pEntry[pEntrySrc[k]].m_pValue;

              if (pVar[MsuItem.m_VarNum] == MsuItem.m_VarValue)
              {
                NewMsu = TRUE;
                if(g_Debug){
                  Rprintf("New MSU found - 2\n");
                }
                break;
              }
            }

            break;
          }

          case 3:
          {
            CItem &MsuItem1 = ItemList[Msu.m_Var[1].FromItem],
                &MsuItem2 = ItemList[Msu.m_Var[2].FromItem];

            for (k = 0; k < NbEntry; ++k){
              TValue *pVar = g_pEntry[pEntrySrc[k]].m_pValue;

              if (pVar[MsuItem1.m_VarNum] == MsuItem1.m_VarValue
                && pVar[MsuItem2.m_VarNum] == MsuItem2.m_VarValue){
                NewMsu = TRUE;
                if(g_Debug){
                  Rprintf("New MSU found - 3\n");
                }
                break;
              }
            }

            break;
          }

          default:
          {
            for (k = 0; k < NbEntry; ++k){
              TValue *pVar = g_pEntry[pEntrySrc[k]].m_pValue;

              for (l = 1; l < Msu.m_NbVar; ++l){
                CItem &MsuItem = ItemList[Msu.m_Var[l].FromItem];

                if (pVar[MsuItem.m_VarNum] != MsuItem.m_VarValue)
                  break;
              }

              if (l == Msu.m_NbVar){
                NewMsu = TRUE;
                if(g_Debug){
                  Rprintf("New MSU found - default\n");
                }
                break;
              }
            }

            break;
          }
        }

        if (NewMsu){
          if (MaxK < g_MaxK){  // In a Subset ?
            CSudaMsu *pNewMsu = CSudaMsu::New(Msu.m_NbVar + 1, Msu.m_Entry);
            pNewMsu->m_Var[0].FromItem = ItemI.m_FromItem;

              //=== Update FromItem
            ForLoop (k, Msu.m_NbVar)
              pNewMsu->m_Var[k+1].FromItem = ItemList[Msu.m_Var[k].FromItem].m_FromItem;
            MsuList.Add(pNewMsu);

            if (CorrelationList.m_NbElement)
              AddCorrelatedMsu(*pNewMsu, MsuList, CorrelationList, MaxK);
          }else{              // In Main Dataset
            m = 1;

              //=== Count how many correlated MSUs we have
            if (CorrelationList.m_NbElement){
              ForLoop (k, Msu.m_NbVar + 1){
                CCorrelation *pCorrelated = CorrelationList.m_pFirst;

                int FromItem;
                int n = 1;

                if (k == Msu.m_NbVar)
                  FromItem = ItemI.m_FromItem;
                else
                  FromItem = ItemList[Msu.m_Var[k].FromItem].m_FromItem;

                while (pCorrelated){
                  if (FromItem == pCorrelated->m_ItemNumA)
                    ++n;

                  pCorrelated = (CCorrelation *) pCorrelated->m_pNext;
                }

                m *= n;
                g_pNbCorrelated[k] = n;
              }
            }else{
              ForLoop (k, Msu.m_NbVar + 1)
                g_pNbCorrelated[k] = 1;
            }

            CEntry &Entry = g_pEntry[Msu.m_Entry];

            ForLoop (k, Msu.m_NbVar + 1){
              int VarNum = (k == Msu.m_NbVar) ? ItemI.m_VarNum
                        : ItemList[Msu.m_Var[k].FromItem].m_VarNum;
              if(g_Debug){
                Rcpp::Rcout << "Add_ Number of NbMsu Per Variable Var" << VarNum+1 << ": " << m / g_pNbCorrelated[k];
              }
              g_pNbMsuPerVariable[VarNum] += m / g_pNbCorrelated[k];

              if (Entry.m_pContribution)
                Entry.m_pContribution[VarNum] += m * g_ab[Msu.m_NbVar];
            }
            if(g_Debug){
              Rprintf("b) New MSUs %d \n",m);
            }
            g_NbMsu += m;
            g_NbMsuN[Msu.m_NbVar] += m;
            Entry.m_pNbMsu[Msu.m_NbVar] += m;
          }
        }

        CSudaMsu *pMsuNext = (CSudaMsu *) pMsu->m_pNext;
        delete[]pMsu;
        pMsu = pMsuNext;
      }
    }

  }
  if(g_Debug){
    Rprintf("Nb Msu:%d\n",g_NbMsu);
  }
  if (g_Debug){
    Rprintf("END FindMsu\n");
  }
}


//* ======================================================================== *
//*                              Suda2
//* ======================================================================== *

struct SValue
{
  int Frequency;
  TValue Value;
  CEntry *pFirst;
  SValue *m_pNext;
};

CList<CItem> *Suda2(CEntry *pAllEntry, int NbEntry)
{
  CList<CItem> *pCorItemList = NULL;
  CList<CItem> *pItemList = NULL;
  int i, j, k,
    NbCorrelated = 0;
  int NbEntryForAllItem = 0;
  CList<CSudaMsu> MsuList;
  CList<CCorrelation> CorrelationList;

  g_BiggestSize = 0;
  g_NbCall = 0;

  g_MaxK = Min((int) g_MaxK, g_NbVarALEX);

  if (g_MaxK < 3){
    Rprintf("Error: this version of Suda2 can find MSUs only in Dataset with more than 2 variables (ie: MaxK >= 3)\n");
    return NULL;
  }
  if(g_Debug){
    Rprintf("Suda2 ==> g_MaxK=%d\n",g_MaxK);
  }
  pItemList = new CList<CItem>;
  pCorItemList = new CList<CItem>;

  ForLoop (i, g_NbVarALEX){
    CList<SValue> ValueList;

    ForLoop (j, g_NbEntry){
      CEntry &Entry = g_pEntry[j];

      if (Entry.m_Ignore){   // This Entry has 2 or more other duplicates
        if(g_Debug){
          Rprintf("Suda2 ==> Ignore var %d msu %d",i+1,j+1);
        }
        continue;
      }
      SValue *pValue = ValueList.m_pFirst;

      while (pValue){
        if (pValue->Value == Entry.m_pValue[i])
          break;
        pValue = pValue->m_pNext;
      }

      if (pValue == NULL){
        pValue = new SValue;
        pValue->Frequency = 0;
        pValue->Value = Entry.m_pValue[i];
        pValue->pFirst = NULL;
        ValueList.Add(pValue);
      }

      ++pValue->Frequency;
      Entry.m_pNextIdenticalVar = pValue->pFirst;
      pValue->pFirst = &Entry;
    }

    for (SValue *pValue = ValueList.m_pFirst; pValue; pValue = pValue->m_pNext){
      CEntry *pEntry = pValue->pFirst;
      int Freq = pValue->Frequency;

      if (pValue->Value == g_MissingValueALEX)  // If missing Value, ignore item
        continue;

      if (Freq == 1){    // 1-MSU ?
        if(g_Debug){
          Rprintf("Suda2 ==> Var%d, Freq==1 Value %d\n",i+1,pValue->Value);
        }
        CSudaMsu *pMsu = CSudaMsu::New(1, pEntry->m_Index);
        pMsu->m_Var[0].FromItem = -1;
        if(g_Debug){
          Rprintf("a) New MSUs %d \n",1);
        }
        ++g_NbMsu;
        ++g_NbMsuN[0];
        ++pEntry->m_pNbMsu[0];
        if(g_Debug){
          Rprintf("Add- Number of NbMsu Per Variable Var0%d:%d \n",i+1,1);
        }
        ++g_pNbMsuPerVariable[i];
        if (pEntry->m_pContribution){
          pEntry->m_pContribution[i] += g_ab[0];
        }
      }else if (Freq && Freq != NbEntry){
        if(g_Debug){
          Rprintf("Suda2 ==> Var%d, Freq!=1 Value %d\n",i+1, pValue->Value);
        }
        CItem *pItem = CItem::New(i, pValue->Value, Freq, 0);

        pItem->m_FromItem = -pItemList->m_NbElement - 1;
        pItem->m_EntryCrc = 0;

        ForLoop (k, Freq){
          pItem->m_pEntry[Freq-1-k] = pEntry->m_Index;
          pItem->m_EntryCrc += pEntry->m_Index;
          pEntry = pEntry->m_pNextIdenticalVar;
        }

        ForLoop (k, Freq-1){
          ASSERT(pItem->m_pEntry[k] < pItem->m_pEntry[k+1]);
        }

        ASSERT(pEntry == NULL);

        CCorrelation *pCorrelation = CorrelateItem(*pItemList, CorrelationList, pItem->m_pEntry,
                                    Freq, pItem->m_EntryCrc, -1, Freq);

        if (pCorrelation != NULL){// Correlated ?
          if(g_Debug){
            Rprintf("Suda2 ==> Var%d, Delete correlated, Value %d\n",i+1, pValue->Value);
          }
          pCorItemList->Add(pItem);
          ++NbCorrelated;
        }else{
          pItemList->Add(pItem);
          NbEntryForAllItem += Freq;
          g_BiggestSize = Max(Freq, g_BiggestSize);
        }
      }
    }
  }

  if (g_Debug){
    Rprintf("Suda2 ==> Nb Total Item = %d; AvrNbEntryPerItem = %g; NbCorrelated = %d\n",
        pItemList->m_NbElement, NbEntryForAllItem / (float) (pItemList->m_NbElement ? pItemList->m_NbElement : 1), NbCorrelated);
  }

  pItemList->CreateIndex();
  pCorItemList ->CreateIndex();

  g_pEntryCache = new int[g_BiggestSize];
  g_pEntryCacheOut = new int[g_BiggestSize];


  FindMsu(MsuList, CorrelationList, *pItemList, *pCorItemList,NbEntry, g_MaxK);

  if (g_Debug){
    Rprintf("Suda2 ==> NbCall = %d\n", g_NbCall);
  }


  CleanDeleteT(g_pEntryCache);
  CleanDeleteT(g_pEntryCacheOut);

  return pItemList;
}


//* ======================================================================== *
//*                              ClearGlobalVariables
//* ======================================================================== *

void ClearGlobalVariables(void){
  ClearMem(g_NbPerHash);
  g_ValueMax = 0;

  g_NbMsu = 0;
  ClearMem(g_NbMsuN);
}

//* ======================================================================== *
//*                              Main
//* ======================================================================== *
RcppExport SEXP Suda2(SEXP data, SEXP g_MissingValueALEX_R, SEXP MaxK_R, SEXP DisFraction_R, SEXP elliot_scores)
{
  int i, j, k, l;
  float DisFraction = 0.5f;

  Rcpp::LogicalVector like_elliot = elliot_scores;

  // data
  Rcpp::NumericMatrix Mat(data);  // creates Rcpp matrix from SEXP
  Mat2 = &Mat;
  g_NbEntry = Mat.rows();
  g_NbVarALEX = Mat.cols();

  g_MissingValueALEX = Rcpp::as<int>(g_MissingValueALEX_R);
  g_MaxK = Rcpp::as<int>(MaxK_R);
  float DisFraction_tmp = Rcpp::as<float>(DisFraction_R);
  if (DisFraction_tmp != 0) DisFraction = DisFraction_tmp;

  Rcpp::NumericMatrix Res(g_NbEntry, g_NbVarALEX+2);



  if (g_NbVarALEX > es_NbMaxVar){
    Rprintf("Error: %d Variables in this Dataset (Max = %d)\n", g_NbVarALEX, es_NbMaxVar);
    return FALSE;
  }

  if (!g_NbEntry){
    Rprintf("Error: Dataset is empty !\n");
    return FALSE;
  }

  if (!g_NbVarALEX){
    Rprintf("Error: Varlist is empty !\n");
    return FALSE;
  }

  if (g_NbVarALEX <= 0){
    Rprintf("Error: Not enough variables in Varlist (need at least %d more)!\n", 1 - g_NbVarALEX);
    return FALSE;
  }

    //============================ Contribution Init
  j = g_NbEntry * g_NbVarALEX;
  CEntry::m_pContributionStack = new double[j];
  ClearMemT(CEntry::m_pContributionStack, j);

    //============================ SudaScore Init
  float *pContribution = new float[g_NbVarALEX];
  g_ab = new double[g_NbVarALEX];
  if (like_elliot[0]==true) {
    ForLoop (i, g_NbVarALEX) {
      long double result = 1.0;
      for (j = i+1; j < g_NbVarALEX; ++j) {
        result *= (g_NbVarALEX-j);
      }
      g_ab[i] = result;
    }
  } else {
    long double FactNbVar = 1.0;

    ForLoop (i, g_NbVarALEX)
      FactNbVar *= i + 1;

    ForLoop (i, g_NbVarALEX) {
      int n = (g_NbVarALEX - (i+1));

      long double a = pow(2.0, n) - 1;

      long double Factorial = 1.0;
      for (j = 2; j <= i + 1; ++j)
        Factorial *= j;

      long double FactBis = 1.0;
      for (j = 2; j <= n; ++j)
        FactBis *= j;

      long double MulI = FactNbVar;
      long double b = Factorial * FactBis / MulI;

      g_ab[i] = a * b;
    }
  }
  CleanDeleteT(pContribution);

    //============================ Inits & Hash
  g_pEntry = new CEntry[g_NbEntry];
  g_MaxK = g_NbVarALEX;

  ClearMem(g_NbPerHash);
  CEntry::m_pVarStack = new TValue[g_NbEntry * g_NbVarALEX];
  CEntry::m_pNbMsuStack = new int[g_NbEntry * g_NbVarALEX];
  ClearMemT(CEntry::m_pNbMsuStack, g_NbEntry * g_NbVarALEX);

  g_pNbMsuPerVariable = new int[g_NbVarALEX];
  g_pNbCorrelated = new int[g_NbVarALEX];
  ClearMemT(g_pNbMsuPerVariable, g_NbVarALEX);
  ClearMemT(g_pNbCorrelated, g_NbVarALEX);

  ClearGlobalVariables();

  ForLoop (i, g_NbEntry){

    CEntry &Entry = g_pEntry[i];

    Entry.Init(i);

    Entry.AddAfter(g_HashList + Entry.m_Hash);
    ++g_NbPerHash[Entry.m_Hash];
  }


    //============================ Find uniques & remove entries duplicated more than twice
  int NbUniqueTotal = 1;
  NbUniqueTotal = NbUniqueTotal - 1; // to avoid notes in CRAN checks;
  int NbUniqueInHash = 1;
  NbUniqueInHash = NbUniqueInHash - 1; // to avoid notes in CRAN checks;
  int NbEmptyHash = 1;
  NbEmptyHash = NbEmptyHash - 1; // to avoid notes in CRAN checks;
  int NbTotalDuplicate = 1;
  NbTotalDuplicate = NbTotalDuplicate - 1; // to avoid notes in CRAN checks;

  ForLoop (i, es_HashListSize){
    CChainedList &List = g_HashList[i];

    if (List.m_pNext == NULL)   // Empty List ..?
      ++NbEmptyHash;
  }

  int NbNotEmpty = 1;
  NbNotEmpty = NbNotEmpty - 1; // to avoid notes in CRAN checks;
  int NbDuplicateRemoved = 1;
  NbDuplicateRemoved = NbDuplicateRemoved - 1; // to avoid notes in CRAN checks;
  int NbDuplicatePair = 1;
  NbDuplicatePair = NbDuplicatePair - 1; // to avoid notes in CRAN checks;
  int NbProcessedEntry = 1;
  NbProcessedEntry = NbProcessedEntry - 1; // to avoid notes in CRAN checks;

  ForLoop (i, es_HashListSize){
    CChainedList &List = g_HashList[i];

    if (List.m_pNext == NULL){   // Empty List ..?
      ASSERT(!g_NbPerHash[i]);
      continue;
    }

    ++NbNotEmpty;


    CEntry *pEntry1 = (CEntry *) List.m_pNext;

    if (pEntry1->m_pNext == NULL){   // Just one in List ..?
      ++NbProcessedEntry;
      ASSERT(g_NbPerHash[i] == 1);
      ++NbUniqueTotal;
      ++NbUniqueInHash;
      continue;
    }

    for (j = 0; pEntry1; ++j){
      ASSERT(pEntry1);
      ++NbProcessedEntry;

      if (!pEntry1->m_Ignore){
        CEntry *pEntry2 = (CEntry *) pEntry1->m_pNext;//List.m_pNext;

        int NbDuplicate = 0;

        for (k = 0; pEntry2; ++k){
          ASSERT(pEntry2);

          if (!pEntry2->m_Ignore){
            ForLoop (l, g_NbVarALEX){
              if (pEntry2->m_pValue[l] != pEntry1->m_pValue[l])
                break;    // different
            }

            if (l == g_NbVarALEX){   // Not unique ?
              if (NbDuplicate){
                pEntry2->m_Ignore = TRUE;
                pEntry2->m_pNextDuplicate = pEntry1->m_pNextDuplicate;
                pEntry1->m_pNextDuplicate = pEntry2;
                ++NbDuplicateRemoved;
              }
              else
                pEntry2->m_Ignore = -1;

              ++NbDuplicate;
            }
          }

          pEntry2 = (CEntry *) pEntry2->m_pNext;
        }

        ASSERT(k + j + 1 == g_NbPerHash[i]);

        if (NbDuplicate){
          ++NbTotalDuplicate;
          if (NbDuplicate == 1)   // Ignore Triple & more for DIS Score
            NbDuplicatePair += 2;
        }
        else if (k == g_NbPerHash[i]){ // Unique !!
          ASSERT(pEntry2 == NULL);
          ++NbUniqueTotal;

        }
      }
      else if (pEntry1->m_Ignore == -1) // First Duplicate, we have to keep it,
        pEntry1->m_Ignore = FALSE;      // so Suda2 will find the duplicates as well

      pEntry1 = (CEntry *) pEntry1->m_pNext;
    }

    ASSERT(pEntry1 == NULL);
  }

  ASSERT(NbProcessedEntry == g_NbEntry);

  CList<CItem> *pItemList = Suda2(g_pEntry, g_NbEntry);
  if(g_Debug){
    Rprintf("=== NbMsu : %d\n", g_NbMsu);

    ForLoop (i, g_NbVarALEX)
    {
      Rprintf("%6d Msu%02d; ", g_NbMsuN[i], i+1);
      if (!((i+1) % 5) || i == g_NbVarALEX - 1)
        Rprintf("\n");
    }
  }
    //============================ Nb Msu Per Variable output
  ForLoop (i, g_NbVarALEX)
    g_pNbCorrelated[i] = i;

  ForLoop (i, g_NbVarALEX){    // Bubble sort the variables by highest count
    for (j = i+1; j < g_NbVarALEX; ++j){
      if (g_pNbMsuPerVariable[g_pNbCorrelated[i]] < g_pNbMsuPerVariable[g_pNbCorrelated[j]])
        Swap(g_pNbCorrelated[i], g_pNbCorrelated[j]);
    }
  }
  if(g_Debug){
    Rprintf("=== NbMsu Per Variable :\n");

    ForLoop (i, g_NbVarALEX){
      Rprintf("Var%02d: %6d; ", g_pNbCorrelated[i]+1, g_pNbMsuPerVariable[g_pNbCorrelated[i]]);

      if (!((i+1) % 5) || i == g_NbVarALEX - 1)
        Rprintf("\n");
    }
    Rprintf("=====================\n");
  }



    //============================ Dis risk
  float Dis = NbUniqueTotal * DisFraction
        / (NbUniqueTotal * DisFraction + NbDuplicatePair * (1.0f - DisFraction));


  //============================ Suda Score
  double TotalSudaScore = 0.0;
    //=== Suda Score
  ForLoop (i, g_NbEntry){
    CEntry &Entry = g_pEntry[i];
    if (Entry.m_Ignore){   // This Entry has 2 or more other duplicates
      if(g_Debug){
        Rprintf("Ignore Obs %d",i+1);
      }
      continue;
    }
      //=== Calculate SudaScore
    double SudaScore = 0.0;
    ForLoop (j, g_MaxK){
      if(g_Debug){
        Rprintf("Obs %d - Size of MSU %d - Number of Entries %d, contribution to score: %f\n",i+1,j+1,Entry.m_pNbMsu[j], g_ab[j]);
      }
      SudaScore += Entry.m_pNbMsu[j] * g_ab[j];
    }
    if(g_Debug){
      Rprintf("Obs %d - SudaScore :%f\n",i+1,SudaScore);
    }
    if (CEntry::m_pContributionStack && SudaScore){
        //=== Var Contribution per Row
      ForLoop (j, g_NbVarALEX){
        if(g_Debug){
          Rprintf("Obs %d - Var %d: Raw contribution=%f",i+1,j+1,Entry.m_pContribution[j]);
        }
        Entry.m_pContribution[j] /= SudaScore;
        if(g_Debug){
          Rprintf(", Adjusted contribution=%f\n",Entry.m_pContribution[j]);
        }
        Res(Entry.m_Index,j) = Entry.m_pContribution[j];
      }
    }
    Entry.m_SudaScore = SudaScore;
    TotalSudaScore += SudaScore;
    Res(Entry.m_Index,g_NbVarALEX) = SudaScore;
      //=== Set SudaScore for duplicate Entries
    CEntry *pNextDuplicate = Entry.m_pNextDuplicate;

    while (pNextDuplicate){
      pNextDuplicate->m_SudaScore = SudaScore;
      if (CEntry::m_pContributionStack)
        memcpy(pNextDuplicate->m_pContribution, Entry.m_pContribution,
                    sizeof(Entry.m_pContribution[0]) * g_NbVarALEX);
      Res(pNextDuplicate->m_Index,g_NbVarALEX)=SudaScore;
      pNextDuplicate = pNextDuplicate->m_pNextDuplicate;
    }
  }
  if (CEntry::m_pContributionStack){
    j = Max(g_NbVarALEX, pItemList->m_NbElement);
    double *pContribution = new double[j];
    int *pOrder = new int[j];
      //=== Global Var Contribution
    ClearMemT(pContribution, g_NbVarALEX);
    ForLoop (j, g_NbVarALEX){
      double TotalVarContribution = 0.0;
      ForLoop (i, g_NbEntry){
        CEntry &Entry = g_pEntry[i];
        if (Entry.m_Ignore)   // This Entry has 2 or more other duplicates
          continue;
        TotalVarContribution += Entry.m_pContribution[j] * Entry.m_SudaScore;
      }
      TotalVarContribution /= TotalSudaScore;
      pContribution[j] = TotalVarContribution;
      pOrder[j] = j;
    }
    ForLoop (i, g_NbVarALEX - 1){  // Bubble sort by Contribution
      for (j = i+1; j < g_NbVarALEX; ++j){
        if (pContribution[pOrder[i]] < pContribution[pOrder[j]])
          Swap(pOrder[i], pOrder[j]);
      }
    }
    ClearMemT(pContribution, pItemList->m_NbElement);
    ForLoop (i, pItemList->m_NbElement){
      CItem &Item = (*pItemList)[i];
      double TotalItemContribution = 0.0;
      ForLoop (k, Item.m_NbEntry){
        CEntry &Entry = g_pEntry[Item.m_pEntry[k]];
        TotalItemContribution += Entry.m_pContribution[Item.m_VarNum] * Entry.m_SudaScore;
      }
      TotalItemContribution /= TotalSudaScore;
      pContribution[i] = TotalItemContribution;
      pOrder[i] = i;
    }
    ForLoop (i, pItemList->m_NbElement - 1){ // Bubble sort by Contribution
      for (j = i+1; j < pItemList->m_NbElement; ++j){
        if (pContribution[pOrder[i]] < pContribution[pOrder[j]])
          Swap(pOrder[i], pOrder[j]);
      }
    }
    CleanDeleteT(pContribution);
    CleanDeleteT(pOrder);
  }
    //=== Dis Suda Score
  double Q = 1.0f + (8.0f - g_NbVarALEX) / 20.0f,
        AdjFactor = 0.0;
  ForLoop (i, g_NbEntry){
    CEntry &Entry = g_pEntry[i];
    if (Entry.m_SudaScore)
      AdjFactor += 1.0 / pow(Entry.m_SudaScore, Q);
  }
  ForLoop (i, g_NbEntry){
    CEntry &Entry = g_pEntry[i];
    if (Entry.m_SudaScore)
      Entry.m_DisSudaScore = 1.0 / (1.0 + (NbUniqueTotal / Dis - NbUniqueTotal)
                        / (pow(Entry.m_SudaScore, Q) * AdjFactor));
    else
      Entry.m_DisSudaScore = 0;
    Res(Entry.m_Index,g_NbVarALEX+1) = Entry.m_DisSudaScore;
  }

    //============================ Uninit
  CleanDeleteT(g_pEntry);
  CleanDeleteT(CEntry::m_pVarStack);
  CleanDeleteT(CEntry::m_pNbMsuStack);
  CleanDeleteT(CEntry::m_pContributionStack);
  CleanDeleteT(g_pNbMsuPerVariable);
  CleanDeleteT(g_pNbCorrelated);
  CleanDelete(pItemList);
  CleanDeleteT(g_ab);

  return Rcpp::List::create(
      Rcpp::Named( "Res" ) = Res
  ) ;
}
