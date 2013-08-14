#ifndef __DATADESC_H
#define __DATADESC_H

#include <vector>

typedef double TValue_LocalRec;
// typedef double TDist;

class CData
{
public:
	struct SAncestor
	{
		int Nb, Index;
	};

	static BOOL *m_Numerical;
	static float *m_Weight;
	static int m_NbVariable, m_NbAncestor, m_CategoryCountVar;
	static SAncestor *m_Ancestor;
	static std::vector<int> m_CategorySize;
	static TDist *m_pAllDist;
	static TValue_LocalRec *m_pMissingValueDist;

	int m_Index;
	TValue_LocalRec *m_Value;
	TDist *m_pDist;

public:
	// static int Init(int NbVariable, int argc = 0, char **argv = NULL);
	static int Init(int NbVariable, SEXP weights);
	//static int InitAncestors(int NbVariable, int argc, char **argv);
	static int InitAncestors(int NbVariable, SEXP ancestors);
	static void Uninit_LocalRec(void);

	CData(void)
	{
		m_Value = new TValue_LocalRec[m_NbVariable + m_NbAncestor + 1];
		m_pDist = NULL;
	}

	~CData() { CleanDeleteT(m_Value); }
};

int LoadData(int n, double na, CData *p, SEXP mat);

TDist dist(CData *x, CData *y);
void PrintData(CData *d);

extern TValue_LocalRec g_MissingValue_LocalRec;

enum EOutput
{
	e_Output_Range,
	e_Output_Average
};

extern EOutput g_Output;
extern BOOL g_Debug_LocalRec;

#endif	// __DATADESC_H
