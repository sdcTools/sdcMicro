#pragma once

#include "datadesc.h"
#include "datadesc.cpp"
//#include "../Framework.h"

#define MALLOC(t, n)		(t *)new t[n]
#define FREE(a)			CleanDeleteT(a)
#define infty				1e20f
#define es_Epsilon		1e-8f
#define UPPERBOUND		32767

#define push1(x, h)		((x)->next = (h), (h) = (x))
#define pop1(x, h)		((x) = (h), (h) = (h)->next)

extern int g_NbRow_LocRec;
extern float g_Epsilon;

//extern TDist g_MinGreater, g_MinZero;

//#define greaterp(x, y)	(x > y + es_Epsilon)

inline bool greaterp(TDist x, TDist y)
{
//	if (x)
//		g_MinGreater = Min(g_MinGreater, Abs(x));

//	if (y)
//		g_MinGreater = Min(g_MinGreater, Abs(y));
	if (y > 0.0)
		return (x / y) > 1.0 + g_Epsilon;

	if (y < 0.0)
		return (x / y) < 1.0 - g_Epsilon;

	return x > y + g_Epsilon;
}

inline BOOL zerop(TDist x)
{
//	if (x)
//		g_MinZero = Min(g_MinZero, x);
	return Abs(x) <= g_Epsilon;
}

typedef enum
{
	e_Label_Unlabeled,
	e_Label_Odd,
	e_Label_Even
} labeltyp;						/* vertex label */

typedef struct vertex
{									/* record for vertex and shrunken blossom */
	struct vertex *base;		/* base vertex of blossom (NULL if it is a vertex) */
	struct vertex *parent;	/* parent blossom (NULL if it is outermost) */
	struct vertex *root;		/* outermost blossom (itself if it is outermost) */
	struct vertex *round_blossom; /* round vertex in a blossom */
	struct vertex *next;				/* for stack */
	struct vertex *partner;			/* partner vertex (NULL if it is unmatched) */
	struct vertex *prev_tail;		/* tail of previous edge */
	struct vertex *prev_head;		/* head of previous edge */
	TDist var;					/* dual variable */
	struct adj *adj_list;	/* head of adjacency list */
	CData *item;				/* vertex number */
	labeltyp label;			/* vertex label */
} vertex_type;

typedef struct adj
{	/* record for adjacency list */
	struct adj *next;					/* next */
	struct vertex *head_vertex;	/* head vertex */
	TDist weight;						/* edge weight */
} adj_type;

#define SHIFT(w)	(((g_NbRow_LocRec / 2) * ((TDist)g_Diameter)) - (w) + 1)

void make_adj(vertex_type *v, int n, int c, adj_type *a);
int get_matching(vertex_type *v, int n, int *m);
void sum_matching(vertex_type *v, int *m, int c);
void write_matching(vertex_type *v, int n, int *m, int c, SEXP r);
TDist diameter(CData *d, int n);

	//=== Complete Match
namespace NSComplete
{
	#include "EdmondsMatch.h"
}
	//=== Kneib Match
namespace NSKneib
{
	#include "EdmondsMatch.h"
}

//#endif	// __EDMONDS_H
