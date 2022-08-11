/*
   An Implementation of the Edmonds' weighted matching algorithm
   Dec. 1997
   Nakamura Daishin
   daishin@im.uec.ac.jp
*/
#include "edmonds.h"

TDist g_Diameter, g_ShiftBound;

TDist diameter(CData *pData, int NbRow)
{
  int i, k;
    TDist h, b;
    CData *p, *q;

    h = 0;
    for (p = pData + (i = NbRow - 1); i >= 0; --i, --p)
    {
      for (q = pData + (k = i - 1); k >= 0; --k, --q)
      {
        b = dist(p, q);
        if (b > h)
        {
          h = b;
        }
      }
    }
  return h;
}

/* push leaves */
void push_leaves(vertex_type *z, vertex_type **sp)
{
  vertex_type *b, *p;

  b = z->base;
  if (b == NULL)
  {
    push1(z, *sp);
  }
  else
  {
    p = b;
    do
    {
      push_leaves(p, sp);
      p = p->round_blossom;
    }
    while (p != b);
  }
}

/* set root blossom of ancestors */
void setroot(vertex_type *x)
{
  vertex_type *b, *p;

  b = x->base;
  if (b == NULL)
  {
  }
  else
  {
    p = b;
    do
    {
      p->root = (x->root);
      setroot(p);
      p = p->round_blossom;
    }
    while (p != b);
  }
}

/* shrink a blossom */
void shrink(vertex_type *e, vertex_type *f, vertex_type *b, vertex_type **sp,
        vertex_type **freeblossom)
{
  vertex_type *p, *q, *r, *s, *v, *w, *z;

  pop1(z, *freeblossom);
  z->base = b;
  z->root = z;
  z->parent = NULL;
  z->prev_tail = b->prev_tail;
  z->prev_head = b->prev_head;
  z->label = e_Label_Even;
  z->var = 0;

  for (p = v = e->root; p != b; p = q)
  {
    q = p->prev_head->root;
    q->parent = z;
    p->round_blossom = q;
    if (q->label == e_Label_Odd)
    {
      push_leaves(q, sp);
    }
  }

  do
  {
    v->parent = z;
    w = f->root;
    r = w->prev_tail;
    s = w->prev_head;
    w->round_blossom = v;
    w->prev_tail = f;
    w->prev_head = e;
    v->label = w->label;
    if (w->label == e_Label_Odd)
    {
      push_leaves(w, sp);
    }

    e = r;
    f = s;
    v = w;
  }
  while (v != b);

  setroot(z);
}

void changeblossom(vertex_type *, vertex_type * );

/* add an edge to matching */
void addmatch(vertex_type *x, vertex_type *y, vertex_type *e, vertex_type *f)
{
  e->partner = f;
  f->partner = e;
  changeblossom(x, e);
  changeblossom(y, f);
}

/* change base of blossom from b to x */
void changebase(vertex_type *b, vertex_type *x)
{
  int i;
  vertex_type *p, *q, *r;

  if (x->label == e_Label_Even)
  {
    p = x;
    r = b;
    i = 1;
  }
  else
  {
    p = b;
    r = x;
    i = 0;
  }

  for (; p != r; i = 1 - i, p = q)
  {
    q = p->round_blossom;
    if (i)
    {
      q->label = e_Label_Even;
    }
    else
    {
      addmatch(p, q, p->prev_tail, p->prev_head);
      q->label = e_Label_Odd;
    }
  }
}

/* change blossom */
void changeblossom(vertex_type *u, vertex_type *e)
{
  vertex_type *p, *q;

  if (u->base == NULL)
  {           /* if u is a vertex, do nothing */
  }
  else
  {
    p = e;
    do
    {
      q = p->parent;
      changebase(q->base, p);
      q->base = p;
      p = q;
    }
    while (p != u);
  }
}

/* expand a blossom */
void expand(vertex_type *z, vertex_type **freeblossom)
{
  vertex_type *p, *b;

  b = z->base;
  p = b;
  do
  {
    p->root = p;
    p->parent = NULL;
    setroot(p);
    p = p->round_blossom;
  }
  while (p != b);
  z->base = NULL;
  push1(z, *freeblossom);
}

/* expand an e_Label_Odd blossom */
vertex_type *expand_odd(vertex_type *z, vertex_type **sp, vertex_type **freeblossom)
{
  int i;
  vertex_type *b, *x, *e, *f, *p, *q, *g, *h;

  b = z->base;
  e = z->prev_tail;
  f = z->prev_head;
  expand(z, freeblossom);
  x = e->root;

  if (x->label == e_Label_Even)
  {
    for (i = 0, p = x; p != b; p = q, e = g, f = h, i = 1 - i)
    {
      q = p->round_blossom;
      g = p->prev_head;
      h = p->prev_tail;
      p->prev_tail = e;
      p->prev_head = f;

      if (i)
      {
        p->label = e_Label_Even;
        push_leaves(p, sp);
      }
      else
      {
        p->label = e_Label_Odd;
      }
    }

    do
    {
      p->label = e_Label_Unlabeled;
      p = p->round_blossom;
    }
    while (p != x);

    b->prev_tail = e;
    b->prev_head = f;
    b->label = e_Label_Odd;
  }
  else
  {
    i = 0;
    p = b;

    do
    {
      q = p->round_blossom;
      if (i)
      {
        p->label = e_Label_Even;
        push_leaves(p, sp);
      }
      else
      {
        p->label = e_Label_Odd;
      }

      p = q;
      i = 1 - i;
    }
    while (p != x);

    do
    {
      p->label = e_Label_Unlabeled;
      p = p->round_blossom;
    }
    while (p != b);

    x->prev_tail = e;
    x->prev_head = f;
    x->label = e_Label_Odd;
  }

  return b;
}

/* expand all blossoms with zero dual variable */
void expand_zero(vertex_type *v, int n, vertex_type **freeblossom)
{
  int i;
  vertex_type *p;

  for (p = v + (i = n - 1); i >= 0; --i, --p)
  {
    while (p->root != p && zerop(p->root->var))
      expand(p->root, freeblossom);
  }
}

/* augmenting(half) */
void aughalf(vertex_type *x)
{
  vertex_type *p, *q, *e, *f;

  for (p = x; (e = p->prev_tail) != NULL; p = q)
  {
    f = p->prev_head;
    q = f->root;
    if (p->label == e_Label_Even)
    {
    }
    else
    {
      addmatch(p, q, e, f);
    }
  }
}

/* augmenting */
void augment(vertex_type *x, vertex_type *y, vertex_type *e, vertex_type *f)
{
  aughalf(x);
  aughalf(y);
  if (x->label == e_Label_Even)
    addmatch(x, y, e, f);
}

/* find nearest common ancestor of v and w */
vertex_type *findcommon(vertex_type *v, vertex_type *w)
{
  int i, k;
  vertex_type *p, *q, *b;

  /* search */
  i = (v->label == e_Label_Even);
  k = (w->label == e_Label_Even);
  v->label = w->label = e_Label_Unlabeled;

  for (p = v, q = w, b = NULL; p->prev_head != NULL || q->prev_head != NULL;)
  {
    if (p->prev_head != NULL)
    {
      p = p->prev_head->root;
      if (p->label == e_Label_Unlabeled)
      {
        b = p;
        break;  /* found ! */
      }

      p->label = e_Label_Unlabeled;
    }

    if (q->prev_head != NULL)
    {
      q = q->prev_head->root;
      if (q->label == e_Label_Unlabeled)
      {
        b = q;
        break;  /* found ! */
      }

      q->label = e_Label_Unlabeled;
    }
  }

  /* recover labels */
  for (p = v; p->label == e_Label_Unlabeled; i = !i)
  {
    if (i)
      p->label = e_Label_Even;
    else
      p->label = e_Label_Odd;
    if (p->prev_head != NULL)
    {
      p = p->prev_head->root;
    }
  }

  for (p = w; p->label == e_Label_Unlabeled; k = !k)
  {
    if (k)
      p->label = e_Label_Even;
    else
      p->label = e_Label_Odd;

    if (p->prev_head != NULL)
    {
      p = p->prev_head->root;
    }
  }

  return b;
}

void new_odd(vertex_type *e, vertex_type *f, vertex_type *y, vertex_type **sp)
{
  vertex_type *p, *q, *z;

  y->prev_tail = f;
  y->prev_head = e;
  y->label = e_Label_Odd;

  for (p = y; p->base != NULL; p = p->base) {}

  q = p->partner;

  if (q != NULL)
  {
    z = q->root;
    if (z->label == e_Label_Unlabeled)
    {
      z->label = e_Label_Even;
      z->prev_tail = q;
      z->prev_head = p;
      push_leaves(z, sp);
    }
  }
}



  //=== Complete Match
#define COMPLETEMATCH
namespace NSComplete
{
  #include "EdmondsMatch-Inc.h"
}
#undef COMPLETEMATCH

  //=== Kneib Match
#define KNEIBMATCH
namespace NSKneib
{
  #include "EdmondsMatch-Inc.h"
}
#undef KNEIBMATCH

int get_matching(vertex_type *v, int n, int *m)
{
  int i, c;
  vertex_type *p, *q;

  for (c = 0, p = v + (i = n - 1); i >= 0; --i, --p)
  {
    q = p->partner;

    if (q != NULL && p < q)
    {
      *(m++) = (int)(p - v);
      *(m++) = (int)(q - v);
      ++c;
    }
  }

  return c;
}

int set_matching(vertex_type *v, int n, int *m, int c, int ub)
{
  int i, k;
  vertex_type *p, *q;

  for (p = v + (i = n - 1); i >= 0; --i, --p)
  {
    p->partner = NULL;
  }

  for (k = 0, i = 0; i < c; ++i)
  {
    p = v + *(m++);
    q = v + *(m++);

    if (dist(p->item, q->item) <= ub)
    {
      p->partner = q;
      q->partner = p;
      ++k;
    }
  }

  return k;
}

void write_matching(vertex_type *pVertex, int NbRow, int *pMatch, int c, SEXP res_mat)
{
  int i, p, q;
 // printf("# of unmatched vertices: %d\n", NbRow - 2 * c);
  Rcpp::NumericMatrix res(res_mat);
    for (i = 0; i < NbRow; ++i)
    {
      if ((pVertex + i)->partner == NULL)
      {
        //printf("%d ", i);
        //PrintData((pVertex + i)->item);
        //printf("\n");
      }
    }

  int *pNbRecodingPerVariable = new int[CData::m_NbVariable],
      *pRecodingCountIndex = new int[CData::m_NbVariable];
  ClearMemT(pNbRecodingPerVariable, CData::m_NbVariable);

  for (i = 0; i < c; ++i)
  {
    p = pMatch[i*2+0];
    q = pMatch[i*2+1];

    int j, k, l;
    char Buf[128];
    int s = 0;
    CData &d1 = *pVertex[p].item,
        &d2 = *pVertex[q].item;


    ForLoop (j, 2)
    {
      CData &Out = j ? d2 : d1;

      ForLoop (k, CData::m_NbVariable)
      {
        TValue_LocalRec Value;

        if (d1.m_Value[k] != d2.m_Value[k])
        {
          TValue_LocalRec v1 = d1.m_Value[k],
              v2 = d2.m_Value[k];

          if (v1 > v2)
            Swap(v1, v2);

          if (!CData::m_Numerical[k])
          {
            int FirstAncestor = CData::m_Ancestor[k].Index;

            for (l = 0; l < CData::m_Ancestor[k].Nb; ++l)
            {
              if (d1.m_Value[FirstAncestor+l] == d2.m_Value[FirstAncestor+l])
                break;
            }

            if (l == CData::m_Ancestor[k].Nb)
              Value = g_MissingValue_LocalRec;
            else
              Value = d1.m_Value[FirstAncestor+l];

            if (Value != Out.m_Value[k])
              ++pNbRecodingPerVariable[k];
          }
          else
          {
            ++pNbRecodingPerVariable[k];

            if (g_Output == e_Output_Average)
            {
              TValue_LocalRec w1 = d1.m_Value[CData::m_CategoryCountVar],
                  w2 = d2.m_Value[CData::m_CategoryCountVar],
                  v1 = d1.m_Value[k],
                  v2 = d2.m_Value[k];

              if (v1 == g_MissingValue_LocalRec)
              {
                Value = v2;
              }
              else if (v2 == g_MissingValue_LocalRec)
              {
                Value = v1;
              }
              else
                Value = (v1 * w1 + v2 * w2) / (w1 + w2);
            }
            else  //=== Range
            {
              if (v1 == v2)
              {
                if (v1 == g_MissingValue_LocalRec)
                  Value = g_MissingValue_LocalRec;
                else
                  Value = v1;
              }
              else if (v1 == g_MissingValue_LocalRec)
                Value = v2;
              else if (v2 == g_MissingValue_LocalRec)
                Value = v1;
              else {
                Value = v1 - v2;
                s = sprintf(Buf, "%g.%g", v1, v2);
              }
            }
          }
        }
        else
          Value = d1.m_Value[k];

        // write value
        if(s) {
          res(Out.m_Index,k) = atof(Buf);
        }
        else
          res(Out.m_Index,k) = Value;
      }
    }
  }

    //============================ Nb Recoding Per Variable output
  ForLoop (i, CData::m_NbVariable)
    pRecodingCountIndex[i] = i;

  ForLoop (i, CData::m_NbVariable)    // Bubble sort the variables by highest count
  {
    for (int j = i+1; j < CData::m_NbVariable; ++j)
    {
      if (pNbRecodingPerVariable[pRecodingCountIndex[i]] < pNbRecodingPerVariable[pRecodingCountIndex[j]])
        Swap(pRecodingCountIndex[i], pRecodingCountIndex[j]);
    }
  }


  //printf("Nb Recoding Per Variable :\n");

  //ForLoop (i, CData::m_NbVariable)
  //{
    //printf("Var%02d: %6d; ", pRecodingCountIndex[i]+1, pNbRecodingPerVariable[pRecodingCountIndex[i]]);

    //if (!((i+1) % 5) || i == CData::m_NbVariable - 1)
    //      printf("\n");
    //}
  CleanDeleteT(pNbRecodingPerVariable);
  CleanDeleteT(pRecodingCountIndex);
}

void sum_matching(vertex_type *pVertex, int *pMatch, int c)
{
  int i, p, q, t[512];
  TDist d, h, l;

  h = -32767;
  l = 32767;
  s = 0;

  ClearMem(t);

  for (i = 0; i < c; ++i)
  {
    p = *(pMatch++);
    q = *(pMatch++);
    d = dist(pVertex[p].item, pVertex[q].item);

    if (d > h)
      h = d;

    if (d < l)
      l = d;
    s += d;

    d += 0.5;
    if (d >= 0 && d < 512)
      ++t[(int)d];
  }
  //printf("Total: %f; Max: %g; Min: %g\n", s, h, l);
  //printf("Mean: %f / %d = %f\n", s, c, s / c);
  //printf("Distribution:\n");

  //for (i = (int) l; i <= h; ++i)
  //  {
  //    if (i >= 0 && i < 512)
  //      printf("%d:%d ", i, t[i]);
  //  }

  //printf("\n");
}

void count_degree(CData *d, int n, int (*f) (CData *, CData *), int upperbound)
{
  int i, k, c, l, h, b;
  CData *p, *q;

  s = 0;
  l = 32767;
  h = -32767;

  for (p = d + (i = n - 1); i >= 0; --i, --p)
  {
    c = 0;
    for (q = d + (k = n - 1); k >= 0; --k, --q)
    {
      if (i != k)
      {
        b = f(p, q);
        if (b <= upperbound)
          ++c;
      }
    }

    s += c;
    if (l > c)
      l = c;
    if (h < c)
      h = c;
  }

}

typedef struct
{
  TDist key;
  int no;
} wn_type;

#define LT(x, y)  (h[x].key < h[y].key)
#define EX(x, y) \
  t = h[x]; \
  h[x] = h[y]; \
  h[y] = t

void heapify(wn_type *h, int n, int x)
{
  int y;
  wn_type t;

  while ((y = 2 * x + 1) < n)
  {
    if (y + 1 < n)
    {
      if (LT(y + 1, y))
        ++y;
    }

    if (!LT(y, x))
      break;
    EX(x, y);
    x = y;
  }
}

void heap_sort(wn_type *h, int n, int c)
{
  int i;
  wn_type t;

  for (i = (n - 1) / 2; i >= 0; --i)
    heapify(h, n, i);

  for (i = 1; i <= c; ++i)
  {
    EX(0, n - i);
    heapify(h, n - i, 0);
  }
}

#undef LT
#undef EX

void make_adj(vertex_type *v, int n, int c, adj_type *a)
{
  int i, k;
  TDist w;
  vertex_type *p, *q;
  wn_type *h, *x;

  h = MALLOC(wn_type, n - 1);

  for (p = v + (i = n - 1); i >= 0; --i, --p)
  {
    p->adj_list = NULL;
  }

  for (p = v + (i = n - 1); i >= 0; --i, --p)
  {
    for (x = h, q = v + (k = n - 1); k >= 0; --k, --q)
    {
      if (i != k)
      {
        x->key = dist(p->item, q->item);
        x->no = k;
        ++x;
      }
    }

    heap_sort(h, n - 1, c);

    for (x = h + ((n - 1) - 1), k = 0; k < c; ++k, --x)
    {
      q = v + (x->no);
      w = SHIFT(x->key);
      a->head_vertex = q;
      a->weight = w;
      a->next = p->adj_list;
      p->adj_list = a;
      ++a;
      a->head_vertex = p;
      a->weight = w;
      a->next = q->adj_list;
      q->adj_list = a;
      ++a;
    }
  }

  FREE(h);
}
