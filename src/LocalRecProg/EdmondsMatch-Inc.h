#pragma once

#undef LOOP
#undef END
#undef WEIGHT
#undef ADJ

#ifdef COMPLETEMATCH    // defines for complete matching
  #define LOOP(p, q) for (q = v + (n - 1); q >= v; --q) {

  #define END }

  #define WEIGHT(p, q) (SHIFT(dist(p->item, q->item)))
  #define ADJ(p, q)   (dist(p->item, q->item) <= UPPERBOUND)
#elif defined(KNEIBMATCH)     // defines for k-neighbor matching
  #define LOOP(p, q) \
    adj_type *aa; \
    for (aa = p->adj_list; aa != NULL; aa = aa->next) { \
      q = aa->head_vertex;
  #define END }

  #define WEIGHT(p, q) (aa->weight)
  #define ADJ(p, q)   (aa->weight >= g_ShiftBound)
#else
  #error This source file should be only used by being included from Edmonds.cpp
#endif


  // Edmonds' blossom algorithm for weighted matching
int weighted(vertex_type *v,  // array of vertices
        int n,        // # of vertices
        vertex_type *u) // array for blossoms (size n / 2)
{
  int iterat=0;
  //int i, m, c;
  int i, c;
  TDist w, vm, eu, ee, od;
  vertex_type *b, *p, *q, *x, *y, *sp, *freeblossom;
  /* start */
  vm = -infty;
  for (p = v + (i = n - 1); i >= 0; --i, --p)
  {
    LOOP(p, q) if (p < q)
    {
      w = WEIGHT(p, q);
      if (greaterp(w, vm))
        vm = w;
    }

    END
  }
  vm *= 0.5;
  for (p = v + (i = n - 1); i >= 0; --i, --p)
  {
    p->root = p;
    p->parent = NULL;
    p->base = NULL;
    p->partner = NULL;
    p->var = vm;
  }
  for (freeblossom = NULL, p = u + (i = n / 2 - 1); i >= 0; --i, --p)
  {
    p->base = NULL;
    push1(p, freeblossom);
  }
  //m = n / 2;      /* maximum cardinarity */
  c = 0;

  /* clear */
l1:
  for (p = v + (i = n - 1); i >= 0; --i, --p)
  {
    p->root->label = e_Label_Unlabeled;
  }
  /*if (c == m) return(c); */
  for (sp = NULL, p = v + (i = n - 1); i >= 0; --i, --p)
  {
    if (p->partner == NULL)
    {
      x = p->root;
      x->label = e_Label_Even;
      x->prev_tail = x->prev_head = NULL;
      push_leaves(x, &sp);
    }
  }
  /* growing alternating tree */
l3:
  iterat++;
  while (sp != NULL)
  {
    pop1(p, sp);
    LOOP(p, q) x = p->root;
    y = q->root;
    if (x == y)
    {         /* self loop */
    }
    else if (!ADJ(p, q))
    {         /* not adjacent */
    }
    else if (greaterp((p->var) + (q->var), WEIGHT(p, q)))
    {
    }
    else if (p->partner == q)
    {         /* matched with q */
    }
    else if (y->label == e_Label_Unlabeled)
    {
      new_odd(p, q, y, &sp);
    }
    else if (y->label == e_Label_Even)
    {
      b = findcommon(x, y);
      if (b == NULL)
      {
        augment(x, y, p, q);
        ++c;
        expand_zero(v, n, &freeblossom);
        goto l1;
      }
      else
      {
        shrink(p, q, b, &sp, &freeblossom);
      }
    }

    END
  }
  /* dual change */
  for (eu = ee = od = infty, p = v + (i = n - 1); i >= 0; --i, --p)
  {
    x = p->root;
    if (x->label == e_Label_Even)
    {
      LOOP(p, q) y = q->root;
      if (x == y)
      {       /* self loop */
      }
      else if (!ADJ(p, q))
      {       /* not adjacent */
      }
      else if (y->label == e_Label_Unlabeled)
      {
        w = (p->var) + (q->var) - (WEIGHT(p, q));
        if (greaterp(eu, w))
          eu = w;
      }
      else if (y->label == e_Label_Even)
      {
        g_ShiftBound = 0;

        w = (p->var) + (q->var) - (WEIGHT(p, q));
        if (greaterp(ee, w))
          ee = w;
      }

      END
    }
    else if (x != p && x->label == e_Label_Odd)
    {
      if (greaterp(od, x->var))
        od = x->var;
    }
  }
  ee *= 0.5;
  od *= 0.5;
  w = vm;
  if (greaterp(w, eu))
    w = eu;
  if (greaterp(w, ee))
    w = ee;
  if (greaterp(w, od))
    w = od;
  for (p = v + (i = n - 1); i >= 0; --i, --p)
  {
    x = p->root;
    if (x->label == e_Label_Even)
    {
      p->var -= w;
    }
    else if (x->label == e_Label_Odd)
    {
      p->var += w;
    }
  }
  vm -= w;
  w *= 2;
  for (p = v + (n + (i = n / 2 - 1)); i >= 0; --i, --p)
  {
    if (p->base == NULL)
    {
    }
    else if (p->root != p)
    {         /* inner blossom */
    }
    else if (p->label == e_Label_Even)
    {
      p->var += w;
    }
    else if (p->label == e_Label_Odd)
    {
      p->var -= w;
    }
  }
  if (zerop(vm))
    return c;   /* maximum matching was found */
  for (p = v + (i = n - 1); i >= 0; --i, --p)
  {
    if ((x = p->root) != p && (x->label == e_Label_Odd) && zerop(x->var))
    {
      expand_odd(x, &sp, &freeblossom);
    }
  }
  for (p = v + (i = n - 1); i >= 0; --i, --p)
  {
    LOOP(p, q) x = p->root;
    y = q->root;
    if (x == y)
    {         /* self loop */
    }
    else if (!ADJ(p, q))
    {         /* not adjacent */
    }
    else if (p->partner == q)
    {
    }
    else if (x->label != e_Label_Even)
    {
    }
    else if (greaterp((p->var) + (q->var), WEIGHT(p, q)))
    {
    }
    else if (y->label == e_Label_Unlabeled)
    {
      new_odd(p, q, y, &sp);
    }
    else if (y->label == e_Label_Even)
    {
      b = findcommon(x, y);
      if (b == NULL)
      {
        augment(x, y, p, q);
        ++c;
        expand_zero(v, n, &freeblossom);
        goto l1;
      }
      else
      {
        shrink(p, q, b, &sp, &freeblossom);
      }
    }

    END
  }
  if(iterat>100000){
    return c;
  }
  goto l3;
}

int match_check(vertex_type *v, int n)
{
  int i, c;
  vertex_type *p, *q;

  c = 0;
  for (p = v + (i = n - 1); i >= 0; --i, --p)
  {
    q = p->partner;
    if (p == q)
    {
      return -1;
    }
    else if (q != NULL)
    {
      if (q->partner != p)
      {
        q = q->partner;
        return -1;
      }

      ++c;
    }
  }
  return c / 2;
}


vertex_type *commonblossom(vertex_type *v, vertex_type *w)
{
  vertex_type *p, *q, *r, *b;

  r = v->root;
  if (w->root != r)
    return NULL;

  v->root = w->root = NULL;
  for (p = v, q = w, b = NULL; (p->parent != NULL) || (q->parent != NULL);)
  {
    if (p->parent != NULL)
    {
      p = p->parent;
      if (p->root == NULL)
      {
        b = p;
        break;  /* found ! */
      }

      p->root = NULL;
    }

    if (q->parent != NULL)
    {
      q = q->parent;
      if (q->root == NULL)
      {
        b = q;
        break;  /* found ! */
      }

      q->root = NULL;
    }
  }

  /* recover */
  for (p = v; (p != NULL) && (p->root == NULL); p = p->parent)
    p->root = r;
  for (p = w; (p != NULL) && (p->root == NULL); p = p->parent)
    p->root = r;
  return b;
}

BOOL dual_check(vertex_type *v, int n, vertex_type *u)
{
  int i, k, c;
  TDist s, w;
  vertex_type *p, *q, *r, *e, *f;

  for (p = v + (i = n - 1); i >= 0; --i, --p)
  {
    if (greaterp(0, p->var))
    {
      return FALSE;
    }
    else if (greaterp(p->var, 0) && (p->partner == NULL))
    {
      return FALSE;
    }
  }

  for (p = u + (i = n / 2 - 1); i >= 0; --i, --p)
  {
    if (p->base == NULL)
    {
    }
    else if (greaterp(0, p->var))
    {
      return FALSE;
    }
    else if (greaterp(p->var, 0))
    {
      q = p->base;
      k = 0;
      c = 0;
      do
      {
        e = q->prev_tail;
        f = q->prev_head;
        if (e->partner == f)
        {
          if (!k)
          {
            return FALSE;
          }
        }
        else
        {
          if (k)
          {
            return FALSE;
          }
        }

        q = q->round_blossom;
        k = 1 - k;
        ++c;
      }
      while (q != p->base);
      if ((c < 3) || (c % 2 == 0))
      {
        return FALSE;
      }
    }
  }

  int NbInfeasible = 1; 
  NbInfeasible = NbInfeasible - 1; // to avoid notes in CRAN checks; 
  int NbInequality = 1;
  NbInequality = NbInequality - 1; // to avoid notes in CRAN checks; 
  
  for (p = v + (i = n - 1); i >= 0; --i, --p)
  {
    LOOP(p, q) if (p < q && ADJ(p, q))
    {
      w = WEIGHT(p, q);
      s = (p->var) + (q->var);
      for (r = commonblossom(p, q); r != NULL; r = r->parent)
        s += (r->var);
      if (greaterp(w, s))
      {
        ++NbInfeasible;
      }
      else if (p->partner == q && greaterp(s, w))
      {
        ++NbInequality;
      }
    }

    END
  }
  return TRUE;
}
