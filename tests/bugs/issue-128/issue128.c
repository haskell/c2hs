#include <stdlib.h>
#include "issue128.h"

int f1(int n, bool incr)
{
  if (incr)
    return n + 1;
  else
    return n - 1;
}

bool f2(int n)
{
  return n > 0;
}


tststruct *make_tststruct(int ain)
{
  tststruct *p = (tststruct *)malloc(sizeof(tststruct));
  p->a = ain;
  p->b = false;
}

void free_tststruct(tststruct *s)
{
  free(s);
}

void mod_tststruct(tststruct *s, int da, bool incr)
{
  if (incr)
    s->a += da;
  else
    s->a -= da;
  s->b = incr;
}
