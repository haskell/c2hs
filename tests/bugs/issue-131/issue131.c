#include <stdlib.h>
#include "issue131.h"

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
