#include "issue54.h"

bar b;
struct foo f;

bar *get_bar(int n)
{
  b.c = n;
  b.d = n / 10.0;
  return &b;
}

struct foo *get_foo(int n)
{
  f.a = n;
  f.b = n / 10.0;
  return &f;
}
