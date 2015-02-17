#include "issue23.h"
#include "issue23x.h"

enum hello hello_fn(int n)
{
  switch (n) {
  case 0: return H1;
  case 1: return H2;
  default: return H3;
  }
}
