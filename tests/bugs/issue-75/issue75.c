#include "issue75.h"

CHK_TST tmpstruct;

CHK_TST *chk_make_tst(void)
{
  tmpstruct.a = 1;
  tmpstruct.b = 4;
  return &tmpstruct;
}

int chk_tst(CHK_TST *s)
{
  return s->a + s->b;
}
