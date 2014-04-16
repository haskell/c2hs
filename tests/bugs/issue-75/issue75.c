#include "issue75.h"

CHK_TST tmpstruct;

CHK_TST *chk_make_tst(void)
{
  tmpstruct.a = 1;
  return &tmpstruct;
}
