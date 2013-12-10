#include "issue32.h"

static testStruct makeItFrom;

testStruct *makeIt(void)
{
  makeItFrom.a = 1234;
  makeItFrom.b = 1;
  makeItFrom.c = 523;
  makeItFrom.d = 24;
  return &makeItFrom;
}
