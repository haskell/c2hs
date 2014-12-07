#include "issue46.h"

void func(oid *obj, int aval, float bval)
{
  obj->a = aval;
  obj->b = bval;
}
