#include "issue46.h"

void func(oid *obj, int aval, float bval)
{
  obj->a = aval;
  obj->b = bval;
}

int oid_a(oid *obj)
{
  return obj->a;
}

float oid_b(oid *obj)
{
  return obj->b;
}
