#include "issue31.h"

test_enum enum_test(test_enum n)
{
  switch (n) {
  case E_1: return E_2;
  case E_2: return E_3;
  case E_3: return E_1;
  }
}

test_struct1 tmpstruct1;

test_struct1 *make_struct1(void)
{
  tmpstruct1.a = 1;
  return &tmpstruct1;
}

int access_struct1(test_struct1 *s) { return s->a; }


test_struct2 tmpstruct2;

test_struct2 *make_struct2(void)
{
  tmpstruct2.b = 2;
  return &tmpstruct2;
}

int access_struct2(test_struct2 *s) { return s->b; }


test_struct3 tmpstruct3;

test_struct3 *make_struct3(void)
{
  tmpstruct3.c = 3;
  return &tmpstruct3;
}

int access_struct3(test_struct3 *s) { return s->c; }


test_struct4 tmpstruct4;

test_struct4 *make_struct4(void)
{
  tmpstruct4.d = 4;
  return &tmpstruct4;
}

int access_struct4(test_struct4 *s) { return s->d; }
