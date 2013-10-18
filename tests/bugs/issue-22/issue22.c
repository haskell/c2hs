#include <string.h>
#include "issue22.h"

struct_t s;
substruct_t subs;

struct_t *foo(int n)
{
  strcpy(s.somefield, "abcdef");
  s.substruct.field = n;
  s.substruct_p = &subs;
  subs.field = n * 10;
  return &s;
}
