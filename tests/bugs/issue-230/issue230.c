#include <stdlib.h>

#include "issue230.h"

struct test1 *make_test1(void)
{
  struct test1 *t = malloc(sizeof(struct test1));
  t->a = 1;
  t->b = 2;
  t->c = 3;
  t->d = 4.0;
  return t;
}

struct test2 *make_test2(void)
{
  struct test2 *t = malloc(sizeof(struct test2));
  t->a = 5;
  t->b = 6;
  t->c = 7;
  t->d = 8.0;
  return t;
}
