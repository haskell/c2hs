#include <stdio.h>
#include <stdlib.h>
#include "issue73.h"

test_struct3 *make_struct3(void)
{
  test_struct3 *tmp = (test_struct3 *)(malloc(sizeof(test_struct3)));
  tmp->c = 3;
  printf("Allocated struct3\n");
  return tmp;
}

int access_struct3(test_struct3 *s) { return s->c; }

void free_struct3(test_struct3 *s) {
  printf("Freeing struct3\n");
  free(s);
}


test_struct4 *make_struct4(void)
{
  test_struct4 *tmp = (test_struct4 *)(malloc(sizeof(test_struct4)));
  tmp->d = 4;
  printf("Allocated struct4\n");
  return tmp;
}

int access_struct4(test_struct4 *s) { return s->d; }

void free_struct4(test_struct4 *s) {
  printf("Freeing struct4\n");
  free(s);
}
