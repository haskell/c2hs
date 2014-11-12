#include "issue103.h"
#include <stdio.h>

void test_func(test_enum val)
{
  switch (val) {
  case E_1: printf("1\n"); return;
  case E_2: printf("2\n"); return;
  case E_3: printf("3\n"); return;
  }
}
