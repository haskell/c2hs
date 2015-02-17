#include "issue38.h"

test_enum enum_test(test_enum n)
{
  switch (n) {
  case TEST_A: return TEST_B;
  case TEST_B: return TEST_C;
  case TEST_C: return TEST_A;
  }
}
