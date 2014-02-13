typedef enum {
  TEST_A,
  TEST_B,
  TEST_C,
  TEST_A_ALIAS = TEST_A,
  TEST_C_ALIAS = TEST_C
} test_enum;

test_enum enum_test(test_enum n);

