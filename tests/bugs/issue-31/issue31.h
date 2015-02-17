typedef enum {
  E_1,
  E_2,
  E_3
} test_enum;

test_enum enum_test(test_enum n);

typedef struct { int a; } test_struct1;
test_struct1 *make_struct1(void);
int access_struct1(test_struct1 *);

typedef struct { int b; } test_struct2;
test_struct2 *make_struct2(void);
int access_struct2(test_struct2 *);

typedef struct { int c; } test_struct3;
test_struct3 *make_struct3(void);
int access_struct3(test_struct3 *);

typedef struct { int d; } test_struct4;
test_struct4 *make_struct4(void);
int access_struct4(test_struct4 *);
