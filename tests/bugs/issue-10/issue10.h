#include <stdlib.h>

size_t size_of_s1(void);
size_t size_of_s2(void);
size_t size_of_s3(void);
size_t size_of_s4(void);

typedef struct {
  int f1:1;
  int f2:1;
  int f3:1;
  int f4:1;
  int f5:1;
} S1;

typedef struct {
  int f1:4;
  int f2:3;
  int f3:1;
  int f4:8;
  int f5:1;
} S2;

typedef struct {
  int f1:1;
} S3;

typedef struct {
  unsigned int b0: 31;
  unsigned int b30: 1;
} S4;
