#include <stdlib.h>
#define BFSZ(ty,bits) bits
size_t size_of_s1();
size_t size_of_s2();
size_t size_of_s3();
size_t size_of_s4();

size_t align_of_s1();
size_t align_of_s2();
size_t align_of_s3();
size_t align_of_s4();

typedef struct s1 {
        int x;
        char y;
        void* z;
        } S1;
typedef struct s2 {
        int* x[5];
        int (*y)[7];
        int (*f1)(void);
        int (*f2)[11];
} S2;

typedef struct s3 {
        int a:7;
} S3;

typedef struct s4 {
  struct {
        int a : BFSZ(int,13);
        int b : BFSZ(int,13);
        int b_1: BFSZ(int,13);
        int b_2: BFSZ(int,13);
        int b_3: BFSZ(int,13);
  } f0;
  /* NOT SUPPORTED: c2hs does not allow char/short etc. as bitfield types
  struct {
        signed char c:BFSZ(signed char,4);  
        unsigned char d;
        short e:BFSZ(short,7);
        short f:BFSZ(short,7);
        short f_1:BFSZ(short,7);
        long long g;
        long long h:BFSZ(long long, 15);
  */
} S4;
