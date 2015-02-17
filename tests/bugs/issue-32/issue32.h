typedef struct testStruct_ testStruct;

struct testStruct_ {
  unsigned a: 27;
  unsigned b:  1;
  unsigned c: 13;
  unsigned d:  8;
};

testStruct *makeIt(void);
