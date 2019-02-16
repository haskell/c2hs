struct test1 {
  int a;
  struct {
    int c;
    double d;
  };
  int b;
};

struct test2 {
  int a;
  union {
    int c;
    double d;
  };
  int b;
};

struct test1* make_test1(void);
struct test2* make_test2(void);
