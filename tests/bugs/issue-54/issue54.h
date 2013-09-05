typedef struct {
  int c;
  double d;
} bar;

struct foo {
  int a;
  double b;
};

bar *get_bar(int n);
struct foo *get_foo(int n);
