#include <stdbool.h>

int f1(int n, bool incr);
bool f2(int n);

typedef struct {
  int a;
  bool b;
} tststruct;

tststruct *make_tststruct(int ain);
void free_tststruct(tststruct *s);
void mod_tststruct(tststruct *s, int da, bool incr);
