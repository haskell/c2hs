#ifndef _BOOLS_H
#define _BOOLS_H
#include <stdbool.h>

struct bools {
  bool a;
  bool b;
  bool c;
  bool d;
};

struct bools* make_bools(bool a, bool b, bool c, bool d);

#endif /* _BOOLS_H */
