#include <stdlib.h>
#include "issue257.h"

struct bools* make_bools(bool a, bool b, bool c, bool d) {
  struct bools* bs = malloc(sizeof(struct bools));
  bs->a = a;
  bs->b = b;
  bs->c = c;
  bs->d = d;
  return bs;
}
