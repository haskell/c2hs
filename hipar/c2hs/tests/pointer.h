#ifndef _POINTER_H
#define _POINTER_H

typedef char *string;

struct _Point {
  int x, y;
};

typedef struct _Point Point;

Point *make_point (int x, int y);

#endif /* !_POINTER_H */
