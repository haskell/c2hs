#ifndef _POINTER_H
#define _POINTER_H

typedef char *string;

string concat (string str1, string str2);

struct _Point {
  int x, y;
};

typedef struct _Point Point;

Point *make_point (int x, int y);

Point *trans_point (Point *pnt, int x, int y);


#endif /* !_POINTER_H */
