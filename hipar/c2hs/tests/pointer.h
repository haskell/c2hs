#ifndef _POINTER_H
#define _POINTER_H

typedef char *string;

string concat (string str1, string str2);

struct _Point {
  int x, y;
};

struct _ColourPoint {
  int          x, y;
  unsigned int colour;
};

typedef struct _Point Point;

typedef struct _ColourPoint ColourPoint;

typedef struct _Point *PointPtr;

Point *make_point (int x, int y);

Point *trans_point (Point *pnt, int x, int y);

typedef void (*FunPtrFun) (void *data);

typedef char **stringPtr;

#endif /* !_POINTER_H */
