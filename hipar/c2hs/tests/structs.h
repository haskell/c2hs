#ifndef __STRUCTS_H__
#define __STRUCTS_H__

typedef char bool, mychar;

typedef struct _point *point;

struct _point {
  int x, y;
};

typedef struct {
  struct _point pnt;
  int	        col;
} *cpoint;

typedef struct {
  bool b;
  int  x;
  struct {
    int   y, z;
    point pnt;
  } nested;
} *weird;

point make_point (int x, int y);

weird make_weird (void);

mychar *getSpacePtr (void);

#endif /* __STRUCTS_H__ */
