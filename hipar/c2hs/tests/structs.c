#include "structs.h"

point make_point (int x, int y)
{
  point pnt;

  pnt = (point) malloc (sizeof (*pnt));
  pnt->x = x;
  pnt->y = y;
  return pnt;
}

weird make_weird (void)
{
  weird w;

  w = (weird) malloc (sizeof (*w));
  w->b = ' ';
  w->x = -1;
  w->nested.y   = 4;
  w->nested.z   = 2;
  w->nested.pnt = make_point (100, 200);
  return w;
}

mychar *getSpacePtr (void)
{
  static char c = ' ';
  
  return &c;
}
