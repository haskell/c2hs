#include "pointer.h"

Point *make_point (int x, int y)
{
  Point *pnt;

  pnt = (Point *) malloc (sizeof (Point));
  pnt->x = x;
  pnt->x = y;

  return pnt;
}
