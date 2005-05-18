#include "pointer.h"

string concat (string str1, string str2)
{
  printf ("concat doesn't do anything");
  return str1;
}

Point *make_point (int x, int y)
{
  Point *pnt;

  pnt = (Point *) malloc (sizeof (Point));
  pnt->x = x;
  pnt->x = y;

  return pnt;
}

Point *trans_point (Point *pnt, int x, int y)
{
  Point *newPnt;

  newPnt = (Point *) malloc (sizeof (Point));
  newPnt->x = pnt->x + x;
  newPnt->y = pnt->y + y;

  return newPnt;
}
