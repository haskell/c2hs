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

struct bit_struct my_bit_struct;

struct bit_struct *get_bit_struct()
{
  my_bit_struct.c1             = '\0';
  my_bit_struct.bit            = 1;
  my_bit_struct.very_small_int = -1;
  my_bit_struct.c2             = '\0';

  return &my_bit_struct;
}
