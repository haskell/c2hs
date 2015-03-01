#include <stdlib.h>

#include "issue117.h"

int coord_x(coord_t c, int dummy)
{
    return c.x;
}

coord_t *make_coord(int x, int y)
{
  coord_t *coord;
  coord = (coord_t *)malloc(sizeof(coord_t));
  coord->x = x;
  coord->y = y;
  return coord;
}

void free_coord(coord_t *coord)
{
  free(coord);
}
