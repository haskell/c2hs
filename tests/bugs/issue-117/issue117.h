typedef struct {
  int x;
  int y;
} coord_t;

coord_t *make_coord(int x, int y);
void free_coord(coord_t *coord);
int coord_x(coord_t c, int dummy);
