struct s {
  int x,y;
};

struct t {
  union r {int y; double z;};
  char *const c;
};

typedef struct {
  int x, y;
} point;

struct t x;
