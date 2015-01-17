struct pointer_to_array {
  int (*y)[4];
} PTA;

struct array_of_pointers {
  int *y[4];
} AOP;

typedef char inner_t[32];

typedef struct {
  inner_t first;
  inner_t second;
} outer_t;

typedef struct {
  char first[32];
  char second[32];
} ok_outer_t;
