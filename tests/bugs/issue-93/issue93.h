typedef struct {
    int y;
    int z;
} bar_t;

typedef struct {
    int x;
    bar_t bar;
} foo_t;

void mutate_foo(foo_t *foo, bar_t *bar);
