typedef struct {
    int field;
} substruct_t;

typedef struct {
    char somefield[32];
    substruct_t substruct;
    substruct_t* substruct_p;
} struct_t;

struct_t *foo(int n);
