#include "issue115.h"

array_t myStruct;
int other_a[3];

array_t *get_struct(int n, int m, int o)
{
    myStruct.a[0] = n;
    myStruct.a[1] = m;
    myStruct.a[2] = o;

    other_a[0] = n + 1;
    other_a[1] = m + 1;
    other_a[2] = o + 1;

    myStruct.p = other_a;

    return &myStruct;
}
