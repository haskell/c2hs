#include "issue999.h"

array_t myStruct;

array_t *get_struct(int n, int m, int o)
{
    myStruct.a[0] = n;
    myStruct.a[1] = m;
    myStruct.a[2] = o;

    myStruct.p = myStruct.a;

    return &myStruct;
}
