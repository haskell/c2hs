#pragma once

typedef struct {
    int a[3]; /* An array of length 3. */
    int *p;   /* A pointer to an array. */
} array_t;

array_t *get_struct(int n, int m, int o);
