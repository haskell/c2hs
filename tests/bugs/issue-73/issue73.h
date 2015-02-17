typedef struct { int c; } test_struct3;
test_struct3 *make_struct3(void);
void free_struct3(test_struct3 *v);
int access_struct3(test_struct3 *);

typedef struct { int d; } test_struct4;
test_struct4 *make_struct4(void);
void free_struct4(test_struct4 *v);
int access_struct4(test_struct4 *);
