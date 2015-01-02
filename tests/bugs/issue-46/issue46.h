typedef struct {
  int a;
  float b;
  char dummy;
} oid;

void func(oid *obj, int aval, float bval);
int oid_a(oid *obj);
float oid_b(oid *obj);
