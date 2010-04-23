#include "sizeof.h"
size_t size_of_s1() {
  return sizeof(struct s1);
}
size_t size_of_s2() {
  return sizeof(struct s2);
}
size_t size_of_s3() {
  return sizeof(struct s3);
}
size_t size_of_s4() {
  return sizeof(struct s4);
}

size_t align_of_s1() {
  return __alignof__(struct s1);
}
size_t align_of_s2() {
  return __alignof__(struct s2);
}
size_t align_of_s3() {
  return __alignof__(struct s3);
}
size_t align_of_s4() {
  return __alignof__(struct s4);
}
