#ifndef _STDLIB_H_
#define _STDLIB_H_

int atexit(void (*)(void));

#ifdef __BLOCKS__
int atexit_b(void (^)(void));
#endif

#endif
