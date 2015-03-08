typedef unsigned char TST_BOOL;

#if defined(__cplusplus)

/* Use the C++ compiler's bool type */
#define TST_BOOL bool

#else /* c89, c99, etc. */

/* There is no predefined bool - use our own */
#undef bool
#define bool TST_BOOL

#endif


bool tst(int n);
