struct CHK_TST {
  int a;
  int b;
};

typedef struct CHK_TST CHK_TST;

CHK_TST *chk_make_tst(void);
int chk_tst(CHK_TST *s);
