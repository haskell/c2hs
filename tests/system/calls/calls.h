#ifndef _CALLS_H
#define _CALLS_H

int foo ();
float bar (int);
void baz (int x, float y);
char *MyString (void);
typedef char *tString;
void printString (tString str);
void printString2 (tString);
int foobar (tString chars, int nchars, int *items, float x);

/* type of function `MyString'
 */
typedef char *(*MyStringType) (int);

#endif /* !_CALLS_H */
