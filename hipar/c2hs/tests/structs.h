#ifndef __STRUCTS_H__
#define __STRUCTS_H__

typedef char bool, mychar;

typedef struct _point *point;

struct _point {
  int x, y;
};

typedef struct {
  struct _point pnt;
  int	        col;
} *cpoint;

typedef struct {
  bool b;
  int  x;
  struct {
    int   y, z;
    point pnt;
  } nested;
} *weird;

typedef struct ambiguousName {
  int x;
} ambiguousName;  /* same name for struct tag and type */
typedef struct ambiguousName someOtherName;

point make_point (int x, int y);

weird make_weird (void);

mychar *getSpacePtr (void);


/* bitfield functionality
 */

struct bit_struct {
  char         c1;
  unsigned int bit : 1;
  signed   int very_small_int : 3;
  char	       c2;
};

struct bit_struct *get_bit_struct();


#ifdef __GNUC__
/* this is to check c2hs's resistance to GNU extensions
 */
struct _MyStructAlign {long int x;};
struct _MyStruct
{
  int bar;
}
__attribute__ ((aligned (__alignof (struct _MyStructAlign))))
;

#endif /* __GNUC__ */


/* to test nested struct/unions (regression test)
 */
typedef struct { int type; int typ1; } FT;
typedef union { int type; FT typ1; } SDL_Event;


#endif /* __STRUCTS_H__ */
