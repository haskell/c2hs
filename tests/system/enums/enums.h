#ifndef _ENUMS_H
#define _ENUMS_H

#define STOP -1

enum colour {
  red,
  green,
  blue
};

enum weird {
  NUL,
  EINS = red + 1,		/* refers to other enum */
  FIVE = 5,
  SIX,
  MINUS_ONE = STOP
};

typedef enum {
  TOP,
  BOTTOM,
  RIGHT,
  LEFT
} side;

typedef side other_side;

enum colour colourOfSide (side aside);

enum enum_net_type { NET_TYPE_TCPIP, NET_TYPE_SOCKET, NET_TYPE_NAMEDPIPE };

typedef struct st_net {
  enum enum_net_type nettype;
  int rest;
} NET;

enum enums_enums {
  ENUMS_ONE   = 1,
  ENUMS_TWO   = 2,
  ENUMS_THREE = 3
};

/* A #define enum
 */
#define THIS 1
#define THAT 2

typedef unsigned long DWORD;

/* A #define enum with casts
 */
#define C_THIS ((DWORD)0x1L)
#define C_THAT ((DWORD)0x2L)

#endif /* !_ENUMS_H */
