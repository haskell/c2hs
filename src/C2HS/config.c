/*  C -> Haskell Compiler: configuration query routines
 *
 *  Author : Manuel M T Chakravarty
 *  Created: 12 November 1
 *
 *  Copyright (c) [2001..2002] Manuel M T Chakravarty
 *
 *  This file is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This file is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  DESCRIPTION ---------------------------------------------------------------
 *
 *  Runtime configuration query functions
 *
 *  TODO ----------------------------------------------------------------------
 */

#include "config.h"

/* compute the direction in which bitfields are growing
 * ====================================================
 */

union bitfield_direction_union {
  unsigned int                          allbits;
  struct {
    unsigned int first_bit  : 1;
    unsigned int second_bit : 1;
  }                                     twobits;
};

int bitfield_direction ()
{
  union bitfield_direction_union v;

  /* if setting the second bit in a bitfield makes the storeage unit contain
   * the value `2', the direction of bitfields must be increasing towards the
   * MSB
   */
  v.allbits            = 0;
  v.twobits.second_bit = 1;

  return (2 == v.allbits ? 1 : -1);
}


/* use padding for overspilling bitfields?
 * =======================================
 */

union bitfield_padding_union {
  struct {
    unsigned int allbits1;
    unsigned int allbits2;
  }                                     allbits;
  struct {
    unsigned int first_bit : 1;
             int full_unit : sizeof (int) * 8;
  }                                     somebits;
};

int bitfield_padding ()
{
  union bitfield_padding_union v;

  /* test whether more than one bit of `full_unit' spills over into `allbits2'
   */
  v.allbits.allbits1   = 0;
  v.allbits.allbits2   = 0;
  v.somebits.full_unit = -1;

  return v.allbits.allbits2 == -1;
}

/* is an `int' bitfield signed?
 * ============================
 */

union bitfield_int_signed_union {
  struct {
    unsigned int first_bit  : 1;
    unsigned int second_bit : 1;
  }                                     two_single_bits;
  struct {
    int two_bits : 2;
  }                                     two_bits;
};

int bitfield_int_signed ()
{
  union bitfield_int_signed_union v;

  /* check whether a two bit field with both bits set, gives us a negative
   * number; then, `int' bitfields must be signed
   */
  v.two_single_bits.first_bit  = 1;
  v.two_single_bits.second_bit = 1;

  return v.two_bits.two_bits == -1;
}


/* alignment constraint for bitfields
 * ==================================
 */

struct bitfield_alignment_struct {
  char         start;
  unsigned int bit : 1;
  char         end;
};

int bitfield_alignment ()
{
  struct bitfield_alignment_struct v;

  return ((int) (&v.end - &v.start)) - 1;
}
