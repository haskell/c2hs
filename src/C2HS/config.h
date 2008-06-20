/*  C -> Haskell Compiler: configuration query header
 *
 *  Author : Manuel M T Chakravarty
 *  Created: 12 November 1
 *
 *  Copyright (c) 2001 Manuel M T Chakravarty
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
 *  Interface to the runtime configuration query functions.
 *
 *  TODO ----------------------------------------------------------------------
 */

#ifndef C2HS_CONFIG
#define C2HS_CONFIG

/* routines querying C compiler properties
 */
int bitfield_direction  ();     /* direction in which bitfields are growing */
int bitfield_padding    ();     /* use padding for overspilling bitfields?  */
int bitfield_int_signed ();     /* is an `int' bitfield signed?             */
int bitfield_alignment  ();     /* alignment constraint for bitfields       */

#endif /* C2HS_CONFIG*/
