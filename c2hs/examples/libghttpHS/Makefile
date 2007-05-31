#  GnomeHaskell bindings: Gnome HTTP library
#
#  Author : Manuel M. T. Chakravarty
#  Created: 8 October 1999
#
#  Copyright (c) 1999 Manuel M. T. Chakravarty
#
#  This file is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This file is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  = DOCU =====================================================================
#
#  = TODO =====================================================================
#
#  * Let configure do the dirty work; how about automake

# the commented out variants are to be used if the directory is moved out of 
# the C->HS source tree
#
GHC=ghc
#C2HS=c2hs
C2HS=../../c2hs
GHTTP_HEADER=ghttpHS.h
#HCFLAGS=`c2hs-config --cflags`
HCFLAGS=-i../../lib -fglasgow-exts

OBJS=Ghttp.o ghttpHS.o

Ghttp.hs: Ghttp.chs ghttpHS.h
	$(C2HS) $(GHTTP_HEADER) Ghttp.chs

Ghttp.o: Ghttp.hs ghttpHS.h
	$(GHC) '-#include"ghttpHS.h"' $(HCFLAGS) -c $<

ghttpHS.o: ghttpHS.h ghttpHS.c

libghttpHS.a: $(OBJS)
	-$(RM) $@
	$(AR) -crs $@ $(OBJS)
