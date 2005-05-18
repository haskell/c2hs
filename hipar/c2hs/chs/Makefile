#  C->Haskell Compiler: makefile for the CHS modules
#
#  Author : Manuel M. T. Chakravarty
#  Derived: 13 March 1999
#
#  Version $Revision: 1.2 $ from $Date: 1999/08/18 00:13:29 $
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
#  * Originally, derived from `c2hs/c/Makefile'.
#

#  ***************************************
#  !!! This makefile requires GNU make !!!
#  ***************************************

# info for this part: its package, name, sources and objects files (must be 
# before the include due to conditionals etc in `common.mk')
#
PACKAGE = c2hs
PART    = chs
SRCS	= CHS.hs CHSLexer.hs
OBJS	= $(patsubst %.hs,%.o,$(filter %.hs,$(SRCS)))

include ../../mk/common.mk
include ../$(DEPEND)


# make all object files
#
objs: $(OBJS)

# make all
#
all: objs

# misc targets
#
.PHONY: clean cleanhi

clean:
	-$(RM) *.o
cleanhi:
	-$(RM) *.hi
