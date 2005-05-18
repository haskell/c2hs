#  C->Haskell Compiler: makefile for the generator modules
#
#  Author : Manuel M. T. Chakravarty
#  Derived: 17 March 1999
#
#  Version $Revision: 1.1 $ from $Date: 1999/08/23 03:49:52 $
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
#  * Originally, derived from `c2hs/chs/Makefile'.
#

#  ***************************************
#  !!! This makefile requires GNU make !!!
#  ***************************************

# info for this part: its package, name, sources and objects files (must be 
# before the include due to conditionals etc in `common.mk')
#
PACKAGE = c2hs
PART    = gen
SRCS	= GenBind.hs
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
