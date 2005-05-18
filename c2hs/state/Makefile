#  C -> Haskell Compiler: makefile for the C2HS state
#
#  Author : Manuel M T Chakravarty
#  Created: 6 March 1999
#
#  Version $Revision: 1.2 $ from $Date: 2002/03/12 12:39:16 $
#
#  Copyright (c) [1999..2002] Manuel M T Chakravarty
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

#  ***************************************
#  !!! This makefile requires GNU make !!!
#  ***************************************

# info for this part: its package, name, sources and objects files (must be 
# before the include due to conditionals etc in `common.mk')
#
PACKAGE = c2hs
PART    = state
SRCS	= C2HSState.hs Switches.hs
OBJS	= $(patsubst %.hs,%.o,$(SRCS))

include ../../mk/common.mk


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
