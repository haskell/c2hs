#  Compiler Toolkit: makefile for the administration modules
#
#  Author : Manuel M. T. Chakravarty
#  Created: 24 October 1997
#
#  Version $Revision: 1.13 $ from $Date: 2002/03/12 12:39:13 $
#
#  Copyright (c) [1997..1998] Manuel M. T. Chakravarty
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
PACKAGE = base
PART    = admin
SRCS	= Common.hs Config.hs BaseVersion.hs
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
