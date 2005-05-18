#  Compiler Toolkit: makefile for the error management
#
#  Author : Manuel M. T. Chakravarty
#  Created: 24 October 1997
#
#  Version $Revision: 1.6 $ from $Date: 1998/07/26 14:50:18 $
#
#  Copyright (C) [1997..1998] Manuel M. T. Chakravarty
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
PART    = errors
SRCS	= Errors.hs
OBJS	= $(patsubst %.hs,%.o,$(SRCS))

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
