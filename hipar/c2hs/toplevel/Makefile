#  C -> Haskell Compiler: makefile for the driver
#
#  Author : Manuel M. T. Chakravarty
#  Derived: 12 August 1999
#
#  Version $Revision: 1.4 $ from $Date: 2001/11/14 09:08:13 $
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
#  * Derived from `nepal/toplevel/Makefile'
#

#  ***************************************
#  !!! This makefile requires GNU make !!!
#  ***************************************

# info for this part: its package, name, sources and objects files (must be 
# before the include due to conditionals etc in `common.mk')
#
PACKAGE = c2hs
PART    = toplevel
SRCS	= C2HSConfig.hs Main.hs Version.hs\
	  c2hs_config.c
OBJS	= $(patsubst %.hs,%.o,$(filter %.hs,$(SRCS)))
ALLOBJS	= $(OBJS)\
	  $(patsubst %.c,%.o,$(filter %.c,$(SRCS)))

include ../../mk/common.mk
include ../$(DEPEND)

# Extra flags
#
# * C2HSConfig.hs needs these flags due to its foreign imports
#
C2HSConfig.hs-HCFLAGS = -fglasgow-exts '-\#include"c2hs_config.h"'

# make all object files
#
objs: $(ALLOBJS)

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
