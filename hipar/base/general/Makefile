#  Compiler Toolkit: makefile for the general modules
#
#  Author : Manuel M. T. Chakravarty
#  Created: 22 October 1997
#
#  Version $Revision: 1.13 $ from $Date: 1999/11/06 14:54:16 $
#
#  Copyright (C) [1997..1999] Manuel M. T. Chakravarty
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
PART    = general
SRCS	= DLists.hs FiniteMaps.hs FNameOps.hs FileOps.hs GetOpt.hs Sets.hs\
	  UNames.hs Utils.hs
OBJS	= $(patsubst %.hs,%.o,$(SRCS))

include ../../mk/common.mk
include ../$(DEPEND)

# make all object files
#
objs: $(OBJS)

# make all
#
all: objs

# FiniteMap tests
#
.PHONY: test
test: fm doubles unordered

fm: FiniteMaps.o tests/Main.hs
	mkdir -p $(TMP)
	$(HC) -o $(TMP)/$@ $(HCFLAGS) tests/Main.hs FiniteMaps.o
	@echo "*** call $(TMP)/$@"

doubles: FiniteMaps.o tests/doubles.hs
	mkdir -p $(TMP)
	$(HC) -o $(TMP)/$@ $(HCFLAGS) tests/doubles.hs FiniteMaps.o
	@echo "*** call $(TMP)/$@"

unordered: FiniteMaps.o tests/unordered.hs
	mkdir -p $(TMP)
	$(HC) -o $(TMP)/$@ $(HCFLAGS) tests/unordered.hs FiniteMaps.o
	@echo "*** call $(TMP)/$@"

# misc targets
#
.PHONY: clean cleanhi

clean:
	-$(RM) *.o $(TMP)/fm $(TMP)/doubles $(TMP)/unordered
cleanhi:
	-$(RM) *.hi
