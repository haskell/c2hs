#  C -> Haskell Compiler: makefile for the C modules
#
#  Author : Manuel M. T. Chakravarty
#  Derived: 6 March 1999
#
#  Version $Revision: 1.6 $ from $Date: 1999/10/19 04:09:22 $
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
#  * Originally, derived from `nepal/kl/Makefile'.
#

#  ***************************************
#  !!! This makefile requires GNU make !!!
#  ***************************************

# info for this part: its package, name, sources and objects files (must be 
# before the include due to conditionals etc in `common.mk')
#
PACKAGE = c2hs
PART    = c
SRCS	= C.hs CAST.hs CAttrs.hs CLexer.hs CNames.hs CParser.hs CTrav.hs
OBJS	= $(patsubst %.hs,%.o,$(filter %.hs,$(SRCS)))

include ../../mk/common.mk
include ../$(DEPEND)


# make all object files
#
objs: $(OBJS)

# make all
#
all: objs

# C modules test
#
.PHONY: test
test: c

COBJS = CLexer.o CAST.o CParser.o C2HSState.o Switches.o

c: $(COBJS) tests/Main.hs
	mkdir -p $(TMP)
	$(HC) -o $(TMP)/$@ $(HCFLAGS) -itests\
	  $(addprefix ../*/,$(COBJS)) ../../base/*/*.o\
	  tests/Main.hs
	@echo "*** call $(TMP)/$@"

# misc targets
#
.PHONY: clean cleanhi

clean:
	-$(RM) *.o $(TMP)/c
cleanhi:
	-$(RM) *.hi tests/*.hi
