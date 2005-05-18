#  Compiler Toolkit: makefile for the syntax-related modules
#
#  Author : Manuel M. T. Chakravarty
#  Created: 27 February 1999
#
#  Version $Revision: 1.6 $ from $Date: 1999/11/30 17:08:21 $
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
#  * As the file `Parsers.hs' uses rank-2 type variables, which are only 
#    supported as an experimental features in some Haskell systems, it is 
#    excluded from the build process in case a system (as identified by $(SYS),
#    which doesn't support the feature is used).
#

#  ***************************************
#  !!! This makefile requires GNU make !!!
#  ***************************************

# info for this part: its package, name, sources and objects files (must be 
# before the include due to conditionals etc in `common.mk')
#
PACKAGE = base
PART    = syntax
SRCS   := Lexers.hs ParserMonad.hs Parsers.hs Pretty.hs
OBJS   := $(patsubst %.hs,%.o,$(SRCS))

include ../../mk/common.mk
-include ../$(DEPEND)

# check whether we can use rank-2 type variables
#
RANK2 = $(findstring $(SYS),ghc4 nhc1)

# if we cannot use rank-2 variables, exclude the `Parsers' modules
#
ifeq ($(RANK2),)
SRCS	:= $(filter-out Parsers.hs,$(SRCS))
OBJS	:= $(filter-out Parsers.o,$(SRCS))
endif

# file-specific compiler options
#
Parsers.hs-HCFLAGS = -fglasgow-exts

# make all object files
#
objs: $(OBJS)

# make all
#
all: objs

# config comment
#
config:
ifeq ($(RANK2),)
	@echo "*** $(SYS) does *not* support rank-2 type variables; omitting \`Parsers'."
else
	@echo "*** $(SYS) supports rank-2 type variables; will be building \`Parsers'."
endif

# Parsers tests
#
.PHONY: test
test: lexers parsers

lexers: ../*/*.o\
	 tests/LexersTest.hs
	mkdir -p $(TMP)
	$(HC) -o $(TMP)/$@ $(HCFLAGS) $^
	@echo "*** call $(TMP)/$@"

parsers: ../*/*.o\
	 tests/ParsersTest.hs
	mkdir -p $(TMP)
	$(HC) -o $(TMP)/$@ $(HCFLAGS) $^
	@echo "*** call $(TMP)/$@"

# misc targets
#
.PHONY: clean cleanhi

clean:
	-$(RM) *.o
cleanhi:
	-$(RM) *.hi
