#  Compiler Toolkit: makefile for the graph/dependency modules
#
#  Author : Manuel M T Chakravarty
#  Created: 6 December 1999
#
#  Version $Revision: 1.2 $ from $Date: 2002/03/12 12:39:14 $
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
PACKAGE = base
PART    = graphs
SRCS	= Marks.hs
OBJS	= $(patsubst %.hs,%.o,$(SRCS))

include ../../mk/common.mk

# make all object files
#
objs: $(OBJS)

# make all
#
all: objs

# State modules test
#
.PHONY: test
test: marks

MarksOBJS = ../syms/Attributes ../admin/Common.o ../admin/BaseVersion.o \
	    ../state/CIO.o ../admin/Config.o \
	    ../errors/Errors.o ../general/FiniteMaps.o Idents.o \
	    ../state/State.o ../state/StateBase.o ../state/StateTrans.o \
	    ../sysdep/SysDep.o NameSpaces.o \
	    ../general/FileOps.o ../general/FNameOps.o ../general/UNames.o \
	    ../general/Utils.o 
ifeq ($(SYS),$(findstring $(SYS),nhc1))	# not very nice
  SymsOBJS += ../sysdep/IOExtsNHC1.o
endif

marks: $(MarksOBJS) tests/Main.hs
	mkdir -p $(TMP)
	$(HC) -o $(TMP)/$@ $(HCFLAGS) $(MarksOBJS) tests/Main.hs
	@echo "*** call $(TMP)/$@"

# misc targets
#
.PHONY: clean cleanhi

clean:
	-$(RM) *.o $(TMP)/marks
cleanhi:
	-$(RM) *.hi
