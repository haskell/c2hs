#  Compiler Toolkit: makefile for the symbols modules
#
#  Author : Manuel M T Chakravarty
#  Created: 8 January 1998
#
#  Version $Revision: 1.9 $ from $Date: 2002/05/19 09:10:17 $
#
#  Copyright (c) [1998..2002] Manuel M T Chakravarty
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
PCKNAME = ctk
PART    = syms
SRCS	= Attributes.hs Idents.hs NameSpaces.hs
OBJS	= $(patsubst %.hs,%.o,$(SRCS))

include ../../mk/common.mk
include ../$(DEPEND)

# make all object files
#
objs: $(OBJS)

# make all
#
all: objs

# State modules test
#
.PHONY: test
test: syms

SymsOBJS = Attributes.o ../admin/Common.o ../admin/BaseVersion.o \
	   ../state/CIO.o ../admin/Config.o \
	   ../errors/Errors.o ../general/FiniteMaps.o Idents.o \
	   ../state/State.o ../state/StateBase.o ../state/StateTrans.o \
	   ../sysdep/SysDep.o NameSpaces.o \
	   ../general/FileOps.o ../general/FNameOps.o ../general/UNames.o \
	   ../general/Utils.o 
ifeq ($(SYS),$(findstring $(SYS),nhc1))	# not very nice
  SymsOBJS += ../sysdep/IOExtsNHC1.o
endif

syms: $(SymsOBJS) tests/Main.hs
	mkdir -p $(TMP)
	$(HC) -o $(TMP)/$@ $(HCFLAGS) $(SymsOBJS) tests/Main.hs
	@echo "*** call $(TMP)/$@"

# misc targets
#
.PHONY: clean cleanhi

clean:
	-$(RM) *.o $(TMP)/syms
cleanhi:
	-$(RM) *.hi
