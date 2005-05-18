#  Compiler Toolkit: makefile for the state management modules
#
#  Author : Manuel M T Chakravarty
#  Created: 22 October 1997
#
#  Version $Revision: 1.15 $ from $Date: 2002/09/06 06:33:52 $
#
#  Copyright (c) [1997..2002] Manuel M T Chakravarty
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
# NB: nhc98 seems to have problems with the import of boundsMA into DynArrays.
#     So, exclude it for the meantime.
#
PACKAGE = base
PCKNAME = ctk
PART    = state
ifeq ($(SYS),nhc1)
SRCS	= CIO.hs State.hs StateBase.hs StateTrans.hs
else
SRCS	= CIO.hs DynArrays.hs State.hs StateBase.hs StateTrans.hs
endif
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
test: state

StateOBJS = BaseVersion.o Common.o CIO.o Config.o Errors.o State.o\
	    StateBase.o StateTrans.o SysDep.o UNames.o Utils.o FileOps.o\
	    FNameOps.o

StateOBJSX= $(wildcard $(addprefix ../*/,$(StateOBJS)))

state: $(StateOBJS) tests/Main.hs
	mkdir -p $(TMP)
	$(HC) -o $(TMP)/$@ $(HCFLAGS) $(StateOBJSX) tests/Main.hs
	@echo "*** call $(TMP)/$@"

# misc targets
#
.PHONY: clean cleanhi

clean:
	-$(RM) *.o $(TMP)/state
cleanhi:
	-$(RM) *.hi
