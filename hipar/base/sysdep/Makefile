#  Compiler Toolkit: makefile for the system dependent modules
#
#  Author : Manuel M T Chakravarty
#  Created: 24 October 1997
#
#  Version $Revision: 1.16 $ from $Date: 2002/09/06 06:33:53 $
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

# we need the config info to determine which sources we have to compile
#
include ../../mk/config.mk

# info for this part: its package, name, sources and objects files (must be 
# before the include due to conditionals etc in `common.mk')
#
PACKAGE = base
PCKNAME = ctk
PART    = sysdep
SRCS	= SysDep.hs SysDepPosix.hs
#
# extra sources for nhc
#
ifeq ($(SYS),$(findstring $(SYS),nhc1))
  SRCS += IOExtsNHC1.hs
endif
#
OBJS	= $(patsubst %.hs,%.o,$(SRCS))

include ../../mk/common.mk

# In case we use a GHC4 or GHC5 system, we need some extra options for 
# `SysDep.hs'. 
#
# * we currently need `-fvia-C', as the native code generator doesn't grok 
#   some of the non-standard primitives
#
ifeq ($(SYS),$(findstring $(SYS),ghc4 ghc5))
  EXTRAHCFLAGS= -fglasgow-exts -fvia-C
endif

# extra dependencies for nhc
#
ifeq ($(SYS),$(findstring $(SYS),nhc1))
SysDep: IOExtsNHC1
endif


# make all object files
#
objs: $(OBJS)

# make all
#
all: testconfig objs


# Use appropriate system dependent modules
# ========================================
#
.PHONY: config testconfig

# name of the Haskell module containing the system dependend code for
# system $(SYS) -- "SysDep" ++ $(SYS) ++ ".hs"
#
# FIXME: Has anybody a good idea how to convert a string to all upper here
#
SYSUPPER      = $(subst nhc,NHC,$(subst ghc,GHC,$(SYS)))
SYSDEPHS      = $(addsuffix .hs,$(addprefix SysDep,$(SYSUPPER)))
AVAIL_SYSDEPHS= $(wildcard SysDep*.hs)

# if the code for the requested system is present, enable it; otherwise, error
#
# * in case of success, we also invoke a `config' target in the `syntax' part,
#   as we have to exlude some module for some systems
#
# * Then, we check for supports of HSLibs' posix package
#
# * SysDepGHC5.hs is an alias of SysDepGHC4.hs
#
config:
	@$(RM) SysDepGHC5.hs
	@$(LN) -s SysDepGHC4.hs SysDepGHC5.hs
ifeq ($(SYSDEPHS),$(findstring $(SYSDEPHS),$(AVAIL_SYSDEPHS) SysDepGHC5.hs))
	$(RM) SysDep.hs
	$(LN) -s $(SYSDEPHS) SysDep.hs
	@echo "*** Configured for compilation system $(SYS)."
	$(MAKE) -C ../syntax $(MFLAGS) $@
else
	@echo "$(SYSDEPHS) in $(AVAIL_SYSDEPHS) SysDepGHC5.hs"
	@echo "*** Unsupported Haskell compiler \`$(SYS)' specified! "
	@exit 1
endif
	$(RM) SysDepPosix.hs
ifeq (posix,$(findstring posix,$(SYSFEATURES)))
	$(LN) -s SysDepPosixAVAIL.hs SysDepPosix.hs
	@echo "*** Configured for use of HSLibs posix package."
else
	$(LN) -s SysDepPosixUNAVAIL.hs SysDepPosix.hs
	@echo "*** Configured WITHOUT HSLibs posix package."
endif

# Configured?
#
testconfig:
	@if [ ! -f SysDep.hs ]; then \
	   echo "*** Configure with \`make config' first! "; \
	   exit 1; \
	 fi
	@echo "*** Compiling with system specifier $(SYS)."


# misc targets
# ============
#
.PHONY: clean cleanhi

clean:
	-$(RM) *.o
	-$(RM) SysDepGHC5.hs SysDep.hs SysDepPosix.hs

cleanhi:
	-$(RM) *.hi
