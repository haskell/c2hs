#  Compiler Toolkit: root makefile 
#
#  Author : Manuel M T Chakravarty
#  Created: 25 July 1998
#
#  Version $Revision: 1.11 $ from $Date: 2002/03/12 12:39:13 $
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
#  Essentially, instantiates the generic definitions from `common.mk' and 
#  includes the Makefiles of all parts, such that cross-part dependencies still
#  allow to use the correct flags when calling the compiler.
#  The dependency information for this package is read from `.depend' if 
#  existent. 
#

#  ***************************************
#  !!! This makefile requires GNU make !!!
#  ***************************************

# package name (must be before the include due to conditionals in `common.mk')
#
PACKAGE=base
PCKVERSION=$(BASEVERSION)

include ../mk/common.mk

# compute object targets
#
# * don't include the files from sysdep/ as some are not used and those used 
#   will be demanded anyway
#
OBJS=$(patsubst %.hs,%.o,\
       $(wildcard $(addsuffix /*.hs,$(filter-out sysdep,$(BASEPARTS)))))

# directories containing interface files that might be used here
#
HIDIRS=$(BASEPARTS)

# Options for $(MKDEPENDHS)
#
# * MKDEPENDFILES are set here, because some of the .hs files in sysdep/ will
#   not be used at all
#
MKDEPENDOPTS   =
MKDEPENDFILES += $(addprefix sysdep/,SysDep.hs SysDepPosix.hs)

# build targets
#
.PHONY: default objs all libctk test

default: all
objs   : $(OBJS)
all    : objs libctk
test   : rectest

# the library achieve containing the whole CTK
#
# NB: Don't put the addsuffix expression to the to dependencies, as the 
#     wildcard won't expand to anything sensible in a clean tree.
#
libctk: libctk.a
libctk.a: objs
	-$(RM) $@
	$(AR) -crs $@ $(addsuffix /*.o,$(PARTS))

# auxilliary targets
#
.PHONY: depend clean cleanhi

depend : gendepend
clean  : recclean
cleanhi: reccleanhi
