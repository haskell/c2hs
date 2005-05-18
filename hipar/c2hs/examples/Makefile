#  C -> Haskell Compiler: examples makefile 
#
#  Author : Manuel M. T. Chakravarty
#  Created: 20 October 1999
#
#  Version $Revision: 1.1 $ from $Date: 1999/10/20 07:39:38 $
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

#  ***************************************
#  !!! This makefile requires GNU make !!!
#  ***************************************

# package name (must be set before the include due to conditionals in 
# `common.mk') 
#
PACKAGE=c2hs
PART=examples

include ../../mk/common.mk


.PHONY: all
all:
	$(MAKE) -C libghttpHS $(MFLAGS) libghttpHS.a
