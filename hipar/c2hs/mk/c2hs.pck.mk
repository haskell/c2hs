#  C -> Haskell Compiler: global make rules
#
#  Author : Manuel M. T. Chakravarty
#  Created: 27 February 1999
#
#  Version $Revision: 1.1 $ from $Date: 2002/02/10 13:34:27 $
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
#  Contains the global package dependent information for the C -> Haskell
#  Compiler.
#
#  = TODO =====================================================================
#

# C->HS-specific information for the toplevel makefile
# ====================================================

# files lists for tar balls
#
C2HSFILES=doc/c2hs mk/c2hs.mk c2hs

# file that contain a `versnum = "x.y.z"' line
#
C2HSVERSFILE =$(TOP)/c2hs/toplevel/Version.hs
C2HSVERSION  =$(shell $(GREP) '^versnum' $(C2HSVERSFILE)\
		      | sed '-e s/versnum.* "//' '-e s/"//')

# base directory for tar balls and exclude patterns
#
C2HSTARBASE=c2hs
C2HSTAREXCL=--exclude=C2HSConfig.hs --exclude=c2hs-config --exclude=c2hs.spec\
            $(TAREXCL)


# Definition of the package parts
# ===============================

C2HSPARTS=c chs gen state toplevel

ifeq ($(strip $(PACKAGE)),c2hs)
  PARTS  = $(C2HSPARTS)
  HIDIRS:= $(addprefix ../base/, $(BASEPARTS))
endif


# Specific targets
# ================

.PHONY: c2hs tar-c2hs tar-c2hs-only
c2hs:
	$(MAKE) -C $(BUILDDIR)/c2hs $(MFLAGS) all
	@echo "*** The executable is \`$(BUILDDIR)/c2hs/c2hs'."

C2HSTARCMD=$(TAR) -c -z $(C2HSTAREXCL) -h -f
tar-c2hs:
	-ln -s . $(C2HSTARBASE)-$(C2HSVERSION)
	$(C2HSTARCMD) $(C2HSTARBASE)-$(C2HSVERSION).tar.gz\
	  $(addprefix $(C2HSTARBASE)-$(C2HSVERSION)/,$(BASEFILES) $(C2HSFILES))
	-$(RM) $(C2HSTARBASE)-$(C2HSVERSION)

tar-c2hs-only:
	-ln -s c2hs $(C2HSTARBASE)-pck-$(C2HSVERSION)
	$(C2HSTARCMD) $(C2HSTARBASE)-pck-$(C2HSVERSION).tar.gz\
	  $(addprefix $(C2HSTARBASE)-pck-$(C2HSVERSION)/,$(C2HSFILES))
	-$(RM) $(C2HSTARBASE)-pck-$(C2HSVERSION)
