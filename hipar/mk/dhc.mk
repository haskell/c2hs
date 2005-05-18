#  Distributed Haskell Compiler: global make rules
#
#  Author : Manuel M. T. Chakravarty
#  Created: 1 December 1998
#
#  Version $Revision: 1.5 $ from $Date: 1999/10/31 07:13:17 $
#
#  Copyright (c) 1998 Manuel M. T. Chakravarty
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
#  Contains the global package dependent information for the Distributed 
#  Haskell Compiler.
#
#  = TODO =====================================================================
#


# C->HS-specific information for the toplevel makefile
# ====================================================

# files lists for tar balls
#
DHCFILES=doc/dhc mk/dhc.mk dhc

# file that contains a `versnum = "x.y.z"' line
#
DHCVERSFILE =$(TOP)/dhc/toplevel/Version.hs
DHCVERSION  =$(shell $(GREP) '^versnum' $(DHCVERSFILE)\
		     | sed '-e s/versnum.* "//' '-e s/"//')

# base directory for tar balls and exclude patterns
#
DHCTARBASE=dhc
DHCTAREXCL= $(TAREXCL)

# Parser generator
#
HAPPY  = happy


# Definition of the package parts
# ===============================

# this does not include `driver', as the driver is standalone, ie, not linked 
# with the other parts
#
DHCPARTS=ents foc kc2emu kcode prims state toplevel

ifeq ($(strip $(PACKAGE)),dhc)
  PARTS  = $(DHCPARTS)
  HIDIRS:= $(addprefix ../base/, $(BASEPARTS))
endif


# Specific targets
# ================

.PHONY: dhc tar-dhc
dhc:
	$(MAKE) -C $(BUILDDIR)/dhc $(MFLAGS) all
	@echo "*** The executable is \`$(BUILDDIR)/dhc/driver/driver'."

DHCTARCMD=$(TAR) -c -z $(DHCTAREXCL) -h -f
tar-dhc:
	-ln -s . $(DHCTARBASE)-$(DHCVERSION)
	$(DHCTARCMD) $(DHCTARBASE)-$(DHCVERSION).tar.gz\
	  $(addprefix $(DHCTARBASE)-$(DHCVERSION)/,$(DHCFILES))
	-$(RM) $(DHCTARBASE)-$(DHCVERSION)
