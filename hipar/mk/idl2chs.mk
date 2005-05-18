#  IDL->CHS Compiler: global make rules
#
#  Authors: Manuel M. T. Chakravarty
#	    Koji Okuma
#  Created: July 2000
#
#  Version $Revision: 1.3 $ from $Date: 2000/09/05 07:54:59 $
#
#  Copyright (c) 2000 Chakravarty & Okuma
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
#  Contains the global package dependent information for the IDL->CHS
#  Compiler.
#
#  = TODO =====================================================================
#

# IDL->CHS-specific information for the toplevel makefile
# =======================================================

# files lists for tar balls
#
IDLFILES=mk/idl2chs.mk idl2chs

# we need this for pretty printing
#
ifeq ($(strip $(PACKAGE)),idl2chs)
  HCFLAGS += -syslib text
endif

# file that contain a `versnum = "x.y.z"' line
#
IDLVERSFILE =$(TOP)/idl2chs/toplevel/Version.hs
IDLVERSION  =$(shell $(GREP) '^versnum' $(IDLVERSFILE)\
		     | sed '-e s/versnum.* "//' '-e s/"//')

# base directory for tar balls and exclude patterns
#
IDLTARBASE=idl2chs
IDLTAREXCL=--exclude=IDLConfig.hs --exclude=idl2chs-config\
           --exclude=idl2chs.spec\
            $(TAREXCL)


# Definition of the package parts
# ===============================

IDLPARTS=idl state gen toplevel

ifeq ($(strip $(PACKAGE)),idl2chs)
  PARTS  = $(IDLPARTS)
  HIDIRS:= $(addprefix ../base/, $(BASEPARTS))
endif


# Specific targets
# ================

.PHONY: idl2chs tar-idl2chs tar-idl2chs-only
idl2chs:
	$(MAKE) -C $(BUILDDIR)/idl2chs $(MFLAGS) all
	@echo "*** The executable is \`$(BUILDDIR)/idl2chs/idl2chs'."

IDLTARCMD=$(TAR) -c -z $(IDLTAREXCL) -h -f
tar-idl2chs:
	-ln -s . $(IDLTARBASE)-$(IDLVERSION)
	$(IDLTARCMD) $(IDLTARBASE)-$(IDLVERSION).tar.gz\
	  $(addprefix $(IDLTARBASE)-$(IDLVERSION)/,$(BASEFILES) $(IDLFILES))
	-$(RM) $(IDLTARBASE)-$(IDLVERSION)

tar-idl2chs-only:
	-ln -s idl2chs $(IDLTARBASE)-pck-$(IDLVERSION)
	$(IDLTARCMD) $(IDLTARBASE)-pck-$(IDLVERSION).tar.gz\
	  $(addprefix $(IDLTARBASE)-pck-$(IDLVERSION)/,$(IDLFILES))
	-$(RM) $(IDLTARBASE)-pck-$(IDLVERSION)
