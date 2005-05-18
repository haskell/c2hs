#  Nepal Compiler: global make rules
#
#  Author : Manuel M. T. Chakravarty
#  Created: 1 December 1998
#
#  Version $Revision: 1.8 $ from $Date: 2000/09/05 11:46:11 $
#
#  Copyright (c) [1998..1999] Manuel M. T. Chakravarty
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
#  Contains the global package dependent information for the Nepal Compiler.
#
#  = TODO =====================================================================
#


# C->HS-specific information for the toplevel makefile
# ====================================================

# files lists for tar balls
#
NEPALFILES=doc/nepal mk/nepal.mk nepal

# file that contain a `versnum = "x.y.z"' line
#
NEPALVERSFILE=$(TOP)/nepal/toplevel/Version.hs
NEPALVERSION =$(shell $(GREP) '^versnum' $(NEPALVERSFILE)\
		      | sed '-e s/versnum.* "//' '-e s/"//')

# base directory for tar balls and exclude patterns
#
NEPALTARBASE=nepal
NEPALTAREXCL= $(TAREXCL)

# Parser generator
#
HAPPY  = happy


# Definition of the package parts
# ===============================


NEPALPARTS=dkl flat kl klemu prims state nputils npsyntax np toplevel

ifeq ($(strip $(PACKAGE)),nepal)
  PARTS  = $(NEPALPARTS)
  HIDIRS:= $(addprefix ../base/, $(BASEPARTS))
endif


# Specific targets
# ================

.PHONY: nepal tar-nepal tar-nepal-only
nepal:
	$(MAKE) -C $(BUILDDIR)/nepal $(MFLAGS) all
	@echo "*** The executable is \`$(BUILDDIR)/nepal/nepal'."

NEPALTARCMD=$(TAR) -c -z $(NEPALTAREXCL) -h -f
tar-nepal:
	-ln -s . $(NEPALTARBASE)-$(NEPALVERSION)
	$(NEPALTARCMD) $(NEPALTARBASE)-$(NEPALVERSION).tar.gz\
	  $(addprefix $(NEPALTARBASE)-$(NEPALVERSION)/,$(BASEFILES) $(NEPALFILES))
	-$(RM) $(NEPALTARBASE)-$(NEPALVERSION)

tar-nepal-only:
	-ln -s nepal $(NEPALTARBASE)-pck-$(NEPALVERSION)
	$(NEPALTARCMD) $(NEPALTARBASE)-pck-$(NEPALVERSION).tar.gz\
	  $(addprefix $(NEPALTARBASE)-pck-$(NEPALVERSION)/,$(NEPALFILES))
	-$(RM) $(NEPALTARBASE)-pck-$(NEPALVERSION)
