#  C -> Haskell Compiler: root makefile 
#
#  Author : Manuel M. T. Chakravarty
#  Created: 27 February 1999
#
#  Version $Revision: 1.12 $ from $Date: 2001/02/19 04:33:11 $
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
#  Essentially, instantiates the generic definitions from `common.mk' and 
#  includes the Makefiles of all parts, such that cross-part dependencies still
#  allow to use the correct flags when calling the compiler.
#  The dependency information for this package is read from `.depend' if 
#  existent. 
#

#  ***************************************
#  !!! This makefile requires GNU make !!!
#  ***************************************

# package name (must be set before the include due to conditionals in 
# `common.mk') 
#
PACKAGE=c2hs
PCKVERSION=$(C2HSVERSION)

include ../mk/common.mk
-include .depend

# directories containing interface files that might be used here
#
HIDIRS=$(addprefix ../base/,$(BASEPARTS)) $(C2HSPARTS)

# Options for $(MKDEPENDHS)
#
MKDEPENDOPTS=-optdep-X$(subst $(space),:,$(addprefix ../base/,$(BASEPARTS)))

# prep targets
#
# * the target `parsers' needs to be defined
#
.PHONY: parsers
parsers:

# build targets
#
.PHONY: default objs all test c2hs libc2hs examples install

default: all
objs   : recobjs
all    : recall c2hs libc2hs c2hs-config
test   : rectest

# the main executable (collect .o files dynamically)
#
# * `Main.o' indirectly has dependencies on all required objects
#
c2hs: Main.o
	$(HC) -o $@ $(HCFLAGS) ../base/*/*.o $(addsuffix /*.o,$(PARTS))

# the support library
#
libc2hs:
	$(MAKE) -C lib $(MFLAGS) libc2hs

# the configuration oracle
#
c2hs-config: config.status c2hs-config.in
	CONFIG_FILES=$@ CONFIG_HEADERS= $(SHELL) ./config.status

# example bindings
#
examples:
	$(MAKE) -C examples $(MFLAGS) all

# installation
#
install: all
	$(INSTALL_PROGRAM) -d $(DESTDIR)$(bindir)
	$(INSTALL) -s c2hs $(DESTDIR)$(bindir)/c2hs
	$(INSTALL_PROGRAM) c2hs-config $(DESTDIR)$(bindir)/c2hs-config
	$(INSTALL) -d $(DESTDIR)$(pkglibdir)
	$(INSTALL_DATA) lib/libc2hs.a $(DESTDIR)$(pkglibdir)/libc2hs.a
	$(INSTALL) -d $(DESTDIR)$(pkgimportdir)
	@for hi in lib/*.hi; do\
	  echo $(INSTALL_DATA) $$hi\
	       $(DESTDIR)$(pkgimportdir)/`$(BASENAME) $$hi`;\
	  $(INSTALL_DATA) $$hi $(DESTDIR)$(pkgimportdir)/`$(BASENAME) $$hi`;\
	done

# auxilliary targets
#
.PHONY: depend clean cleanhi distclean

depend: gendepend
	$(MAKE) -C lib $(MFLAGS) $@
clean: recclean
	-rm -f c2hs
cleanhi: reccleanhi
distclean:
	-$(RM) config.status config.log config.cache
