#  Compiler Toolkit Toplevel: root makefile 
#
#  Author : Manuel M T Chakravarty
#  Created: 24 July 1998 (derived from HiPar root makefile)
#
#  Version $Revision: 1.66 $ from $Date: 2002/03/12 12:39:12 $
#
#  Copyright (c) [1995..2002] Manuel M T Chakravarty
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
#  * This makefile handles the toplevel commands of the Compiler Toolkit.  
#    A new compiler can be dropped in by setting up a `<mycomp>' source 
#    directory plus a `<mycomp>/mk/<mycomp>.pck.mk' makefile.  The makefile 
#    contains compiler-specific definitions for the various `make' targets and
#    the source directory contains the complete set of compiler-specific 
#    source files.
#
#    It is important that the variables and targets defined in 
#    `<mycomp>/mk/<mycomp>.pck.mk' are prefixed with a compiler-specific 
#    prefix.  This allows to have multiple compiler in a single Compiler 
#    Toolkit tree without conflicts.
#
#    The main build target and the tar target of a package are defined 
#    `<mycomp>/mk/<mycomp>.pck.mk'.
#
#  * A build is performed below `build' in a directory named by the basename of
#    the used compiler (as given by $(HC)).  In this directory, shadow 
#    directories of the source trees are created.
#
#  * Before anything can be build, a `make config' has to be executed.  
#    Whenever the compiler $(HC) or system $(SYS) used for the build changes, 
#    `make config' has to be executed again.  It also has to be re-executed
#    when files are added, deleted, or moved in a source tree.
#
#  * GNU make is required: The makefiles use conditionals and various makefile
#    functions.
#
#  = TODO =====================================================================
#

#  ***************************************
#  !!! This makefile requires GNU make !!!
#  ***************************************


# default target (must be first)
# ==============
# 
.PHONY: default
default: all

include mk/common.mk

TMPDIR=/tmp

# files lists
#
# * need the `wildcard' in `BASEPARTSFILES', as the following `filter-out' 
#   wouldn't work otherwise
#
BASEPARTSFILES=$(wildcard base/*/Makefile base/*/*.hs\
			  base/*/tests/Makefile base/*/tests/*.hs)
BASEFILES =AUTHORS COPYING COPYING.LIB INSTALL Makefile README README.CTKlight\
	   aclocal.m4 configure configure.in config.sub config.guess\
	   install-sh\
	   mk/common.mk mk/config.mk.in\
	   base/ChangeLog base/Makefile base/base.build.conf.in\
	   base/base.build.config base/TODO\
	   $(filter-out %/SysDep.hs %/SysDepPosix.hs, $(BASEPARTSFILES))\
	   doc/base/Makefile doc/base/base.tex doc/base/base.bib
CTKLFILES =AUTHORS COPYING.LIB README.CTKlight\
	   base/admin/BaseVersion.hs\
	   base/admin/Config.hs\
	   base/admin/Common.hs\
	   base/errors/Errors.hs\
	   base/general/DLists.hs\
	   base/general/FNameOps.hs\
	   base/general/FiniteMaps.hs\
	   base/general/GetOpt.hs\
	   base/general/Sets.hs\
	   base/general/Utils.hs\
	   base/syntax/Lexers.hs\
	   base/syntax/Parsers.hs\
	   base/syntax/Pretty.hs

# file that contain a `versnum = "x.y.z"' line
#
BASEVERSFILE =base/admin/BaseVersion.hs

# this is far from elegant, but works for extracting the plain version number
#
BASEVERSION =$(shell $(GREP) '^versnum' $(BASEVERSFILE)\
		     | sed '-e s/versnum.* "//' '-e s/"//')

# base directory for tar balls and exclude patterns
#
TARBASE=ctk
TAREXCL=--exclude='*CVS' --exclude='*~' --exclude='.\#*'\
	--exclude=config.log --exclude=config.status


# help target
# ===========
# 

help:
	@echo "*** Usage:"
	@echo "***   \`make prep'       -- generate parsers and compute dependencies"
	@echo "***   \`make base'       -- build Compiler Toolkit"
	@echo "***   \`make <mycomp>'   -- build compiler below <mycomp>"
	@echo "***   \`make all'        -- `prep', build, and all compilers"
	@echo "***   \`make showconfig' -- print current configuration"


# system configuration (has to be executed before building)
# ====================
#
.PHONY: config showconfig

config:
	@echo "*** Selecting system-dependent code..."
	$(MAKE) -C base/sysdep $(MFLAGS) $@
	@echo "*** Configuration successfully finished."

showconfig:
	@echo "*** Current configuration:"
	@echo "  Compiler         : $(HC)"
	@echo "  System           : $(SYS)"
	@echo "  Parser generator : $(HAPPY)"
	@echo "  Mkdepend         : $(MKDEPENDHS)"
	@echo "  Compiler packages: $(PCKS)"


# preparations (run parser generators and compute dependencies)
# ============
#
.PHONY: prep parsers depend

prep: config parsers depend

# Generate parsers
#
parsers:
	@echo "*** Checking for the need to run a parser generator..."
	@for pck in $(PCKS); do\
	  $(MAKE) -C $$pck $(MFLAGS) $@;\
	done

# Compute dependcies within each package
#
depend:
	@echo "*** Building dependency databases..."
	@for pck in base $(PCKS); do\
	  $(MAKE) -C $$pck $(MFLAGS) $@;\
	done


# building things
# ===============
#
.PHONY: all build base doc

all: prep build

build: base $(PCKS)

base:
	$(MAKE) -C base $(MFLAGS) all

doc:
	@echo "*** Building documentation..."
	@for dir in base $(PCKS); do\
	  $(MAKE) -C doc/$$dir $(MFLAGS) all;\
	done


# installation
# ============
#
.PHONY: install install-doc

install:
	@echo "*** Installing packages..."
	@for pck in $(PCKS); do\
	  $(MAKE) -C $$pck $(MFLAGS) $@;\
	done

install-doc:
	@echo "*** Installing documentation..."
	@for pck in base $(PCKS); do\
	  $(MAKE) -C doc/$$pck $(MFLAGS) install;\
	done


# auxilliary targets
# ==================
#

.PHONY: clean cleanhi spotless distclean

# Remove generated objects and executables
#
clean:
	@for pck in base $(PCKS); do\
	  $(MAKE) -C $$pck $(MFLAGS) $@;\
	  $(MAKE) -C doc/$$pck $(MFLAGS) $@;\
	done

# Remove generated interface files
#
cleanhi:
	@for pck in base $(PCKS); do\
	  $(MAKE) -C $$pck $(MFLAGS) $@;\
	done

# Remove all traces of a build
#
spotless:
	-$(RM) -rf config.cache

# Remove everything that is not in the source tar
#
distclean: spotless
	-$(RM) config.status config.log config.cache
	-$(FIND) . -name \*.in | $(SED) -e 's/\.in$$//;/\/configure$$/d'\
	         | xargs -r $(RM)

# tar various packages
#
TARCMD=$(TAR) -c -z $(TAREXCL) -h -f
tar-base:
	-ln -s . $(TARBASE)-$(BASEVERSION)
	$(TARCMD) $(TARBASE)-$(BASEVERSION).tar.gz\
	  $(addprefix $(TARBASE)-$(BASEVERSION)/,$(BASEFILES))
	-$(RM) $(TARBASE)-$(BASEVERSION)
tar-ctk: tar-base
tar-ctkl: 
	@[ ! -e $(TMPDIR)/ctkl-$(BASEVERSION) ]\
	 || (echo "Temp file $(TMPDIR)/ctkl-$(BASEVERSION) already exsits."\
	     && exit 1)
	mkdir $(TMPDIR)/ctkl-$(BASEVERSION)
	$(CP) $(CTKLFILES) $(TMPDIR)/ctkl-$(BASEVERSION)
	cd $(TMPDIR); $(TARCMD) $(shell pwd)/ctkl-$(BASEVERSION)-src.tar.gz\
	  ctkl-$(BASEVERSION)
	$(RM) -r $(TMPDIR)/ctkl-$(BASEVERSION)
