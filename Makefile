#  C->Haskell: root makefile 
#
#  Author : Manuel M T Chakravarty
#  Created: 24 July 1998 (derived from HiPar root makefile)
#
#  Copyright (c) [1995..2005] Manuel M T Chakravarty
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


# default target (must be first)
# ==============
# 
.PHONY: default
default:
	@echo "This is a Cabal-ised Haskell package. \
	      Configure, build, and install with"
	@echo "  \`runhaskell Setup.hs {configure,build,install}' "
	@echo "Also see the file \`INSTALL'."

TMPDIR=/tmp

# files lists
#
# * need the `wildcard' in `BASEPARTSFILES', as the following `filter-out' 
#   wouldn't work otherwise
#
BASEPARTSFILES=$(wildcard base/*/*.hs\
			  base/*/tests/Makefile base/*/tests/*.hs)
BASEFILES =AUTHORS ChangeLog COPYING COPYING.LIB INSTALL Makefile\
	   README README.CTKlight Setup.hs\
	   aclocal.m4 configure configure.in config.sub config.guess\
	   install-sh\
	   base/ChangeLog\
	   c2hs.cabal c2hs.spec\
	   base/TODO\
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
C2HSFILES =doc/c2hs/Makefile doc/c2hs/c2hs.sgml doc/c2hs/man1/*.in\
	   doc/c2hs/lib\
	   c2hs/c2hs-inplace.in\
	   $(addprefix c2hs/c/,tests/*.hs tests/*.i *.hs)\
	   $(addprefix c2hs/chs/,*.hs)\
	   $(addprefix c2hs/gen/,*.hs)\
	   $(addprefix c2hs/state/,*.hs)\
	   $(addprefix c2hs/toplevel/,C2HSConfig.hs.in Main.hs\
				      Version.hs c2hs_config.c c2hs_config.h)\
	   $(addprefix c2hs/tests/,Makefile *.chs *.h *.c)\
	   $(filter-out %/C2HSConfig.hs %/CError.hs %/NewStablePtr.hs\
			%/NewStorable.hs,\
	     $(wildcard $(addprefix c2hs/lib/,Makefile *.hs *.in)))
# FIXME: not including examples/ currently; the example has to be fixed and we
#	 need more/others

# Files containing version information
#
BASEVERSFILE= base/admin/BaseVersion.hs
CABALCONF   = c2hs.cabal

# this is far from elegant, but works for extracting the plain version number
#
BASEVERSION =$(shell $(GREP) '^versnum' $(BASEVERSFILE)\
		     | sed '-e s/versnum.* "//' '-e s/"//')
C2HSVERSION =$(shell $(GREP) '^Version' $(CABALCONF)\
		     | sed '-e s/Version:[[ 	]]*//')

# base directory for tar balls and exclude patterns
#
TARBASE=ctk
TAREXCL=--exclude='*CVS' --exclude='*~' --exclude='.\#*'\
	--exclude=config.log --exclude=config.status

C2HSTARBASE=c2hs
C2HSTAREXCL=$(TAREXCL)

# building things
# ===============
#
.PHONY: doc
doc:
	@echo "*** Building documentation..."
	@for dir in c2hs; do\
	  $(MAKE) -C doc/$$dir $(MFLAGS) all;\
	done


# installation
# ============
#
.PHONY: install-doc
install-doc:
	@echo "*** Installing documentation..."
	@for pck in c2hs; do\
	  $(MAKE) -C doc/$$pck $(MFLAGS) install;\
	done


# auxilliary targets
# ==================
#

.PHONY: clean spotless distclean tar tar-ctkl tar-c2hs

# Remove generated files
#
clean:
	./Setup.hs clean

# Remove all traces of a build
#
spotless: clean
	-$(RM) config.cache

# tar various packages
#
TARCMD=$(TAR) -c -z $(TAREXCL) -h -f
tar-ctkl: 
	@[ ! -e $(TMPDIR)/ctkl-$(BASEVERSION) ]\
	 || (echo "Temp file $(TMPDIR)/ctkl-$(BASEVERSION) already exsits."\
	     && exit 1)
	mkdir $(TMPDIR)/ctkl-$(BASEVERSION)
	$(CP) $(CTKLFILES) $(TMPDIR)/ctkl-$(BASEVERSION)
	cd $(TMPDIR); $(TARCMD) $(shell pwd)/ctkl-$(BASEVERSION)-src.tar.gz\
	  ctkl-$(BASEVERSION)
	$(RM) -r $(TMPDIR)/ctkl-$(BASEVERSION)

C2HSTARCMD=$(TAR) -c -z $(C2HSTAREXCL) -h -f
tar:
tar-c2hs:
	-ln -s . $(C2HSTARBASE)-$(C2HSVERSION)
	$(C2HSTARCMD) $(C2HSTARBASE)-$(C2HSVERSION).tar.gz\
	  $(addprefix $(C2HSTARBASE)-$(C2HSVERSION)/,$(BASEFILES) $(C2HSFILES))
	-$(RM) $(C2HSTARBASE)-$(C2HSVERSION)
