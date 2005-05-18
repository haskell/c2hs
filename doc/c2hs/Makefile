#  C->Haskell Compiler: documentation makefile
#
#  Author : Manuel M T Chakravarty
#  Created: 30 October 1999
#
#  Version $Revision: 1.9 $ from $Date: 2002/07/06 09:59:40 $
#
#  Copyright (c) [1999..2002] Manuel M T Chakravarty
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

PACKAGE    = c2hs
PCKVERSION = $(C2HSVERSION)

# must be first
#
default: all

# slurp in the common definitions (the TOP definition is not nice yet)
#
TOP=../..

include $(TOP)/mk/config.mk $(TOP)/c2hs/mk/c2hs.pck.mk

# man pages in `man1' subdirectory
#
MANPAGES:=$(patsubst man1/%.in,man1/%,$(wildcard man1/*.in))

# install dirs
#
MANDIR =$(mandir)
SGMLDIR=$(pkgdocdir)/sgml
HTMLDIR=$(pkgdocdir)/html
TXTDIR =$(pkgdocdir)/txt

# build targets
#

.PHONY: all
all: $(MANPAGES) c2hs.html c2hs.txt

c2hs.html: c2hs.sgml
	sgml2html c2hs.sgml

c2hs.txt: c2hs.sgml
	sgml2txt c2hs.sgml

$(MANPAGES): %: %.in
	sed -e "s%@DOCDIR@%$(docdir)%g;\
	        s%@VERSION@%$(C2HSVERSION)%g"\
	    < $< > $@

# install target (man pages, sgml, html & txt)
#

.PHONY: install
install:
	@for f in $(MANPAGES); do\
	  echo $(INSTALL) -d `dirname $(MANDIR)/$$f`;\
	  $(INSTALL) -d `dirname $(MANDIR)/$$f`;\
	  echo $(INSTALL_DATA) $$f $(MANDIR)/$$f;\
	  $(INSTALL_DATA) $$f $(MANDIR)/$$f;\
	done

	$(INSTALL) -d $(SGMLDIR)
	@for f in *.sgml; do\
	  echo $(INSTALL_DATA) $$f $(SGMLDIR)/$$f;\
	  $(INSTALL_DATA) $$f $(SGMLDIR)/$$f;\
	done

	$(INSTALL) -d $(HTMLDIR)
	@for f in *.html; do\
	  echo $(INSTALL_DATA) $$f $(HTMLDIR)/$$f;\
	  $(INSTALL_DATA) $$f $(HTMLDIR)/$$f;\
	done

	$(INSTALL) -d $(TXTDIR)
	@for f in *.txt; do\
	  echo $(INSTALL_DATA) $$f $(TXTDIR)/$$f;\
	  $(INSTALL_DATA) $$f $(TXTDIR)/$$f;\
	done

# auxiliary targets
#
.PHONY: clean
clean:
	-$(RM) $(MANPAGES)
	-$(RM) *.html *.txt
