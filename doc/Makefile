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
#
#  * Nothing is installed at the moment. That has to be done manually.

# must be first
#
default: html

XSLTPROC=xsltproc
XSLTPROC_HTML_OUTDIR=users_guide/
XSLTPROC_HTML_CSS=fptools.css
XSLTPROC_HTML_PARAMS=\
	--stringparam use.id.as.filename 1 \
	--stringparam base.dir $(XSLTPROC_HTML_OUTDIR) \
	--stringparam html.stylesheet $(XSLTPROC_HTML_CSS)
XSLTPROC_HTML_STYLESHEET=/usr/share/sgml/docbook/xsl-stylesheets-1.70.1/xhtml/chunk.xsl

# build targets
#
.PHONY: html pdf
html:
	$(XSLTPROC) $(XSLTPROC_HTML_PARAMS) $(XSLTPROC_HTML_STYLESHEET) c2hs.xml
	cp $(XSLTPROC_HTML_CSS) $(XSLTPROC_HTML_OUTDIR)

pdf:
	sgml2pdf c2hs.xml

# auxiliary targets
#
.PHONY: clean
clean:
	-rm -rf users_guide/ c2hs.pdf
