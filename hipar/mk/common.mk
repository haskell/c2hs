#  Compiler Toolkit: common make include
#
#  Author : Manuel M T Chakravarty
#  Created: 22 October 1997
#
#  Version $Revision: 1.49 $ from $Date: 2002/03/12 12:39:17 $
#
#  Copyright (c) [1997..2002] Manuel M T Chakravarty
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
#  This makefile contains the common variables definitions and generic targets.
#  Global definitions, such as the heap size for the Haskell compiler, can 
#  (and should!) be modified locally.  The generic targets avoid tedious 
#  repetitions in the specialised makefiles.
#
#  = TODO =====================================================================
#

#  ***************************************
#  !!! This makefile requires GNU make !!!
#  ***************************************

# default target (the root and each package Makefile should have a `default' 
# target)
# 
.PHONY: dft
dft:
	@make default

# GNU make trickery
#
empty:=
space:= $(empty) $(empty)

# name of dependency files
#
DEPEND=.depend

# determine the top level dir (not very nice)
#
# * first case, we are at the toplevel
# * second case, we are at a package root
# * third case, we are in a package part
#
ifeq ($(strip $(PACKAGE)),)
  TOP=.
else
  ifeq ($(strip $(PART)),)
    TOP=..
  else
    TOP=../..
  endif
endif

# local configuration and dependencies if available
#
# * there are only dependencies available on package toplevels
#
include $(TOP)/mk/config.mk
-include $(DEPEND)

# build directory
#
# * since version 0.24.0, we don't have a separate build directory
#
BUILDDIR:="FORGOT_TO_REMOVE_BUILDIR"
DOCBUILDDIR:="FORGOT_TO_REMOVE_DOCBUILDIR"

# Definition of the package parts
# ===============================

BASEPARTS =admin errors general graphs state syms syntax sysdep

ifeq ($(strip $(PACKAGE)),base)
  PARTS  = $(BASEPARTS)
  HIDIRS:=
endif

# package information for a package <package> is contained in 
# <package>/mk/<package>.pck.mk files, which we also use to compute list of 
# all package names
#
PCKMKS=$(wildcard $(TOP)/*/mk/*.pck.mk)
ifneq ($(strip $(PCKMKS)),)
  include $(PCKMKS)
endif
PCKS=$(notdir $(basename $(basename $(PCKMKS))))

# Search path for make
# ====================
#
# * we don't consider the case where we are within a part, as we will move out
#   before building anything anyway
#
ifeq ($(PART),)
  VPATH  = $(PARTS)
  HIDIRS:= $(HIDIRS) $(VPATH)
else
  VPATH  = $(addprefix ../,$(filter-out $(PART),$(PARTS)))
  HIDIRS:= $(addprefix ../,$(HIDIRS)) $(VPATH)
endif

# Assemble compiler options
# =========================
#
# * $(EXTRAHCFLAGS) may be instantiated in the specialised makefiles
#
# * If we have package support, we need to make sure that all packages declare
#   their package name and that all packages except `base' request the 
#   inclusion of `base'.
#
HIDIRSINCL = $(addprefix $(HIDIROPT),$(HIDIRS))
ifeq ($(HASPKG),yes)
  ifneq ($(strip $(PACKAGE)),base)
    HCFLAGS += -package-conf $(TOP)/base/base.build.conf -package base
  endif
  HCFLAGS   += -package-name $(PACKAGE)
endif
HCFLAGS     += $(PROF) $(HIDIRSINCL) $(EXTRAHCFLAGS)

# Templates
# =========
#
.PRECIOUS: %.hi %.o
.PRECIOUS: $(OBJS)

# propagate `make depend' rules to .hi files
#
%.hi: %.o
	@:

# when we are on a package toplevel, just descend into a part directory to 
# compile a Haskell file; if we are in a part, we may compile any Haskell 
# source that is located in that part
#
# * the idea is to always initiate compiles at the package toplevel; the make 
#   process, however, decends into the corresponding part to get any part local
#   makefile settings
#
# * In addition to $(HCFLAGS), when a variable with the name `<source>-HCFLAGS'
#   is defined (where <source> is the source filename), the value of this 
#   variable is also passed to the compiler.  This is useful for options that 
#   are specific to a source file (like the heap space needed by the compiler).
#
ifeq ($(PART),)
%.o: %.hs
	$(MAKE) -C $(dir $<) $(MFLAGS) `$(BASENAME) $@`
else
$(OBJS): %.o: %.hs
#	$(RM) $@
	$(HC) -c $(HCFLAGS) $($(join $<,-HCFLAGS)) $<
endif

# generate parser (does this in the part containing the parser spec)
#
%.hs: %.ly
	cd $(dir $<); $(HAPPY) --info=$*.info -o $@ $<
#	$(HAPPY) --array --info=$*.info -o $@ $<
%.hs: %.y
	cd $(dir $<); $(HAPPY) --info=$*.info -o $@ $<


# Generic targets
# ===============

# dependency computation
#
# * The variable $(MKDEPENDOPTS) contains options for $(MKDEPENDHS).
# * All Haskell modules (files with suffix `.hs') in the $(PARTS) directories 
#   are  analysed.  However, we remove all modules from the `sysdep' part , 
#   because we don't know which of the other modules are used by which 
#   systems.  The variable MKDEPENDFILES is extended in the base/Makefile
#   to include all the necessary sysdep files.
# * Using `sed' all directory prefixes are removed.  This cannot lead to 
#   ambiguities, because all modules belong to the same program.
#
# * KLUDGE: The use of $(GREP) is a kludge to filter out the `base' modules 
#   from dependencies for `dhc' and `nepal', which is used because the `-X' 
#   option seems to be ignored by ghc's $(MKDEPENDHS).
#
.PHONY: gendepend

MKDEPENDFILES=$(wildcard $(addsuffix /*.hs,$(filter-out sysdep,$(PARTS))))
ifeq ($(SYS),$(findstring $(SYS),ghc4 ghc5))
gendepend:
	@echo "*** Generating dependencies for $(PACKAGE)..."
	$(MKDEPENDHS) -optdep-f -optdep$(DEPEND)\
          $(HCFLAGS) $(MKDEPENDOPTS)\
	  $(MKDEPENDFILES)
endif
ifeq ($(SYS),$(findstring $(SYS),nhc1))
gendepend:
	@echo "*** Generating dependencies for $(PACKAGE)..."
	$(MKDEPENDHS)\
	  $(addprefix $(HIDIROPT),$(HIDIRS)) $(MKDEPENDOPTS)\
	  $(MKDEPENDFILES) >$(DEPEND)
endif

# build targets
# 
.PHONY: recobjs recall rectest

# recursively make all object files
#
recobjs:
	@echo "*** Recursively making $(PACKAGE) objects..."
	@for part in $(PARTS) ; do \
	  $(MAKE) -C $$part $(MFLAGS) objs || exit; \
	done

# recursively make all
#
recall:
	@echo "*** Recursively making $(PACKAGE)..."
	@for part in $(PARTS) ; do \
	  $(MAKE) -C $$part $(MFLAGS) all || exit; \
	done

# recursively make tests
#
rectest:
	@echo "*** Recursively making $(PACKAGE) test..."
	@for part in $(PARTS) ; do \
	  $(MAKE) -C $$part $(MFLAGS) test || exit; \
	done

# auxilliary targets
#
.PHONY: recclean reccleanhi cleanall

recclean:
	@echo "Recursively cleaning $(PACKAGE)..."
	@for part in $(PARTS) ; do \
	  $(MAKE) -C $$part $(MFLAGS) clean || exit; \
	done

reccleanhi:
	@echo "Recursively cleaning $(PACKAGE) interface files..."
	@for part in $(PARTS) ; do \
	  $(MAKE) -C $$part $(MFLAGS) cleanhi || exit; \
	done

cleanall: clean cleanhi
