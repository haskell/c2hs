#  Compiler Toolkit: common make include
#
#  Author : Manuel M T Chakravarty
#  Created: 22 October 1997
#
#  Version $Revision: 1.48 $ from $Date: 2002/02/19 07:34:49 $
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

# local configuration
#
include $(TOP)/mk/config.mk

# build directory (depends on the used system and whether profiling is used)
#
# * however, there is only one build dir for documentation
#
BUILDDIR:=$(TOP)/build/$(SYS)
ifneq ($(strip $(PROF)),)
  BUILDDIR:=$(BUILDDIR)p
endif
DOCBUILDDIR:=$(TOP)/build/doc

# Definition of the package parts
# ===============================

ifneq ($(strip $(PACKAGE)),)
  ifeq ($(strip $(shell pwd | $(GREP) build)),)
    *** If you get an error here, 
    *** you did not execute make in the *build* tree (but somewhere else)!
  endif
endif

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
# * if we are in the root of a package, all parts of that package
# * otherwise, within a part, the other parts of the same package

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
#HIDIRSCOL = $(subst $(space),:,$(strip $(HIDIRS)))
HIDIRSINCL = $(addprefix $(HIDIROPT),$(HIDIRS))
ifeq ($(HASPKG),yes)
  ifneq ($(strip $(PACKAGE)),base)
    HCFLAGS += -package-conf $(TOP)/base/base.build.conf -package base
  endif
  HCFLAGS   += -package-name $(PACKAGE)
endif
HCFLAGS     += $(PROF) $(HIDIRSINCL) $(EXTRAHCFLAGS)

# Misc
# ====

# Name of dependency files
#
DEPEND=.depend

# Templates
# =========
#
.PRECIOUS: %.hi %.o
.PRECIOUS: $(OBJS)

# propagate `make depend' rules to .hi files  ** this is subtle **
#
# * When a .o file changed, its corresponding .hi file *might* also have 
#   changed.
#
# * So, we need to have this rule to make it clear that modules depending on 
#   this .hi have to be considered for re-compiling (the `.depend' files 
#   contain dependencies from .hi files to .o files).
#
# * But, we have to be careful: According to the next two rules, only files 
#   within the current part might be compiled now, and thus, only their .hi 
#   files are considered here.  Using an implicit rules instead of a static 
#   pattern rule is possible, but then a lot of checks are induces in other 
#   parts, where make is recursively called to check these parts only to find 
#   that nothing changed, which was clear from the beginning.  We treat .hi 
#   from other parts later with an implicit rule.
#
# * We touch the .hi file and disable ghc's optimization of not updating .hi 
#   files if they didn't change from one compiler to another.  We do this 
#   because .hi files that are older than their corresponding .o files are a 
#   nightmare for make -- I tried quite hard to avoid this, but given a source
#   distributed over multiple subdirectories with local Makefiles, there
#   doesn't seem to be an altenative.
#
# * The dependence is the .o file and not the .hs file, because this rule 
#   should only be executed *after* the .o file (and thus also the .hi) is 
#   generated.
#
# * Note that there are always exist .hi files when compilation starts, as the
#   initial .hi files are already created by `make prep' (they are, however,
#   empty).
#
$(OBJS:%.o=%.hi): %.hi: %.o
	touch $@

# if the source is in this part, compile here
#
# * In addition to $(HCFLAGS), when a variable with the name `<source>-HCFLAGS'
#   is defined (where <source> is the source filename), the value of this 
#   variable is also passed to the compiler.  This is useful for options that 
#   are specific to a source file (like the heap space needed by the compiler).
#
$(OBJS): %.o: %.hs
	$(RM) $@
	$(HC) -c $(HCFLAGS) $($(join $<,-HCFLAGS)) $<

# if we are in the root dir or the source is not in the current part, 
# make the target in the part where it resides
#
%.o: %.hs
	$(MAKE) -C $(dir $<) $(MFLAGS) `$(BASENAME) $@`
%.hi: %.o
	$(MAKE) -C $(dir $<) $(MFLAGS) `$(BASENAME) $@`

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
# * The way we distinguish here between ghc and nhc is also far from nice (
#   in particular the test for `base') - some autoconf magic would be nicer.
#
.PHONY: gendepend

MKDEPENDFILES=$(wildcard $(addsuffix /*.hs,$(filter-out sysdep,$(PARTS))))
ifeq ($(SYS),$(findstring $(SYS),ghc4 ghc5))
gendepend:
	@echo "*** Generating dependencies for $(PACKAGE)..."
	$(MKDEPENDHS) -optdep-f -optdep$(DEPEND)\
          $(HCFLAGS) $(MKDEPENDOPTS)\
	  $(MKDEPENDFILES)
	$(MV) $(DEPEND) $(DEPEND).orig
	$(GREP) -v base $(DEPEND).orig\
	| $(SED) -e "s|[a-zA-Z0-9,-._#]*/||g" >$(DEPEND)
endif
ifeq ($(SYS),$(findstring $(SYS),nhc1))
gendepend:
	@echo "*** Generating dependencies for $(PACKAGE)..."
	$(MKDEPENDHS)\
	  $(addprefix $(HIDIROPT),$(HIDIRS)) $(MKDEPENDOPTS)\
	  $(MKDEPENDFILES) >$(DEPEND)
	$(MV) $(DEPEND) $(DEPEND).orig
	$(GREP) -v base $(DEPEND).orig\
	| $(SED) -e "s|[a-zA-Z0-9,-._#]*/||g" >$(DEPEND)
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
