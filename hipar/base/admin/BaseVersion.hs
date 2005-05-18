module BaseVersion (version, copyright, disclaimer)
where

-- version number is major.minor.patchlvl; don't change the format of the
-- `versnum' line as it is `grep'ed for by a Makefile
--
idstr      = "$Id: BaseVersion.hs,v 1.31 2000/10/05 07:51:28 chak Exp $"
name       = "Compiler Toolkit"
versnum    = "0.22.2"
date	   = "02 Oct 2000"
version    = name ++ ", version " ++ versnum ++ ", " ++ date
copyright  = "Copyright (c) [1995..2000] Manuel M. T. Chakravarty"
disclaimer = "This software is distributed under the \
	     \terms of the GNU Public Licence.\n\
	     \NO WARRANTY WHATSOEVER IS PROVIDED. \
	     \See the details in the documentation."
