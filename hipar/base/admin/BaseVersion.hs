module BaseVersion (version, copyright, disclaimer)
where

-- version number is major.minor.patchlvl; don't change the format of the
-- `versnum' line as it is `grep'ed for by a Makefile
--
idstr      = "$Id: BaseVersion.hs,v 1.37 2002/03/12 12:39:13 chak Exp $"
name       = "Compiler Toolkit"
versnum    = "0.24.0"
date	   = "12 Mar 2002"
version    = name ++ ", version " ++ versnum ++ ", " ++ date
copyright  = "Copyright (c) [1995..2002] Manuel M T Chakravarty"
disclaimer = "This software is distributed under the \
	     \terms of the GNU Public Licence.\n\
	     \NO WARRANTY WHATSOEVER IS PROVIDED. \
	     \See the details in the documentation."
