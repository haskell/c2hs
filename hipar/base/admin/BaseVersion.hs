module BaseVersion (version, copyright, disclaimer)
where

-- version number is major.minor.patchlvl; don't change the format of the
-- `versnum' line as it is `grep'ed for by a Makefile
--
idstr      = "$Id: BaseVersion.hs,v 1.36 2002/03/05 12:23:01 chak Exp $"
name       = "Compiler Toolkit"
versnum    = "0.23.1"
date	   = "05 Mar 2002"
version    = name ++ ", version " ++ versnum ++ ", " ++ date
copyright  = "Copyright (c) [1995..2002] Manuel M T Chakravarty"
disclaimer = "This software is distributed under the \
	     \terms of the GNU Public Licence.\n\
	     \NO WARRANTY WHATSOEVER IS PROVIDED. \
	     \See the details in the documentation."
