module Version (version, copyright, disclaimer)
where

-- version number is major.minor.patchlvl; don't change the format of the
-- `versnum' line as it is `grep'ed for by a Makefile
--
idstr      = "$Id: Version.hs,v 1.71 2003/10/19 10:46:11 chak Exp $"
name       = "C->Haskell Compiler"
versnum    = "0.12.0"
versnick   = "\"Springtime\""
date	   = "19 Oct 2003"
version    = name ++ ", version " ++ versnum ++ " " ++ versnick ++ ", " ++ date
copyright  = "Copyright (c) [1999..2003] Manuel M T Chakravarty"
disclaimer = "This software is distributed under the \
	     \terms of the GNU Public Licence.\n\
	     \NO WARRANTY WHATSOEVER IS PROVIDED. \
	     \See the details in the documentation."
