module Version (version, copyright, disclaimer)
where

-- version number is major.minor.patchlvl; don't change the format of the
-- `versnum' line as it is `grep'ed for by a Makefile
--
idstr      = "$Id: Version.hs,v 1.53 2001/12/11 02:26:26 chak Exp $"
name       = "C->Haskell Compiler"
versnum    = "0.10.3"
versnick   = "\"Altocumulus Stratiformis Perlucidus Undulatus\""
date	   = "11 Dec 2001"
version    = name ++ ", version " ++ versnum ++ " " ++ versnick ++ ", " ++ date
copyright  = "Copyright (c) [1999..2001] Manuel M. T. Chakravarty"
disclaimer = "This software is distributed under the \
	     \terms of the GNU Public Licence.\n\
	     \NO WARRANTY WHATSOEVER IS PROVIDED. \
	     \See the details in the documentation."
