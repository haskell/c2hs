module Version (version, copyright, disclaimer)
where

-- version number is major.minor.patchlvl; don't change the format of the
-- `versnum' line as it is `grep'ed for by a Makefile
--
idstr      = "$Id: Version.hs,v 1.65 2002/09/06 06:34:13 chak Exp $"
name       = "C->Haskell Compiler"
versnum    = "0.10.16"
versnick   = "\"Altocumulus Stratiformis Perlucidus Undulatus\""
date	   = "06 Sep 2002"
version    = name ++ ", version " ++ versnum ++ " " ++ versnick ++ ", " ++ date
copyright  = "Copyright (c) [1999..2002] Manuel M T Chakravarty"
disclaimer = "This software is distributed under the \
	     \terms of the GNU Public Licence.\n\
	     \NO WARRANTY WHATSOEVER IS PROVIDED. \
	     \See the details in the documentation."
