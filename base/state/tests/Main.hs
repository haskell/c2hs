--  Compiler Toolkit: test routines for state modules
--
--  Author : Manuel M. T. Chakravarty
--  Created: 2 November 95
--
--  Version $Revision: 1.7 $ from $Date: 1999/03/30 11:50:30 $
--
--  Copyright (c) [1995..1998] Manuel M. T. Chakravarty
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  This module provides code used to test the state base modules.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 1.4
--
--- TODO ----------------------------------------------------------------------
--

module Main
where

import State


main :: IO ()
main  = run ("", "", "") () (
	  putStrCIO "Compiler Toolkit state test program\n"	+>
	  putStrCIO "===================================\n\n"	+>
	  testErrors						+>
	  testExceptions
	)

testErrors :: PreCST e s ()
testErrors  = putStrCIO "Testing error management...\n\n"                   +>
	      raiseWarning ("myfile", 100, 10) ["A nice warning message"]   +>
	      raiseError ("myfile", 50, 25) ["That's too much of an error.",
					     "Please avoid this kind of\
					     \ errors in the future."]      +>
	      dumpErrors						    +>
	      dumpErrors						    +>
	      putStrCIO "\n...done (testing error management).\n"
              where
	        dumpErrors :: PreCST e s ()
		dumpErrors  = showErrors	+>= \msg ->
			      putStrCIO msg	+>
			      newlineCIO

testExceptions :: PreCST e s ()
testExceptions  = putStrCIO "Testing exception handling...\n\n"		    +>
		  (raiseExc "testexc")
		  `catchExc` ("testexc", 
			      \msg ->
			        putStrCIO ("Caught `testexc' with message `"
				           ++ msg ++ "'.\n")
			     )						    +>
		  raiseFatal
		  `fatalsHandledBy` (\err ->
				       putStrCIO ("Caught fatal error: "
						  ++ show err ++ "\n")
				    )					    +>
		  -- the following exception is not caught and so
		  -- should turn into a fatal error
		  ((raiseExc "otherexc")
		   `catchExc` ("testexc", 
			       \msg ->
			         putStrCIO ("ATTENTION: If this shows an \
				            \exception erroneously caught!!!\n")
			      )
		  )
		  `fatalsHandledBy` (\err ->
				       putStrCIO ("Caught fatal error: "
						  ++ show err ++ "\n")
				    )					    +>
		  putStrCIO "\n...done (testing exception handling).\n"
		  where
		    raiseExc     :: String -> PreCST e s ()
		    raiseExc exc  = putStrCIO ("Will now raise `" ++ exc 
					       ++ "'.\n")	            +>
				    throwExc exc "A hell of an exception!"  +>
				    putStrCIO ("ATTENTION: This message must \
					       \never show!!!\n")

		    raiseFatal :: PreCST e s ()
		    raiseFatal  = putStrCIO "Will now trigger a fatal \
					    \error!\n"		            +>
				  fatal "Fatal indeed!"			    +>
				  putStrCIO ("ATTENTION: This message must \
					     \never show!!!\n")

				    
