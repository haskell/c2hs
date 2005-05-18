-- test program for C modules
--

-- struct with 20 fields 200 times, 1000 times
--

module Main (main)
where

import Monad
import System
import CPUTime

import C2HSState
import CAST
import CLexer
import CParser


main :: IO ()
main  = runC2HS ("C modules test", "", "") doIt

doIt :: CST ()()
doIt  = do
	  putStrCIO ("C Modules Test\n")
	  putStrCIO ("~~~~~~~~~~~~~~\n")
	  args <- getArgsCIO
	  when (length args /= 1) $
	    error "Exactly one argument (file name) required!"
	  let fname   = head args
	      initPos = (fname, 1, 1)
	  --- measure file read time
	  startTime <- liftIO getCPUTime
	  cs <- readFileCIO fname
	  putStrCIO (fname ++ " contains " ++ show (length cs) 
		     ++ " characters.\n")
	  endTime <- liftIO getCPUTime   -- after demand of `notoks'
	  let frtime  = timeDiffToMSec (endTime - startTime)
	  putStrCIO ("  Read time is " ++ show frtime ++ "ms.\n")
	  --- measure lexing time
	  putStrCIO ("\n*** Subtest: lexer\n")
	  cs <- readFileCIO fname
	  startTime <- liftIO getCPUTime
	  ts <- lexC cs initPos
	  let notoks = length ts
	  putStrCIO ("Read " ++ show notoks ++ " tokens.\n")
	  endTime <- liftIO getCPUTime   -- after demand of `notoks'
	  let reqdtime   = timeDiffToMSec (endTime - startTime)
	      tokpersec1 = (1000 * notoks) `div` reqdtime
	      tokpersec2 = (1000 * notoks) `div` (reqdtime - frtime)
	  putStrCIO ("  Required time overall " ++ show reqdtime 
		     ++ "ms; without file read " ++ show (reqdtime - frtime)
		     ++ "ms.\n")
	  putStrCIO ("  Average of overall " ++ show tokpersec1 ++ " and \
		     \normalized "++ show tokpersec2 ++ " tokens per sec\n\
		     \  (these are returned tokens, ie, will be low for \
		     \whitespace-rich input).\n")
	  --- output lexer errors
	  errs <- showErrors
	  putStrCIO errs
	  --- measure parsing time
	  putStrCIO ("\n*** Subtest: parser\n")
	  cs <- readFileCIO fname
	  startTime <- liftIO getCPUTime
	  (do 
	     CHeader decls _ <- parseC cs initPos
	     putStrCIO ("Read " ++ show (length decls) ++ " declaration(s).\n")
            ) 
	    `fatalsHandledBy` 
	      \err -> let errmsg = ioeGetErrorString err
		      in
		      putStrCIO (">>> Parse error!\n" ++ errmsg)
	  endTime <- liftIO getCPUTime   -- after demand of `decls'
	  let reqdtime   = timeDiffToMSec (endTime - startTime)
	      tokpersec1 = (1000 * notoks) `div` reqdtime
	      tokpersec2 = (1000 * notoks) `div` (reqdtime - frtime)
	  putStrCIO ("  Required time overall " ++ show reqdtime 
		     ++ "ms; without file read " ++ show (reqdtime - frtime)
		     ++ "ms.\n")
	  putStrCIO ("  Average of overall " ++ show tokpersec1 ++ " and \
		     \normalized "++ show tokpersec2 ++ " tokens per sec.\n")

timeDiffToMSec      :: Integer -> Int
timeDiffToMSec psec  = fromInteger (psec `div` 1000000000) 
