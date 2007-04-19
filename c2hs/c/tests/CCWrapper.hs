{-# OPTIONS_GHC -fglasgow-exts #-}

-- test program for C modules
--

module Main (main) where

import System.Environment (getArgs, getEnv)
import System.CPUTime (getCPUTime)
import System.Cmd (rawSystem)
import System.IO (appendFile, openTempFile, hClose, hPutStr)
import System.Directory (getCurrentDirectory, removeFile)
import Control.Exception (evaluate, catchJust, ioErrors)
import Numeric (showFFloat)
import Data.List (isSuffixOf)

import CAST
import CParser (parse, header)


main :: IO ()
main = do
  logdir <- getEnv "C2HS_CC_LOGDIR"
  let logFile = logdir ++ "/cc-wrapper.log"

  args <- getArgs
  case mungeArgs [] [] args of
    Ignore  -> return ()
    Unknown -> appendFile logFile $ 
                 "could not munge gcc args: " ++ show args ++ "\n"

    Groked cfile args' -> do
    
      (outFile, hnd) <- openTempFile logdir "cc-wrapper.i"
      hClose hnd
      gccExitcode <- rawSystem "gcc" (["-E", "-o", outFile] ++ args')

      input <- readFile outFile
      let result = parse header input
      duration <- time $ evaluate result
      removeFile outFile

      case result of
        Right error -> do
          (reportFile, hnd) <- openTempFile logdir "cc-wrapper.report"
          pwd <- getCurrentDirectory
          hPutStr hnd $ "failed to parse " ++ cfile
                     ++ "\nwith message:\n" ++ error
                     ++ "\nworking dir: " ++ pwd
                     ++ "\ncommand: " ++ show args
                     ++ "\npreprocessed input follows:\n\n" ++ input
          hClose hnd
          appendFile logFile $ "failed to parse " ++ cfile
                            ++ "\n  (see " ++ reportFile ++ ")"
                            ++ "\n  with message: " ++ error ++ "\n\n"

        Left headder ->
          let (CHeader decls _) = headder
           in appendFile logFile $ "parsed " ++ cfile
                            ++ " (" ++ show (length decls) ++ " decls) in "
                            ++ showFFloat (Just 2) duration "s\n"

data MungeResult = Unknown | Ignore | Groked FilePath [String]
mungeArgs :: [String] -> String -> [String] -> MungeResult
mungeArgs accum []    [] = Unknown
mungeArgs accum cfile [] = Groked cfile (reverse accum)
mungeArgs accum cfile ("-E":args) = Ignore
mungeArgs accum cfile ("-M":args) = Ignore
mungeArgs accum cfile ("-o":outfile:args) = mungeArgs accum cfile args
mungeArgs accum cfile (cfile':args)
          | ".c" `isSuffixOf` cfile'
         || ".hc" `isSuffixOf` cfile'
         || ".i"  `isSuffixOf` cfile' =
              if null cfile
                then mungeArgs (cfile':accum) cfile' args
                else Unknown
mungeArgs accum cfile (cfile':args)
          | ".S" `isSuffixOf` cfile' = Ignore
mungeArgs accum cfile (arg:args) = mungeArgs (arg:accum) cfile args

time :: IO a -> IO Double
time action = do
    start <- getCPUTime
    action
    end   <- getCPUTime
    return $! (fromIntegral (end - start)) / (10^12)
