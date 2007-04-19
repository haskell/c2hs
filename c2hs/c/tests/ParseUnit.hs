-- test program for C modules
-- parse a whole translation unit
--

module Main (main) where

import System.Environment (getArgs)
import System.Cmd (rawSystem)
import System.Exit (ExitCode(..))
import System.CPUTime (getCPUTime)
import Control.Exception (evaluate)
import Numeric (showFFloat)

import CAST
import CParser (parse, header)


main :: IO ()
main = do
  [inputFile] <- getArgs
  input <- readFile inputFile

  let result = CParser.parse CParser.header input

  duration <- time $ evaluate result

  case result of
    Right error -> do
      putStrLn $ "C parser failed to parse " ++ inputFile
        ++ " with message:\n" ++ error

    Left headder -> do
      let (CHeader decls _) = headder
      putStrLn $ "Read " ++ show (length decls) ++ " declaration(s)"
      putStrLn $ "in " ++ showFFloat (Just 2) duration "s"

time :: IO a -> IO Double
time action = do
    start <- getCPUTime
    action
    end   <- getCPUTime
    return $! (fromIntegral (end - start)) / (10^12)
