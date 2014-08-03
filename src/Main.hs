--  C->Haskell Compiler: main module
--
--  Copyright (c) [1999..2005] Manuel M T Chakravarty
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
--- Description ---------------------------------------------------------------
--
--  Language: Haskell 98
--
--  This is the main module of the compiler.  It sets the version, processes
--  the command line arguments, and controls the compilation process.
--
--  Usage:
--  ------
--
--    c2hs [ option... ] [header-file] binding-file
--
--  The compiler is supposed to emit a Haskell program that expands all hooks
--  in the given binding file.
--
--  File name suffix:
--  -----------------
--
--  Note: These also depend on suffixes defined in the compiler proper.
--
--  .h   C header file
--  .i   pre-processeed C header file
--  .hs  Haskell file
--  .chs Haskell file with C->Haskell hooks (binding file)
--  .chi C->Haskell interface file
--
--  Options:
--  --------
--
--  -C CPPOPTS
--  --cppopts=CPPOPTS
--        Pass the additional options CPPOPTS to the C preprocessor.
--
--        Repeated occurences accumulate.
--
--  -c CPP
--  --cpp=CPP
--        Use the executable CPP to invoke CPP.
--
--        In the case of repeated occurences, the last takes effect.
--
--  -d TYPE
--  --dump=TYPE
--        Dump intermediate representation:
--
--        + if TYPE is `trace', trace the compiler phases (to stderr)
--        + if TYPE is `genbind', trace binding generation (to stderr)
--        + if TYPE is `ctrav', trace C declaration traversal (to stderr)
--        + if TYPE is `chs', dump the binding file (insert `.dump' into the
--          file name to avoid overwriting the original file)
--
--  -h, -?
--  --help
--        Dump brief usage information to stderr.
--
--  -i DIRS
--  --include=DIRS
--        Search the colon (Linux) or semicolon (Windows) separated
--        list of directories DIRS when searching for .chi files.
--
--  -k
--  --keep
--        Keep the intermediate file that contains the pre-processed C header
--        (it carries the suffix `.i').
--
--  -l
--  --copy-library
--        Copies the library module `C2HS' into the same directory where the
--        generated code from the binding file is placed.
--
--  -o FILE
--  --output=FILE
--        Place output in file FILE.
--
--        If `-o' is not specified, the default is to put the output for
--        `source.chs' in `source.hs' in the same directory that contains the
--        binding file.  If specified, the emitted C header file is put into
--        the same directory as the output file.  The same holds for
--        C->Haskell interface file.  All generated files also share the
--        basename.
--
--  -p PLATFORM
--  --platform=PLATFORM
--        Generate output for the given PLATFORM.  By default we generate
--        output for the platform that c2hs executes on.
--
--  -t PATH
--  --output-dir=PATH
--        Place generated files in the directory PATH.
--
--        If this option as well as the `-o' option is given, the basename of
--        the file specified with `-o' is put in the directory specified with
--        `-t'.
--
--  -v,
--  --version
--        Print (on standard output) the version and copyright
--        information of the compiler (before doing anything else).
--
--- TODO ----------------------------------------------------------------------
--

module Main (main)
where

-- standard libraries
import Data.List (intersperse, partition)
import Control.Monad (when, unless)
import Data.Version (showVersion)
import System.Console.GetOpt
                  (ArgOrder(..), OptDescr(..), ArgDescr(..), usageInfo, getOpt)
import qualified System.FilePath as FilePath
                  (takeDirectory, takeExtension, dropExtension)
import System.FilePath ((<.>), (</>), splitSearchPath)
import System.IO (stderr, openFile, IOMode(..))
import System.IO.Error (ioeGetErrorString, ioeGetFileName)
import System.Process (runProcess, waitForProcess)

-- c2hs modules
import C2HS.State  (CST, runC2HS, fatal, fatalsHandledBy,
                   SwitchBoard(..), Traces(..), setTraces,
                   traceSet, setSwitch, getSwitch, putTraceStr)
import qualified System.CIO as CIO
import C2HS.C     (hsuffix, isuffix, loadAttrC)
import C2HS.CHS   (loadCHS, dumpCHS, hssuffix, chssuffix, dumpCHI, hasNonGNU)
import C2HS.Gen.Header  (genHeader)
import C2HS.Gen.Bind      (expandHooks)
import C2HS.Version    (versnum, version, copyright, disclaimer)
import C2HS.Config (cppopts, libfname, PlatformSpec(..),
                   defaultPlatformSpec, platformSpecDB)
import qualified C2HS.Config as CConf
import Paths_c2hs (getDataDir)


-- | wrapper running the compiler
--
main :: IO ()
main  = runC2HS compile


-- option handling
-- ===============

-- | header is output in case of help, before the descriptions of the options;
-- errTrailer is output after an error message
--
header :: String
header =
  version ++ "\n" ++ copyright ++ "\n" ++ disclaimer
  ++ "\n\nUsage: c2hs [ option... ] [header-file] binding-file\n"

trailer, errTrailer :: String
trailer    = "\n\
             \The header file must be a C header file matching the given \
             \binding file.\n\
             \The dump TYPE can be\n\
             \  trace   -- trace compiler phases\n\
             \  genbind -- trace binding generation\n\
             \  ctrav   -- trace C declaration traversal\n\
             \  chs     -- dump the binding file (adds `.dump' to the name)\n"
errTrailer = "Try the option `--help' on its own for more information.\n"

-- | supported option types
--
data Flag = CPPOpts  String     -- ^ additional options for C preprocessor
          | CPP      String     -- ^ program name of C preprocessor
          | Dump     DumpType   -- ^ dump internal information
          | Help                -- ^ print brief usage information
          | Keep                -- ^ keep the .i file
          | Library             -- ^ copy library module @C2HS@
          | Include  String     -- ^ list of directories to search .chi files
          | Output   String     -- ^ file where the generated file should go
          | Platform String     -- ^ target platform to generate code for
          | OutDir   String     -- ^ directory where generates files should go
          | Version             -- ^ print version information on stdout
          | NumericVersion      -- ^ print numeric version on stdout
          | Error    String     -- ^ error occured during processing of options
          deriving Eq

data DumpType = Trace         -- ^ compiler trace
              | GenBind       -- ^ trace "C2HS.Gen.Bind"
              | CTrav         -- ^ trace "C2HS.C.CTrav"
              | CHS           -- ^ dump binding file
              deriving Eq

-- | option description suitable for "Distribution.GetOpt"
--
options :: [OptDescr Flag]
options  = [
  Option ['C']
         ["cppopts"]
         (ReqArg CPPOpts "CPPOPTS")
         "pass CPPOPTS to the C preprocessor",
  Option ['c']
         ["cpp"]
         (ReqArg CPP "CPP")
         "use executable CPP to invoke C preprocessor",
  Option ['d']
         ["dump"]
         (ReqArg dumpArg "TYPE")
         "dump internal information (for debugging)",
  Option ['h', '?']
         ["help"]
         (NoArg Help)
         "brief help (the present message)",
  Option ['i']
         ["include"]
         (ReqArg Include "INCLUDE")
         "include paths for .chi files",
  Option ['k']
         ["keep"]
         (NoArg Keep)
         "keep pre-processed C header",
  Option ['l']
         ["copy-library"]
         (NoArg Library)
         "copy `C2HS' library module in",
  Option ['o']
         ["output"]
         (ReqArg Output "FILE")
         "output result to FILE (should end in .hs)",
  Option ['p']
         ["platform"]
         (ReqArg Platform "PLATFORM")
         "platform to use for cross compilation",
  Option ['t']
         ["output-dir"]
         (ReqArg OutDir "PATH")
         "place generated files in PATH",
  Option ['v']
         ["version"]
         (NoArg Version)
         "show version information",
  Option []
         ["numeric-version"]
         (NoArg NumericVersion)
         "show version number"]

-- | convert argument of 'Dump' option
--
dumpArg           :: String -> Flag
dumpArg "trace"    = Dump Trace
dumpArg "genbind"  = Dump GenBind
dumpArg "ctrav"    = Dump CTrav
dumpArg "chs"      = Dump CHS
dumpArg _          = Error "Illegal dump type."

-- | main process (set up base configuration, analyse command line, and execute
-- compilation process)
--
-- * Exceptions are caught and reported
--
compile :: CST s ()
compile  =
  do
    setup
    cmdLine <- CIO.getArgs
    case getOpt Permute options cmdLine of
      (opts, []  , [])
        | noCompOpts opts -> doExecute opts Nothing
      (opts, args, [])    -> case parseArgs args of
        justargs@(Just _) -> doExecute opts justargs
        Nothing           -> raiseErrs [wrongNoOfArgsErr]
      (_   , _   , errs)  -> raiseErrs errs
  where
    -- These options can be used without specifying a binding module.  Then,
    -- the corresponding action is executed without any compilation to take
    -- place.  (There can be --data and --output-dir (-t) options in addition
    -- to the action.)
    --
    aloneOptions = [Help, Version, NumericVersion, Library]
    --
    noCompOpts opts = let nonDataOpts = filter nonDataOrDir opts
                      in
                      (not . null) nonDataOpts &&
                      all (`elem` aloneOptions) nonDataOpts
      where
        nonDataOrDir (OutDir _) = False
        nonDataOrDir _          = True
    --
    parseArgs :: [FilePath] -> Maybe (FilePath, [FilePath])
    parseArgs = parseArgs' [] Nothing
      where parseArgs' hs (Just chs) []    = Just (chs, reverse hs)
            parseArgs' hs Nothing (file:files)
                | FilePath.takeExtension file == '.':chssuffix
                                           = parseArgs' hs (Just file) files
            parseArgs' hs chs (file:files)
                | FilePath.takeExtension file == '.':hsuffix
                                           = parseArgs' (file:hs) chs files
            parseArgs' _  _   _            = Nothing
    --
    doExecute opts args = do
                            execute opts args
                              `fatalsHandledBy` failureHandler
                            CIO.exitWith CIO.ExitSuccess
    --
    wrongNoOfArgsErr =
      "There must be exactly one binding file (suffix .chs),\n\
      \and optionally one or more header files (suffix .h).\n"
    --
    -- exception handler
    --
    failureHandler err =
      do
        let msg   = ioeGetErrorString err
            fnMsg = case ioeGetFileName err of
                       Nothing -> ""
                       Just s  -> " (file: `" ++ s ++ "')"
        name <- CIO.getProgName
        CIO.hPutStrLn stderr $ concat [name, ": ", msg, fnMsg]
        CIO.exitWith $ CIO.ExitFailure 1

-- | set up base configuration
--
setup :: CST s ()
setup  = do
           setCPP     CConf.cpp
           addCPPOpts cppopts

-- | output error message
--
raiseErrs      :: [String] -> CST s a
raiseErrs errs = do
                   CIO.hPutStr stderr (concat errs)
                   CIO.hPutStr stderr errTrailer
                   CIO.exitWith $ CIO.ExitFailure 1

-- Process tasks
-- -------------

-- | execute the compilation task
--
-- * if 'Help' is present, emit the help message and ignore the rest
-- * if 'Version' is present, do it first (and only once)
-- * actual compilation is only invoked if we have one or two extra arguments
--   (otherwise, it is just skipped)
--
execute :: [Flag] -> Maybe (FilePath, [FilePath]) -> CST s ()
execute opts args | Help `elem` opts = help
                  | otherwise        =
  do
    let (vs,opts') = partition (\opt -> opt == Version
                                     || opt == NumericVersion) opts
    mapM_ processOpt (atMostOne vs ++ opts')
    case args of
      Just (bndFile, headerFiles) -> do
        let bndFileWithoutSuffix  = FilePath.dropExtension bndFile
        computeOutputName bndFileWithoutSuffix
        process headerFiles bndFileWithoutSuffix
      Nothing ->
        computeOutputName "."   -- we need the output name for library copying
    copyLibrary
  where
    atMostOne = (foldl (\_ x -> [x]) [])

-- | emit help message
--
help :: CST s ()
help =
  do
    CIO.putStr (usageInfo header options)
    CIO.putStr trailer
    CIO.putStr $ "PLATFORM can be " ++ hosts ++ "\n"
    CIO.putStr $ "  (default is " ++ identPS defaultPlatformSpec ++ ")\n"
  where
    hosts = (concat . intersperse ", " . map identPS) platformSpecDB

-- | process an option
--
-- * 'Help' cannot occur
--
processOpt :: Flag -> CST s ()
processOpt (CPPOpts  cppopt ) = addCPPOpts  [cppopt]
processOpt (CPP      cpp    ) = setCPP      cpp
processOpt (Dump     dt     ) = setDump     dt
processOpt (Keep            ) = setKeep
processOpt (Library         ) = setLibrary
processOpt (Include  dirs   ) = setInclude  dirs
processOpt (Output   fname  ) = setOutput   fname
processOpt (Platform fname  ) = setPlatform fname
processOpt (OutDir   fname  ) = setOutDir   fname
processOpt Version            = do
                                  CIO.putStrLn version
                                  platform <- getSwitch platformSB
                                  CIO.putStr "  build platform is "
                                  CIO.print platform
processOpt NumericVersion     = CIO.putStrLn (showVersion versnum)
processOpt (Error    msg    ) = abort msg

-- | emit error message and raise an error
--
abort     :: String -> CST s ()
abort msg  = do
               CIO.hPutStrLn stderr msg
               CIO.hPutStr stderr errTrailer
               fatal "Error in command line options"

-- | Compute the base name for all generated files (Haskell, C header, and .chi
-- file)
--
-- * The result is available from the 'outputSB' switch
--
computeOutputName :: FilePath -> CST s ()
computeOutputName bndFileNoSuffix =
  setSwitch $ \sb@SwitchBoard{ outputSB = output } ->
    sb { outputSB = if null output then bndFileNoSuffix else output }

-- | Copy the C2HS library if requested
--
copyLibrary :: CST s ()
copyLibrary =
  do
    outdir  <- getSwitch outDirSB
    library <- getSwitch librarySB
    datadir <- CIO.liftIO getDataDir
    let libFullName = datadir </> libfname
        libDestName = outdir  </> libfname
    when library $
      CIO.readFile libFullName >>= CIO.writeFile libDestName


-- set switches
-- ------------

-- | set the options for the C proprocessor
--
addCPPOpts      :: [String] -> CST s ()
addCPPOpts opts  = setSwitch $ \sb -> sb {cppOptsSB = cppOptsSB sb ++ opts}

-- | set the program name of the C proprocessor
--
setCPP       :: FilePath -> CST s ()
setCPP fname  = setSwitch $ \sb -> sb {cppSB = fname}

-- set the given dump option
--
setDump         :: DumpType -> CST s ()
setDump Trace    = setTraces $ \ts -> ts {tracePhasesSW  = True}
setDump GenBind  = setTraces $ \ts -> ts {traceGenBindSW = True}
setDump CTrav    = setTraces $ \ts -> ts {traceCTravSW   = True}
setDump CHS      = setTraces $ \ts -> ts {dumpCHSSW      = True}

-- | set flag to keep the pre-processed header file
--
setKeep :: CST s ()
setKeep  = setSwitch $ \sb -> sb {keepSB = True}

-- | set flag to copy library module in
--
setLibrary :: CST s ()
setLibrary  = setSwitch $ \sb -> sb {librarySB = True}

-- | set the search directories for .chi files
--
-- * Several -i flags are accumulated. Later paths have higher priority.
--
-- * The current directory is always searched last because it is the
--   standard value in the compiler state.
--
setInclude :: String -> CST s ()
setInclude str =
  setSwitch $ \sb -> sb {chiPathSB = splitSearchPath str ++ (chiPathSB sb)}

-- | set the output file name
--
setOutput       :: FilePath -> CST s ()
setOutput fname  = do
                     when (FilePath.takeExtension fname /= '.':hssuffix) $
                       raiseErrs ["Output file should end in .hs!\n"]
                     setSwitch $ \sb -> sb {outputSB = FilePath.dropExtension fname}

-- | set platform
--
setPlatform :: String -> CST s ()
setPlatform platform =
  case lookup platform platformAL of
    Nothing -> raiseErrs ["Unknown platform `" ++ platform ++ "'\n"]
    Just p  -> setSwitch $ \sb -> sb {platformSB = p}
  where
    platformAL = [(identPS p, p) | p <- platformSpecDB]

-- | set the output directory
--
setOutDir       :: FilePath -> CST s ()
setOutDir fname  = setSwitch $ \sb -> sb {outDirSB = fname}

-- | set the name of the generated header file
--
setHeader       :: FilePath -> CST s ()
setHeader fname  = setSwitch $ \sb -> sb {headerSB = fname}


-- compilation process
-- -------------------

-- | read the binding module, construct a header, run it through CPP, read it,
-- and finally generate the Haskell target
--
-- * the header file name (first argument) may be empty; otherwise, it already
--   contains the right suffix
--
-- * the binding file name has been stripped of the .chs suffix
--
process                    :: [FilePath] -> FilePath -> CST s ()
process headerFiles bndFile  =
  do
    -- load the Haskell binding module
    --
    (chsMod , warnmsgs) <- loadCHS bndFile
    CIO.putStr warnmsgs
    --
    -- get output directory and create it if it's missing
    --
    outFName <- getSwitch outputSB
    outDir   <- getSwitch outDirSB
    let outFPath = outDir </> outFName
    CIO.createDirectoryIfMissing True $ FilePath.takeDirectory outFPath
    --
    -- dump the binding file when demanded
    --
    flag <- traceSet dumpCHSSW
    when flag $ do
      let chsName = outFPath <.> "dump"
      CIO.putStrLn $ "...dumping CHS to `" ++ chsName ++ "'..."
      dumpCHS chsName chsMod False
    --
    -- extract CPP and inline-C embedded in the .chs file (all CPP and
    -- inline-C fragments are removed from the .chs tree and conditionals are
    -- replaced by structured conditionals)
    --
    (header', strippedCHSMod, headerwarnmsgs) <- genHeader chsMod
    CIO.putStr headerwarnmsgs
    --
    -- create new header file, make it #include `headerFile', and emit
    -- CPP and inline-C of .chs file into the new header
    --
    let newHeader     = outFName <.> chssuffix <.> hsuffix
        newHeaderFile = outDir </> newHeader
        preprocFile   = outFPath <.> isuffix
    CIO.writeFile newHeaderFile $ concat $
      [ "#include \"" ++ headerFile ++ "\"\n"
      | headerFile <- headerFiles ]
      ++ header'
    --
    -- Check if we can get away without having to keep a separate .chs.h file
    --
    case headerFiles of
      [headerFile] | null header
        -> setHeader headerFile    -- the generated .hs file will directly
                                   -- refer to this header rather than going
                                   -- through a one-line .chs.h file.
      _ -> setHeader newHeader
    --
    -- run C preprocessor over the header
    --
    cpp      <- getSwitch cppSB
    cppOpts  <- getSwitch cppOptsSB
    let nonGNUOpts =
          if hasNonGNU chsMod
          then [ "-U__GNUC__"
               , "-U__GNUC_MINOR__"
               , "-U__GNUC_PATCHLEVEL__"
               , "-D__AVAILIBILITY__"
               , "-D__OSX_AVAILABLE_STARTING(a,b)"
               , "-D__OSX_AVAILABLE_BUT_DEPRECATED(a,b,c,d)"
               , "-D__OSX_AVAILABLE_BUT_DEPRECATED_MSG(a,b,c,d,e)" ]
          else []
        args = cppOpts ++ nonGNUOpts ++ ["-U__BLOCKS__"] ++ [newHeaderFile]
    tracePreproc (unwords (cpp:args))
    exitCode <- CIO.liftIO $ do
      preprocHnd <- openFile preprocFile WriteMode
      cppproc <- runProcess cpp args
        Nothing Nothing Nothing (Just preprocHnd) Nothing
      waitForProcess cppproc
    case exitCode of
      CIO.ExitFailure _ -> fatal "Error during preprocessing custom header file"
      _                 -> return ()
    --
    -- load and analyse the C header file
    --
    (cheader, preprocMsgs) <- loadAttrC preprocFile
    CIO.putStr preprocMsgs
    --
    -- remove the pre-processed header and if we no longer need it, remove the
    -- custom header file too.
    --
    keep <- getSwitch keepSB
    unless keep $ do
      CIO.removeFile preprocFile
      case headerFiles of
        [_headerFile] | null header
          -> CIO.removeFile newHeaderFile
        _ -> return () -- keep it since we'll need it to compile the .hs file
    --
    -- expand binding hooks into plain Haskell
    --
    (hsMod, chi, hooksMsgs) <- expandHooks cheader strippedCHSMod
    CIO.putStr hooksMsgs
    --
    -- output the result
    --
    dumpCHS outFPath hsMod True
    dumpCHI outFPath chi           -- different suffix will be appended
  where
    tracePreproc cmd = putTraceStr tracePhasesSW $
                         "Invoking cpp as `" ++ cmd ++ "'...\n"
