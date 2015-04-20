{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Shelly hiding (FilePath)
import Data.Char
import Data.List (nub)
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as T
import Data.Yaml
default (T.Text)

data RegressionTest = RegressionTest
                      { name :: Text
                      , cabal :: Bool
                      , flags :: [Text]
                      , aptPPA :: [Text]
                      , aptPackages :: [Text]
                      , cabalBuildTools :: [Text]
                      , specialSetup :: [Text]
                      , extraPath :: [Text]
                      , extraSOPath :: [Text]
                      , extraIncludeDirs :: [Text]
                      , extraLibDirs :: [Text]
                      , onTravis :: Bool
                      , runTests :: Bool
                      } deriving (Eq, Show)

instance FromJSON RegressionTest where
  parseJSON (Object v) = RegressionTest <$> v .: "name"
                                        <*> v .:? "cabal" .!= True
                                        <*> v .:? "flags" .!= []
                                        <*> v .:? "apt-ppa" .!= []
                                        <*> v .:? "apt-packages" .!= []
                                        <*> v .:? "cabal-build-tools" .!= []
                                        <*> v .:? "special-setup" .!= []
                                        <*> v .:? "extra-path" .!= []
                                        <*> v .:? "extra-so-path" .!= []
                                        <*> v .:? "extra-include-dirs" .!= []
                                        <*> v .:? "extra-lib-dirs" .!= []
                                        <*> v .:? "on-travis" .!= True
                                        <*> v .:? "run-tests" .!= False
  parseJSON _ = mzero

data Code = TestOK
          | DepsFailed
          | ConfFailed
          | BuildFailed
          | TestsFailed
          deriving Eq

instance Show Code where
  show TestOK = "OK"
  show DepsFailed = "dependencies"
  show ConfFailed = "configuration"
  show BuildFailed = "build"
  show TestsFailed = "tests"

makeCode :: (Int, Int, Int, Int) -> Code
makeCode (0, 0, 0, 0) = TestOK
makeCode (0, 0, 0, _) = TestsFailed
makeCode (0, 0, _, _) = BuildFailed
makeCode (0, _, _, _) = ConfFailed
makeCode (_, _, _, _) = DepsFailed

readTests :: FilePath -> IO [RegressionTest]
readTests fp = maybe [] id <$> decodeFile fp

checkApt :: Sh ()
checkApt = do
  apt <- which "apt-get"
  case apt of
    Nothing -> errorExit "Can't find apt-get.  Are you sure this is Ubuntu?"
    _ -> return ()

main :: IO ()
main = shelly $ do
  travis <- maybe False (const True) <$> get_env "TRAVIS"
  enabled <- maybe False (const True) <$> get_env "C2HS_REGRESSION_SUITE"
  when (not (travis || enabled)) $ do
    echo "REGRESSION SUITE IS DISABLED"
    exit 0

  when travis checkApt
  let travisCheck t = case travis of
        False -> True
        True -> onTravis t
  tests <- liftIO $ filter travisCheck <$>
           readTests "tests/regression-suite.yaml"
  let ppas = nub $ concatMap aptPPA tests
      pkgs = nub $ concatMap aptPackages tests
      buildTools = nub $ concatMap cabalBuildTools tests
      specials = concatMap specialSetup tests
      extraPaths = concatMap extraPath tests
      extraSOPaths = concatMap extraSOPath tests

  when (not travis) $
    echo "ASSUMING THAT ALL NECESSARY LIBRARIES ALREADY INSTALLED!\n"

  home <- fromText <$> get_env_text "HOME"
  appendToPath $ home </> ".cabal/bin"

  when travis $ do
    when (not (null ppas)) $ do
      echo "SETTING UP APT PPAS\n"
      forM_ ppas $ \ppa -> run_ "sudo" $ ["apt-add-repository", "ppa:" <> ppa]
      run_ "sudo" $ ["apt-get", "update"]
      echo "\n"

    when (not (null pkgs)) $ do
      echo "INSTALLING APT PACKAGES\n"
      run_ "sudo" $ ["apt-get", "install", "-y"] ++ pkgs
      echo "\n"

    when (not (null specials)) $ do
      echo "SPECIAL INSTALL STEPS\n"
      forM_ specials $ \s -> let (c:as) = escapedWords s in
        run_ (fromText c) as
      echo "\n"

    when (not (null extraPaths)) $ do
      echo "ADDING PATHS\n"
      forM_ extraPaths $ \p -> do
        echo p
        appendToPath $ fromText p
      echo "\n"

    when (not (null extraSOPaths)) $ do
      echo "ADDING SHARED LIBRARY PATHS\n"
      forM_ extraSOPaths $ \p -> do
        echo p
        appendToSOPath p
      echo "\n"

  codes <- forM (filter cabal tests) $ \t -> do
    let n = name t
        tst = runTests t
        infs = concatMap (\f -> ["-f", f]) $ flags t
        extralibs = map (\f -> "--extra-lib-dirs=" <> f) $
                    extraLibDirs t
        extraincs = map (\f -> "--extra-include-dirs=" <> f) $
                    extraIncludeDirs t
    mefs <- get_env $ "C2HS_REGRESSION_FLAGS_" <> n
    let fs = if tst then ["--enable-tests"] else [] ++ case mefs of
          Nothing -> infs
          Just efs -> infs ++ concatMap (\f -> ["-f", f]) (T.splitOn "," efs)
    echo $ "\nREGRESSION TEST: " <> n <> "\n"
    errExit False $ do
      unpack <- run "cabal" ["unpack", n]
      let d = T.drop (T.length "Unpacking to ") $ T.init $ last $ T.lines unpack
      chdir (fromText d) $ do
        run_ "cabal" $ ["sandbox", "init"]
        run_ "cabal" $ ["install", "--only-dep", "-v"] ++ fs
        dep <- lastExitCode
        run_ "cabal" $ ["configure"] ++ extraincs ++ extralibs ++ fs
        conf <- lastExitCode
        run_ "cabal" $ ["build"]
        build <- lastExitCode
        test <-
          if tst then do
            run_ "cabal" ["test"]
            lastExitCode
          else return 0
        return $ makeCode (dep, conf, build, test)

  if all (== TestOK) codes
    then exit 0
    else do
    echo "\n\nSOME TESTS FAILED\n"
    let failed = filter (\(c, _) -> c /= TestOK) $ zip codes (filter cabal tests)
    forM_ failed $ \(c, t) -> echo $ "FAILED: " <> name t <>
                                     " (" <> T.pack (show c) <> ")"
    exit 1

escapedWords :: Text -> [Text]
escapedWords = map (T.pack . reverse) . escWords False "" . T.unpack
  where escWords :: Bool -> String -> String -> [String]
        -- End of string: just return the accumulator if there is one.
        escWords _ acc "" = case acc of
          "" -> []
          _  -> [acc]
        -- Not escaping.
        escWords False acc (c:cs)
          | isSpace c = acc : escWords False "" cs
          | c == '\'' = case acc of
            "" -> escWords True "" cs
            _  -> acc : escWords True "" cs
          | otherwise = escWords False (c:acc) cs
        -- Escaping.
        escWords True acc (c:cs)
          | c == '\'' = acc : escWords False "" cs
          | otherwise = escWords True (c:acc) cs

appendToSOPath :: Text -> Sh ()
appendToSOPath tp = do
  pe <- get_env_text "LD_LIBRARY_PATH"
  setenv "LD_LIBRARY_PATH" $ pe <> ":" <> tp
