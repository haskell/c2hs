{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Shelly hiding (FilePath)
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
  parseJSON _ = mzero

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
  tests <- liftIO $ readTests "tests/regression-suite.yaml"
  let ppas = nub $ concatMap aptPPA tests
      pkgs = nub $ concatMap aptPackages tests
      buildTools = nub $ concatMap cabalBuildTools tests
      specials = concatMap specialSetup tests
      extraPaths = concatMap extraPath tests

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
      forM_ specials $ \s -> let (c:as) = T.words s in run_ (fromText c) as
      echo "\n"

    when (not (null extraPaths)) $ do
      echo "ADDING PATHS\n"
      forM_ extraPaths $ \p -> do
        echo p
        appendToPath $ fromText p
      echo "\n"

  codes <- forM (filter cabal tests) $ \t -> do
    let n = name t
        infs = concatMap (\f -> ["-f", f]) $ flags t
    mefs <- get_env $ "C2HS_REGRESSION_FLAGS_" <> n
    let fs = case mefs of
          Nothing -> infs
          Just efs -> infs ++ concatMap (\f -> ["-f", f]) (T.splitOn "," efs)
    echo $ "\nREGRESSION TEST: " <> n <> "\n"
    errExit False $ do
      run_ "cabal" $ ["install", "--jobs=1"] ++ fs ++ [n]
      lastExitCode

  if all (== 0) codes
    then exit 0
    else errorExit "SOME TESTS FAILED"
