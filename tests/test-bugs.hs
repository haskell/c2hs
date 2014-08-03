{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, assert)
import System.FilePath (searchPathSeparator)
import Prelude hiding (FilePath)
import Control.Monad.IO.Class
import Shelly
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as T
import Paths_c2hs
default (T.Text)

main :: IO ()
main = defaultMain tests

c2hsShelly :: MonadIO m => Sh a -> m a
c2hsShelly as = shelly $ do
  oldpath <- get_env_text "PATH"
  let newpath = "../../../dist/build/c2hs:" <> oldpath
  setenv "PATH" newpath
  as

tests :: [Test]
tests =
  [ testGroup "Bugs"
    [ testCase "call_capital (issue #??)" call_capital
    , testCase "Issue #80" issue80
    , testCase "Issue #79" issue79
    , testCase "Issue #75" issue75
    , testCase "Issue #69" issue69
    , testCase "Issue #60" issue60
    , testCase "Issue #51" issue51
    , testCase "Issue #47" issue47
    , testCase "Issue #31" issue31
    , testCase "Issue #30" issue30
    , testCase "Issue #23" issue23
    , testCase "Issue #22" issue22
    , testCase "Issue #54" issue54
    , testCase "Issue #45" issue45
    , testCase "Issue #44" issue44
    , testCase "Issue #43" issue43
    , testCase "Issue #32" issue32
    , testCase "Issue #38" issue38
    , testCase "Issue #29" issue29
    , testCase "Issue #19" issue19
    , testCase "Issue #16" issue16
--    , testCase "Issue #10" issue10
    , testCase "Issue #7" issue7
    ]
  ]

call_capital :: Assertion
call_capital = c2hsShelly $ chdir "tests/bugs/call_capital" $ do
  mapM_ rm_f ["Capital.hs", "Capital.chs.h", "Capital.chi",
              "Capital_c.o", "Capital"]
  cmd "c2hs" "-d" "genbind" "Capital.chs"
  cmd "cc" "-c" "-o" "Capital_c.o" "Capital.c"
  cmd "ghc" "--make" "-cpp" "Capital_c.o" "Capital.hs"
  res <- absPath "./Capital" >>= cmd
  let expected = ["upper C();", "lower c();", "upper C();"]
  liftIO $ assertBool "" (T.lines res == expected)

issue80 :: Assertion
issue80 = build_issue 80

issue79 :: Assertion
issue79 = expect_issue 79 ["A=1", "B=2", "C=2", "D=3"]

issue75 :: Assertion
issue75 = build_issue 75

issue69 :: Assertion
issue69 = build_issue 69

issue60 :: Assertion
issue60 = build_issue 60

issue54 :: Assertion
issue54 = expect_issue 54 ["2", "0.2", "2", "0.2",
                           "3", "0.3", "3", "0.3",
                           "3", "0.3", "3", "0.3"]

issue51 :: Assertion
issue51 = do
  expect_issue_with 51 "nonGNU" [] ["0"]
  expect_issue_with 51 "GNU" [] ["1"]

issue47 :: Assertion
issue47 = build_issue 47

issue45 :: Assertion
issue45 = build_issue 45

issue44 :: Assertion
issue44 = build_issue 44

issue43 :: Assertion
issue43 = expect_issue 43 ["Test1A=0", "Test1B=1", "Test1C=5", "Test1D=6",
                           "AnonA=8", "AnonB=9", "AnonC=15", "AnonD=16"]

issue38 :: Assertion
issue38 = expect_issue 38 ["Enum OK"]

issue32 :: Assertion
issue32 = expect_issue 32 ["1234", "1", "523"]

issue31 :: Assertion
issue31 = expect_issue 31 ["Enum OK",
                           "Pointer 1: 1 1",
                           "Pointer 2: 2",
                           "Foreign pointer: 3",
                           "Foreign newtype pointer: 4"]

-- This is tricky to test since it's Windows-specific, but we can at
-- least make sure that paths with spaces work OK.
issue30 :: Assertion
issue30 = c2hsShelly $ chdir "tests/bugs/issue-30" $ do
  mkdir_p "test 1"
  mkdir_p "test 2"
  mapM_ rm_f ["Issue30.hs", "Issue30.chs.h", "Issue30.chi",
              "Issue30Aux1.hs", "Issue30Aux1.chs.h", "test 1/Issue30Aux1.chi",
              "Issue30Aux2.hs", "Issue30Aux2.chs.h", "test 2/Issue30Aux2.chi",
              "issue30_c.o", "issue30aux1_c.o", "issue30aux2_c.o", "Issue30"]
  cmd "c2hs" "Issue30Aux1.chs"
  mv "Issue30Aux1.chi" "test 1"
  cmd "c2hs" "Issue30Aux2.chs"
  mv "Issue30Aux2.chi" "test 2"
  let sp = T.pack $ "test 1" ++ [searchPathSeparator] ++ "test 2"
  cmd "c2hs" "--include" sp "Issue30.chs"
  cmd "cc" "-c" "-o" "issue30_c.o" "issue30.c"
  cmd "cc" "-c" "-o" "issue30aux1_c.o" "issue30aux1.c"
  cmd "cc" "-c" "-o" "issue30aux2_c.o" "issue30aux2.c"
  cmd "ghc" "--make" "issue30_c.o" "issue30aux1_c.o" "issue30aux2_c.o"
    "Issue30Aux1.hs" "Issue30Aux2.hs" "Issue30.hs"
  res <- absPath "./Issue30" >>= cmd
  let expected = ["3", "2", "4"]
  liftIO $ assertBool "" (T.lines res == expected)

issue29 :: Assertion
issue29 = c2hsShelly $ do
  errExit False $ do
      cd "tests/bugs/issue-29"
      mapM_ rm_f ["Issue29.hs", "Issue29.chs.h", "Issue29.chi"]
      run "c2hs" [toTextIgnore "Issue29.chs"]
  code <- lastExitCode
  liftIO $ assertBool "" (code == 0)

issue23 :: Assertion
issue23 = expect_issue 23 ["H1"]

issue22 :: Assertion
issue22 = expect_issue 22 ["abcdef", "2", "20"]

issue19 :: Assertion
issue19 = expect_issue 19 ["Did it!"]

issue16 :: Assertion
issue16 = build_issue 16

issue10 :: Assertion
issue10 = expect_issue 10 ["SAME", "SAME", "SAME"]

issue7 :: Assertion
issue7 = c2hsShelly $ do
  errExit False $ do
      cd "tests/bugs/issue-7"
      mapM_ rm_f ["Issue7.hs", "Issue7.chs.h", "Issue7.chi"]
      setenv "LANG" "zh_CN.utf8"
      run "c2hs" [toTextIgnore "Issue7.chs"]
  code <- lastExitCode
  liftIO $ assertBool "" (code == 0)

do_issue_build :: Int -> String -> [Text] -> Sh ()
do_issue_build n ext c2hsargs =
  let wdir = "tests/bugs" </> ("issue-" <> show n)
      lc = "issue" <> show n
      lcc = lc <> "_c"
      uc = fromText $ T.pack $ "Issue" <> show n <>
           (if ext == "" then "" else "_" <> ext)
  in do
    cd wdir
    mapM_ rm_f [uc <.> "hs", uc <.> "chs.h", uc <.> "chi", lcc <.> "o", uc]
    run "c2hs" $ c2hsargs ++ [toTextIgnore $ uc <.> "chs"]
    cmd "cc" "-c" "-o" (lcc <.> "o") (lc <.> "c")
    cmd "ghc" "-Wall" "-Werror" "--make" (lcc <.> "o") (uc <.> "hs")

expect_issue :: Int -> [Text] -> Assertion
expect_issue n expected = expect_issue_with n "" [] expected

expect_issue_with :: Int -> String -> [Text] -> [Text] -> Assertion
expect_issue_with n ext c2hsargs expected = c2hsShelly $ do
  do_issue_build n ext c2hsargs
  res <- absPath ("." </> (fromText $ T.pack $ "Issue" <> show n <>
                           (if ext == "" then "" else "_" <> ext))) >>= cmd
  liftIO $ assertBool "" (T.lines res == expected)

build_issue_with :: Int -> [Text] -> Assertion
build_issue_with n c2hsargs = c2hsShelly $ do
  errExit False $ do_issue_build n "" c2hsargs
  code <- lastExitCode
  liftIO $ assertBool "" (code == 0)

build_issue :: Int -> Assertion
build_issue n = build_issue_with n []
