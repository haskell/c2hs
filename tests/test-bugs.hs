{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, assert)
import System.FilePath (searchPathSeparator)
import Shelly
import Data.Text.Lazy (Text)
import Data.Monoid
import qualified Data.Text.Lazy as LT
default (LT.Text)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Bugs"
    [ testCase "call_capital (issue #??)" call_capital
    , testCase "Issue #47" issue47
    , testCase "Issue #30" issue30
    , testCase "Issue #22" issue22
    , testCase "Issue #54" issue54
    , testCase "Issue #45" issue45
    , testCase "Issue #44" issue44
    , testCase "Issue #43" issue43
    ]
  ]

call_capital :: Assertion
call_capital = shelly $ chdir "tests/bugs/call_capital" $ do
  mapM_ rm_f ["Capital.hs", "Capital.chs.h", "Capital.chi",
              "Capital_c.o", "Capital"]
  cmd "c2hs" "-d" "genbind" "Capital.chs"
  cmd "cc" "-c" "-o" "Capital_c.o" "Capital.c"
  cmd "ghc" "--make" "-cpp" "Capital_c.o" "Capital.hs"
  res <- absPath "./Capital" >>= cmd
  let expected = ["upper C();", "lower c();", "upper C();"]
  liftIO $ assertBool "" (LT.lines res == expected)

issue47 :: Assertion
issue47 = build_issue 47

-- This is tricky to test since it's Windows-specific, but we can at
-- least make sure that paths with spaces work OK.
issue30 :: Assertion
issue30 = shelly $ chdir "tests/bugs/issue-30" $ do
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
  let sp =  "test 1" ++ [searchPathSeparator] ++ "test 2"
  cmd "c2hs" "--include" sp "Issue30.chs"
  cmd "cc" "-c" "-o" "issue30_c.o" "issue30.c"
  cmd "cc" "-c" "-o" "issue30aux1_c.o" "issue30aux1.c"
  cmd "cc" "-c" "-o" "issue30aux2_c.o" "issue30aux2.c"
  cmd "ghc" "--make" "issue30_c.o" "issue30aux1_c.o" "issue30aux2_c.o"
    "Issue30Aux1.hs" "Issue30Aux2.hs" "Issue30.hs"
  res <- absPath "./Issue30" >>= cmd
  let expected = ["3", "2", "4"]
  liftIO $ assertBool "" (LT.lines res == expected)

issue22 :: Assertion
issue22 = expect_issue 22 ["abcdef", "2", "20"]

issue54 :: Assertion
issue54 = expect_issue 54 ["2", "0.2", "2", "0.2", "3", "0.3", "3", "0.3"]

issue45 :: Assertion
issue45 = build_issue 45

issue44 :: Assertion
issue44 = build_issue 44

issue43 :: Assertion
issue43 = expect_issue 43 ["Test1A=0", "Test1B=1", "Test1C=5", "Test1D=6",
                           "AnonA=8", "AnonB=9", "AnonC=15", "AnonD=16"]


do_issue_build :: Int -> Sh ()
do_issue_build n =
  let wdir = "tests/bugs" </> ("issue-" <> show n)
      lc = "issue" <> show n
      lcc = lc <> "_c"
      uc = fromText $ LT.pack $ "Issue" <> show n
  in do
    cd wdir
    mapM_ rm_f [uc <.> "hs", uc <.> "chs.h", uc <.> "chi", lcc <.> "o", uc]
    cmd "c2hs" $ uc <.> "chs"
    cmd "cc" "-c" "-o" (lcc <.> "o") (lc <.> "c")
    cmd "ghc" "-Wall" "-Werror" "--make" (lcc <.> "o") (uc <.> "hs")

expect_issue :: Int -> [Text] -> Assertion
expect_issue n expected = shelly $ do
  do_issue_build n
  res <- absPath ("." </> (fromText $ LT.pack $ "Issue" <> show n)) >>= cmd
  liftIO $ assertBool "" (LT.lines res == expected)

build_issue :: Int -> Assertion
build_issue n = shelly $ do
  errExit False $ do_issue_build n
  code <- lastExitCode
  liftIO $ assertBool "" (code == 0)
