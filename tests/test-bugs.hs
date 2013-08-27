{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, assert)
import System.FilePath (searchPathSeparator)
import Shelly
import Data.Text.Lazy (Text)
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
issue47 = shelly $ chdir "tests/bugs/issue-47" $ do
  mapM_ rm_f ["Issue47.hs", "Issue47.chs.h", "Issue47.chi",
              "issue47_c.o", "Issue47"]
  cmd "c2hs" "Issue47.chs"
  cmd "cc" "-c" "-o" "issue47_c.o" "issue47.c"
  errExit False $
    cmd "ghc" "-Wall" "-Werror" "--make" "issue47_c.o" "Issue47.hs"
  code <- lastExitCode
  liftIO $ assertBool "" (code == 0)

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
