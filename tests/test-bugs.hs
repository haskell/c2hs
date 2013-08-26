{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, assert)
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
