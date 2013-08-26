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
    , testCase "Issue #51" issue51
    ]
  ]

call_capital :: Assertion
call_capital = shelly $ chdir "tests/bugs/call_capital" $ do
  cmd "c2hs" "-d" "genbind" "Capital.chs"
  cmd "cc" "-c" "-o" "Capital_c.o" "Capital.c"
  cmd "ghc" "--make" "-cpp" "Capital_c.o" "Capital.hs"
  res <- absPath "./Capital" >>= cmd
  let expected = ["upper C();", "lower c();", "upper C();"]
  liftIO $ assertBool "" (LT.lines res == expected)

issue51 :: Assertion
issue51 = shelly $ chdir "tests/bugs/issue-51" $ do
  cmd "c2hs" "Issue51.chs"
  cmd "cc" "-c" "-o" "issue51_c.o" "issue51.c"
  cmd "ghc" "--make" "issue51_c.o" "Issue51.hs"
  res <- absPath "./Issue51" >>= cmd
  liftIO $ assertBool "" (LT.lines res == ["0"])
