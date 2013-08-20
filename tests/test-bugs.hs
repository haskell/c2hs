{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, assert)
import Shelly
import qualified Data.Text as T
default (T.Text)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Bugs"
    [ testCase "call_capital (issue #??)" call_capital
    ]
  ]

call_capital :: Assertion
call_capital = shelly $ chdir "tests/bugs/call_capital" $ do
  cmd "c2hs" "-d" "genbind" "Capital.chs"
  cmd "cc" "-c" "-o" "Capital_c.o" "Capital.c"
  cmd "ghc" "--make" "-cpp" "Capital_c.o" "Capital.hs"
  res <- absPath "./Capital" >>= cmd
  let expected = ["upper C();", "lower c();", "upper C();"]
  liftIO $ assertBool "" (T.lines res == expected)
