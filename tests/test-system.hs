{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, assert)
import Shelly
import qualified Shelly as Sh
import Prelude hiding (FilePath)
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Paths_c2hs
default (T.Text)

main :: IO ()
main = defaultMain tests

c2hs :: FilePath
c2hs = "../../../dist/build/c2hs/c2hs"

tests :: [Test]
tests =
  [ testGroup "System"
    [ testCase "Calls"   test_calls
    , testCase "Cpp"     test_cpp
    , testCase "Enums"   test_enums
    , testCase "Marsh"   test_marsh
    , testCase "Pointer" test_pointer
    , testCase "Simple"  test_simple
--    , testCase "Sizeof"  test_sizeof    -- KNOWN FAILURE: ISSUE #10
    , testCase "Structs" test_structs
    ]
  ]

run_test_exit_code :: Sh.FilePath -> [(Sh.FilePath, [Text])] -> Assertion
run_test_exit_code dir cmds = shelly $ chdir dir $ do
  forM_ (init cmds) $ \(c, as) -> run c as
  errExit False $ run (fst $ last cmds) (snd $ last cmds)
  code <- lastExitCode
  liftIO $ assertBool "" (code == 0)

run_test_expect :: Sh.FilePath -> [(Sh.FilePath, [Text])] ->
                   Sh.FilePath -> [Text] -> Assertion
run_test_expect dir cmds expcmd expected = shelly $ chdir dir $ do
  forM_ cmds $ \(c, as) -> run c as
  res <- absPath expcmd >>= cmd
  liftIO $ assertBool "" (T.lines res == expected)


test_calls :: Assertion
test_calls = run_test_exit_code "tests/system/calls"
             [(c2hs, ["calls.h", "Calls.chs"]),
              ("ghc", ["-c", "Calls.hs"])]

test_cpp :: Assertion
test_cpp = run_test_exit_code "tests/system/cpp"
           [(c2hs, ["Cpp.chs"]),
            ("ghc", ["-c", "Cpp.hs"])]

test_enums :: Assertion
test_enums = run_test_expect "tests/system/enums"
             [(c2hs, ["enums.h", "Enums.chs"]),
              ("cc", ["-o", "enums_c.o", "-c", "enums.c"]),
              ("ghc", ["-o", "enums", "enums_c.o", "Enums.hs"])]
             "./enums"
             ["Did it!"]

test_marsh :: Assertion
test_marsh = run_test_expect "tests/system/marsh"
             [(c2hs, ["marsh.h", "Marsh.chs"]),
              ("ghc", ["-o", "marsh", "Marsh.hs"])]
             "./marsh"
             ["Hello World!", "[5,3,7]"]

-- Issue #21
test_pointer :: Assertion
test_pointer = run_test_exit_code "tests/system/pointer"
              [(c2hs, ["pointer.h", "Pointer.chs"]),
               ("cc", ["-o", "pointer_c.o", "-c", "pointer.c"]),
               ("ghc", ["-o", "pointer", "pointer_c.o", "Pointer.hs"])]

test_simple :: Assertion
test_simple = run_test_expect "tests/system/simple"
              [(c2hs, ["simple.h", "Simple.chs"]),
               ("ghc", ["-c", "-o", "Simple_hs.o", "Simple.hs"]),
               ("cc", ["-c", "simple.c"]),
               ("ghc", ["-o", "simple", "simple.o", "Simple_hs.o"])]
              "./simple"
              ["I am the mighty foo!"]

-- Issue #10
test_sizeof :: Assertion
test_sizeof = run_test_expect "tests/system/sizeof"
              [(c2hs, ["sizeof.h", "Sizeof.chs"]),
               ("ghc", ["-c", "-o", "Sizeof.o", "Sizeof.hs"]),
               ("cc", ["-o", "sizeof_c.o", "-c", "sizeof.c"]),
               ("ghc", ["-o", "sizeof", "sizeof_c.o", "Sizeof.o"])]
              "./sizeof"
              ["16 & 64 & 4 & 10",
               "8 & 8 & 4 & 4"]

test_structs :: Assertion
test_structs = run_test_expect "tests/system/structs"
               [(c2hs, ["structs.h", "Structs.chs"]),
                ("ghc", ["-c", "-o", "Structs.o", "Structs.hs"]),
                ("cc", ["-o", "structs_c.o", "-c", "structs.c"]),
                ("ghc", ["-o", "structs", "structs_c.o", "Structs.o"])]
               "./structs"
               ["42 & -1 & 2 & 200 & ' '"]
