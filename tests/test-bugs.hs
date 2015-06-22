{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, assert)
import System.FilePath (searchPathSeparator)
import System.Info (os)
import Prelude hiding (FilePath)
import Control.Monad.IO.Class
import Shelly
import Data.List (sort)
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

cc :: FilePath
cc = if os == "cygwin32" || os == "mingw32" then "gcc" else "cc"

tests :: [Test]
tests =
  [ testGroup "Bugs" $
    [ testCase "call_capital (issue #??)" call_capital
    , testCase "Issue #7" issue07
    , testCase "Issue #9" issue09
    , testCase "Issue #10" issue10
    , testCase "Issue #15" issue15
    , testCase "Issue #16" issue16
    , testCase "Issue #19" issue19
    , testCase "Issue #20" issue20
    , testCase "Issue #22" issue22
    , testCase "Issue #23" issue23
    , testCase "Issue #25" issue25
    , testCase "Issue #29" issue29
    , testCase "Issue #30" issue30
    , testCase "Issue #31" issue31
    , testCase "Issue #32" issue32
    , testCase "Issue #36" issue36
    , testCase "Issue #38" issue38
    , testCase "Issue #43" issue43
    , testCase "Issue #44" issue44
    , testCase "Issue #45" issue45
    , testCase "Issue #46" issue46
    , testCase "Issue #47" issue47
    , testCase "Issue #51" issue51
    , testCase "Issue #54" issue54
    , testCase "Issue #60" issue60
    , testCase "Issue #62" issue62
    , testCase "Issue #65" issue65
    , testCase "Issue #69" issue69
    , testCase "Issue #70" issue70
    , testCase "Issue #73" issue73
    , testCase "Issue #75" issue75
    , testCase "Issue #79" issue79
    , testCase "Issue #80" issue80
    , testCase "Issue #82" issue82
    , testCase "Issue #93" issue93
    , testCase "Issue #95" issue95
    , testCase "Issue #96" issue96
    , testCase "Issue #97" issue97
    , testCase "Issue #98" issue98
    , testCase "Issue #103" issue103
    , testCase "Issue #107" issue107
    , testCase "Issue #113" issue113
    , testCase "Issue #115" issue115
    , testCase "Issue #116" issue116
    , testCase "Issue #117" issue117
    , testCase "Issue #123" issue123
    , testCase "Issue #127" issue127
    , testCase "Issue #128" issue128
    , testCase "Issue #130" issue130
    , testCase "Issue #131" issue131
    , testCase "Issue #133" issue133
    , testCase "Issue #136" issue136
    ] ++
    -- Some tests that won't work on Windows.
    if os /= "cygwin32" && os /= "mingw32"
    then [ testCase "Issue #48" issue48
         , testCase "Issue #83" issue83
         , testCase "Issue #102" issue102 ]
    else [ ]
  ]

call_capital :: Assertion
call_capital = c2hsShelly $ chdir "tests/bugs/call_capital" $ do
  mapM_ rm_f ["Capital.hs", "Capital.chs.h", "Capital.chi",
              "Capital_c.o", "Capital"]
  cmd "c2hs" "-d" "genbind" "Capital.chs"
  cmd cc "-c" "-o" "Capital_c.o" "Capital.c"
  cmd "ghc" "--make" "-cpp" "Capital_c.o" "Capital.hs"
  res <- absPath "./Capital" >>= cmd
  let expected = ["upper C();", "lower c();", "upper C();"]
  liftIO $ assertBool "" (T.lines res == expected)

issue136 :: Assertion
issue136 = build_issue_tolerant 136

issue133 :: Assertion
issue133 = hs_only_build_issue 133

issue131 :: Assertion
issue131 = c2hsShelly $ chdir "tests/bugs/issue-131" $ do
  mapM_ rm_f ["Issue131.hs", "Issue131.chs.h", "Issue131.chs.c", "Issue131.chi",
              "issue131_c.o", "Issue131.chs.o", "Issue131"]
  cmd "c2hs" "Issue131.chs"
  cmd cc "-c" "-o" "issue131_c.o" "issue131.c"
  cmd cc "-c" "Issue131.chs.c"
  cmd "ghc" "--make" "issue131_c.o" "Issue131.chs.o" "Issue131.hs"
  res <- absPath "./Issue131" >>= cmd
  let expected = ["5", "3",
                  "True", "False"]
  liftIO $ assertBool "" (T.lines res == expected)

issue130 :: Assertion
issue130 = expect_issue 130  ["3", "3"]

issue128 :: Assertion
issue128 = c2hsShelly $ chdir "tests/bugs/issue-128" $ do
  mapM_ rm_f ["Issue128.hs", "Issue128.chs.h", "Issue128.chs.c", "Issue128.chi",
              "issue128_c.o", "Issue128.chs.o", "Issue128"]
  cmd "c2hs" "Issue128.chs"
  cmd cc "-c" "-o" "issue128_c.o" "issue128.c"
  cmd cc "-c" "Issue128.chs.c"
  cmd "ghc" "--make" "issue128_c.o" "Issue128.chs.o" "Issue128.hs"
  res <- absPath "./Issue128" >>= cmd
  let expected = ["5", "3",
                  "True", "False",
                  "10", "False",
                  "12", "True",
                  "7", "False",
                  "8", "True"]
  liftIO $ assertBool "" (T.lines res == expected)

issue127 :: Assertion
issue127 = expect_issue 127  ["True", "False"]

issue125 :: Assertion
issue125 = expect_issue 125  ["NYI"]

issue123 :: Assertion
issue123 = expect_issue 123  ["[8,43,94]", "[7,42,93]", "[2,4,8]", "[3,9,27]"]

issue117 :: Assertion
issue117 = c2hsShelly $ chdir "tests/bugs/issue-117" $ do
  mapM_ rm_f ["Issue117.hs", "Issue117.chs.h", "Issue117.chs.c", "Issue117.chi",
              "issue117_c.o", "Issue117.chs.o", "Issue117"]
  cmd "c2hs" "Issue117.chs"
  cmd cc "-c" "-o" "issue117_c.o" "issue117.c"
  cmd cc "-c" "Issue117.chs.c"
  cmd "ghc" "--make" "issue117_c.o" "Issue117.chs.o" "Issue117.hs"
  res <- absPath "./Issue117" >>= cmd
  let expected = ["5"]
  liftIO $ assertBool "" (T.lines res == expected)

issue116 :: Assertion
issue116 = build_issue 116

issue115 :: Assertion
issue115 = expect_issue 115 ["[8,43,94]", "[7,42,93]"]

issue113 :: Assertion
issue113 = build_issue 113

issue107 :: Assertion
issue107 = hs_only_expect_issue 107 True ["True"]

issue103 :: Assertion
issue103 = c2hsShelly $ chdir "tests/bugs/issue-103" $ do
  mapM_ rm_f ["Issue103.hs", "Issue103.chs.h", "Issue103.chi",
              "Issue103A.hs", "Issue103A.chs.h", "Issue103A.chi",
              "issue103_c.o", "Issue103"]
  cmd "c2hs" "Issue103A.chs"
  cmd "c2hs" "Issue103.chs"
  cmd cc "-c" "-o" "issue103_c.o" "issue103.c"
  cmd "ghc" "--make" "issue103_c.o" "Issue103A.hs" "Issue103.hs"
  res <- absPath "./Issue103" >>= cmd
  let expected = ["1", "2", "3"]
  liftIO $ assertBool "" (T.lines res == expected)

issue102 :: Assertion
issue102 = hs_only_expect_issue 102 False ["TST 1: 1234",
                                           "TST 2: 13 47",
                                           "TST 3: testing",
                                           "Unlocked"]

issue98 :: Assertion
issue98 = build_issue 98

issue97 :: Assertion
issue97 = c2hsShelly $ chdir "tests/bugs/issue-97" $ do
  mapM_ rm_f ["Issue97.hs", "Issue97.chs.h", "Issue97.chi",
              "Issue97A.hs", "Issue97A.chs.h", "Issue97A.chi",
              "issue97_c.o", "Issue97"]
  cmd "c2hs" "Issue97A.chs"
  cmd "c2hs" "Issue97.chs"
  cmd cc "-c" "-o" "issue97_c.o" "issue97.c"
  cmd "ghc" "--make" "issue97_c.o" "Issue97A.hs" "Issue97.hs"
  res <- absPath "./Issue97" >>= cmd
  let expected = ["42"]
  liftIO $ assertBool "" (T.lines res == expected)

issue96 :: Assertion
issue96 = build_issue 96

issue95 :: Assertion
issue95 = build_issue 95

issue93 :: Assertion
issue93 = build_issue_tolerant 93

issue82 :: Assertion
issue82 = hs_only_build_issue 82

issue83 :: Assertion
issue83 = hs_only_expect_issue 83 True ["(True,True)", "TEST_VAL",
                                        "8415", "8415", "TESTING"]

issue80 :: Assertion
issue80 = build_issue 80

issue79 :: Assertion
issue79 = expect_issue 79 ["A=1", "B=2", "C=2", "D=3"]

issue75 :: Assertion
issue75 = build_issue 75

issue73 :: Assertion
issue73 = unordered_expect_issue 73 [ "Allocated struct3"
                                    , "Foreign pointer: 3"
                                    , "Allocated struct3"
                                    , "Foreign pointer: 3"
                                    , "Allocated struct4"
                                    , "Foreign newtype pointer: 4"
                                    , "Allocated struct4"
                                    , "Foreign newtype pointer: 4"
                                    , "Freeing struct3"
                                    , "Freeing struct4" ]

issue70 :: Assertion
issue70 = build_issue 70

issue69 :: Assertion
issue69 = build_issue 69

issue65 :: Assertion
issue65 = expect_issue 65 ["123", "3.14", "\"hello\""]

issue62 :: Assertion
issue62 = build_issue 62

issue60 :: Assertion
issue60 = build_issue 60

issue54 :: Assertion
issue54 = expect_issue 54 ["2", "0.2", "2", "0.2",
                           "3", "0.3", "3", "0.3",
                           "3", "0.3", "3", "0.3"]

issue51 :: Assertion
issue51 = do
  expect_issue_with True True 51 "nonGNU" [] ["0"]
  expect_issue_with True True 51 "GNU" [] ["1"]

issue48 :: Assertion
issue48 = expect_issue 48 ["2", "5"]

issue47 :: Assertion
issue47 = build_issue 47

issue46 :: Assertion
issue46 = expect_issue 46 ["(1,2.5)"]

issue45 :: Assertion
issue45 = build_issue 45

issue44 :: Assertion
issue44 = build_issue 44

issue43 :: Assertion
issue43 = expect_issue 43 ["Test1A=0", "Test1B=1", "Test1C=5", "Test1D=6",
                           "AnonA=8", "AnonB=9", "AnonC=15", "AnonD=16"]

issue38 :: Assertion
issue38 = expect_issue 38 ["Enum OK"]

issue36 :: Assertion
issue36 = hs_only_build_issue 36

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
  cmd cc "-c" "-o" "issue30_c.o" "issue30.c"
  cmd cc "-c" "-o" "issue30aux1_c.o" "issue30aux1.c"
  cmd cc "-c" "-o" "issue30aux2_c.o" "issue30aux2.c"
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

issue25 :: Assertion
issue25 = hs_only_expect_issue 25 True ["-1", "abcdef"]

issue23 :: Assertion
issue23 = expect_issue 23 ["H1"]

issue22 :: Assertion
issue22 = expect_issue 22 ["abcdef", "2", "20"]

issue20 :: Assertion
issue20 = expect_issue 20 ["4"]

issue19 :: Assertion
issue19 = expect_issue 19 ["Did it!"]

issue16 :: Assertion
issue16 = build_issue 16

issue15 :: Assertion
issue15 = expect_issue 15 ["True"]

issue10 :: Assertion
issue10 = expect_issue 10 ["SAME", "SAME", "SAME", "SAME"]

issue09 :: Assertion
issue09 = expect_issue 9 $ archdep ++ ["(32,64)", "64", "OK"]
  where archdep
          | (maxBound::Int) == 2147483647 = ["PTA:4", "AOP:16"] -- 32 bit
          | otherwise =                     ["PTA:8", "AOP:32"] -- 64 bit

issue07 :: Assertion
issue07 = c2hsShelly $ do
  errExit False $ do
      cd "tests/bugs/issue-7"
      mapM_ rm_f ["Issue7.hs", "Issue7.chs.h", "Issue7.chi"]
      setenv "LANG" "zh_CN.utf8"
      run "c2hs" [toTextIgnore "Issue7.chs"]
  code <- lastExitCode
  liftIO $ assertBool "" (code == 0)

do_issue_build :: Bool -> Bool -> Int -> String -> [Text] -> Sh ()
do_issue_build strict cbuild n ext c2hsargs =
  let wdir = "tests/bugs" </> ("issue-" <> show n)
      lc = "issue" <> show n
      lcc = lc <> "_c"
      uc = fromText $ T.pack $ "Issue" <> show n <>
           (if ext == "" then "" else "_" <> ext)
  in do
    cd wdir
    mapM_ rm_f [uc <.> "hs", uc <.> "chs.h", uc <.> "chi", lcc <.> "o", uc]
    run "c2hs" $ c2hsargs ++ [toTextIgnore $ uc <.> "chs"]
    when cbuild $ cmd cc "-c" "-o" (lcc <.> "o") (lc <.> "c")
    case (strict, cbuild) of
      (True, True) ->
        cmd "ghc" "-Wall" "-Werror" "--make" (lcc <.> "o") (uc <.> "hs")
      (False, True) ->
        cmd "ghc" "--make" (lcc <.> "o") (uc <.> "hs")
      (True, False) ->
        cmd "ghc" "-Wall" "-Werror" "--make" (uc <.> "hs")
      (False, False) ->
        cmd "ghc" "--make" (uc <.> "hs")

expect_issue :: Int -> [Text] -> Assertion
expect_issue n expected = expect_issue_with True True n "" [] expected

unordered_expect_issue :: Int -> [Text] -> Assertion
unordered_expect_issue n expected =
  expect_issue_with False True n "" [] expected

hs_only_expect_issue :: Int -> Bool -> [Text] -> Assertion
hs_only_expect_issue n ordered expected =
  expect_issue_with ordered False n "" [] expected

expect_issue_with :: Bool -> Bool -> Int -> String -> [Text] -> [Text]
                  -> Assertion
expect_issue_with ordered cbuild n ext c2hsargs expected = c2hsShelly $ do
  do_issue_build True cbuild n ext c2hsargs
  res <- absPath ("." </> (fromText $ T.pack $ "Issue" <> show n <>
                           (if ext == "" then "" else "_" <> ext))) >>= cmd
  liftIO $ assertBool "" $ case ordered of
    True -> T.lines res == expected
    False -> sort (T.lines res) == sort expected

build_issue_with :: Bool -> Bool -> Int -> [Text] -> Assertion
build_issue_with strict cbuild n c2hsargs = c2hsShelly $ do
  errExit False $ do_issue_build strict cbuild n "" c2hsargs
  code <- lastExitCode
  liftIO $ assertBool "" (code == 0)

build_issue :: Int -> Assertion
build_issue n = build_issue_with True True n []

build_issue_tolerant :: Int -> Assertion
build_issue_tolerant n = build_issue_with False True n []

hs_only_build_issue :: Int -> Assertion
hs_only_build_issue n = build_issue_with True False n []
