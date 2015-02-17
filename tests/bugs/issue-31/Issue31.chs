module Main where

import Control.Monad
import Foreign
import Foreign.C

#include "issue31.h"

-- CASE 1:
--
-- fromIntegral . fromEnum and toEnum . fromIntegral from an enum hook

{#enum test_enum as TestEnum {underscoreToCase} deriving (Eq, Show)#}
{#fun enum_test {`TestEnum'} -> `TestEnum'#}

enumTest :: IO ()
enumTest = do
  res1 <- enum_test E1
  res2 <- enum_test E2
  res3 <- enum_test E3
  case (res1, res2, res3) of
    (E2, E3, E1) -> putStrLn "Enum OK"
    _            -> putStrLn "Enum FAILED"


-- CASE 2:
--
-- id and id from both naked and newtype pointer hooks

data TestStruct1 = TestStruct1 { a :: Int }
{#pointer *test_struct1 as TestNakedPtr -> TestStruct1#}
{#pointer *test_struct2 as TestNtPtr newtype#}

{#fun make_struct1 as nakedMakeStruct {} -> `TestNakedPtr'#}
{#fun make_struct2 as newtypeMakeStruct {} -> `TestNtPtr'#}
{#fun access_struct1 as nakedAccess {`TestNakedPtr'} -> `Int'#}
{#fun access_struct2 as newtypeAccess {`TestNtPtr'} -> `Int'#}

pointerTest :: IO ()
pointerTest = do
  nakedPtr <- nakedMakeStruct
  nakedVal1 <- nakedAccess nakedPtr
  nakedVal2 <- {#get test_struct1->a#} nakedPtr
  putStrLn $ "Pointer 1: " ++ show nakedVal1 ++ " " ++ show nakedVal2
  newtypePtr <- newtypeMakeStruct
  newtypeVal <- newtypeAccess newtypePtr
  putStrLn $ "Pointer 2: " ++ show newtypeVal


-- CASE 3:
--
-- * withForeignPtr and newForeignPtr_ for foreign pointer hooks

{#pointer *test_struct3 as TestForeignPtr foreign#}
{#fun make_struct3 as foreignMakeStruct {} -> `TestForeignPtr'#}
{#fun access_struct3 as foreignAccess {`TestForeignPtr'} -> `Int'#}

foreignPointerTest :: IO ()
foreignPointerTest = do
  foreignPtr <- foreignMakeStruct
  foreignVal <- foreignAccess foreignPtr
  putStrLn $ "Foreign pointer: " ++ show foreignVal


-- CASE 4:
--
-- * withPointerType (the generated function) and
--   PointerType . newForeignPtr_ for foreign newtype pointer
--   hooks. The out marshaller is not great here, a !ForeignPtr with
--   no finalizers is not terribly useful concealed inside the
--   newtype. Perhaps foreign newtype should be left naked, or
--   furnished with an 'in' default marshaller only.

{#pointer *test_struct4 as TestForeignNtPtr foreign newtype#}
{#fun make_struct4 as foreignNtMakeStruct {} -> `TestForeignNtPtr'#}
{#fun access_struct4 as foreignNtAccess {`TestForeignNtPtr'} -> `Int'#}

foreignNtPointerTest :: IO ()
foreignNtPointerTest = do
  foreignNtPtr <- foreignNtMakeStruct
  foreignNtVal <- foreignNtAccess foreignNtPtr
  putStrLn $ "Foreign newtype pointer: " ++ show foreignNtVal
  return ()


main :: IO ()
main = do
  enumTest
  pointerTest
  foreignPointerTest
  foreignNtPointerTest
  return ()
