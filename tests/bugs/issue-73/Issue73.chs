module Main where

#include "issue73.h"

-- * withForeignPtr and newForeignPtr_ for foreign pointer hooks

{#pointer *test_struct3 as TestForeign1Ptr foreign#}
{#fun make_struct3 as foreign1MakeStruct {} -> `TestForeign1Ptr'#}
{#fun access_struct3 as foreign1Access {`TestForeign1Ptr'} -> `Int'#}

foreign1 :: IO ()
foreign1 = do
  foreignPtr <- foreign1MakeStruct
  foreignVal <- foreign1Access foreignPtr
  putStrLn $ "Foreign pointer: " ++ show foreignVal


{#pointer *test_struct3 as TestForeign2Ptr foreign finalizer free_struct3#}
{#fun make_struct3 as foreign2MakeStruct {} -> `TestForeign2Ptr'#}
{#fun access_struct3 as foreign2Access {`TestForeign2Ptr'} -> `Int'#}

foreign2 :: IO ()
foreign2 = do
  foreignPtr <- foreign2MakeStruct
  foreignVal <- foreign2Access foreignPtr
  putStrLn $ "Foreign pointer: " ++ show foreignVal


-- * withPointerType (the generated function) and
--   PointerType . newForeignPtr_ for foreign newtype pointer
--   hooks. The out marshaller is not great here, a !ForeignPtr with
--   no finalizers is not terribly useful concealed inside the
--   newtype. Perhaps foreign newtype should be left naked, or
--   furnished with an 'in' default marshaller only.

{#pointer *test_struct4 as TestForeignNt1Ptr foreign newtype#}
{#fun make_struct4 as foreignNt1MakeStruct {} -> `TestForeignNt1Ptr'#}
{#fun access_struct4 as foreignNt1Access {`TestForeignNt1Ptr'} -> `Int'#}

foreignNt1 :: IO ()
foreignNt1 = do
  foreignNtPtr <- foreignNt1MakeStruct
  foreignNtVal <- foreignNt1Access foreignNtPtr
  putStrLn $ "Foreign newtype pointer: " ++ show foreignNtVal
  return ()


{#pointer *test_struct4 as TestForeignNt2Ptr
                           foreign finalizer free_struct4 newtype#}
{#fun make_struct4 as foreignNt2MakeStruct {} -> `TestForeignNt2Ptr'#}
{#fun access_struct4 as foreignNt2Access {`TestForeignNt2Ptr'} -> `Int'#}

foreignNt2 :: IO ()
foreignNt2 = do
  foreignNtPtr <- foreignNt2MakeStruct
  foreignNtVal <- foreignNt2Access foreignNtPtr
  putStrLn $ "Foreign newtype pointer: " ++ show foreignNtVal
  return ()


main :: IO ()
main = do
  foreign1
  foreign2
  foreignNt1
  foreignNt2
  return ()
