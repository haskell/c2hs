module Main where

import Monad (liftM, when)
import Foreign.C

main = do
  size
  alignment

size = do
  let sz1 = {# sizeof S1 #}
  sz1expect <- liftM fromIntegral {# call size_of_s1 #}
  when (sz1 /= sz1expect) $ fail "Fatal: sizeof s1 != size_of_s1()"

  let sz2 = {# sizeof S2 #}
  sz2expect <- liftM fromIntegral {# call size_of_s2 #}
  when (sz2 /= sz2expect) $ fail "Fatal: sizeof s2 != size_of_s2()"

  -- small bitfield in struct gets wrong size, should be sizeof int, c2hs gets 1
  -- http://hackage.haskell.org/trac/c2hs/ticket/10
  let sz3 = {# sizeof S3 #}
  sz3expect <- liftM fromIntegral {# call size_of_s3 #}
  when (sz3 /= sz3expect) $ fail $ "Fatal: sizeof s3 != size_of_s3(): " ++ show sz3 ++ " but expected " ++ show sz3expect

  let sz4 = {# sizeof S4 #}
  sz4expect <- liftM fromIntegral {# call size_of_s4 #}
  when (sz4 /= sz4expect) $ fail $ "Fatal: sizeof s4 != size_of_s4(): " ++ show sz4 ++ " but expected " ++ show sz4expect

  putStrLn $ show sz1 ++ " & "
          ++ show sz2 ++ " & "
          ++ show sz3 ++ " & "
          ++ show sz4

alignment = do
  let al1 = {# alignof S1 #}
  al1expect <- liftM fromIntegral {# call align_of_s1 #}
  when (al1 /= al1expect) $ fail "Fatal: alignment s1 != align_of_s1()"

  let al2 = {# alignof S2 #}
  al2expect <- liftM fromIntegral {# call align_of_s2 #}
  when (al2 /= al2expect) $ fail "Fatal: alignment s2 != align_of_s2()"

  let al3 = {# alignof S3 #}
  al3expect <- liftM fromIntegral {# call align_of_s3 #}
  when (al3 /= al3expect) $ fail $ "Fatal: alignment s3 != align_of_s3(): " ++ show al3 ++ " but expected " ++ show al3expect

  let al4 = {# alignof S4 #}
  al4expect <- liftM fromIntegral {# call align_of_s4 #}
  when (al4 /= al4expect) $ fail $ "Fatal: alignment s4 != align_of_s4(): " ++ show al4 ++ " but expected " ++ show al4expect

  putStrLn $ show al1 ++ " & "
          ++ show al2 ++ " & "
          ++ show al3 ++ " & "
          ++ show al4
