-- -*-haskell-*-

module Main where

import Control.Monad
import Foreign hiding (unsafePerformIO)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

withCStringLenIntConv :: Num n => String -> ((CString, n) -> IO a) -> IO a
withCStringLenIntConv s f = withCStringLen s $ \(p, n) -> f (p, fromIntegral n)

peekIntConv :: (Storable a, Integral a, Integral b) => Ptr a -> IO b
peekIntConv = liftM fromIntegral . peek

{#context lib="calls"#}

type TString   = {#type tString#}
type MyStringT = {#type MyStringType#}  -- extract a function type

main :: IO ()
main  = do
  let barfoo = {#call fun bar#} {#call fun foo#}
  {#call unsafe baz#} {#call fun foo#} barfoo
  -- BUG !
  {#call printString#} {# call pure  MyString as myString #}
  -- test typedef'ed args without argument variable in prototype
  {#call printString2#} {# call pure MyString as myString #}

{#fun foo as fooFun {} -> `Int'#}

{#fun pure bar as barFun {`Int'} -> `Float'#}

{#fun baz as bazFun {`Int', `Float'} -> `()'#}

{#fun pure MyString as myStringFun {} -> `String'#}

{#fun printString as printStringFun {`String'} -> `()'#}

{#fun foobar {        `String'&             ,
              alloca- `Int'     peekIntConv*,
                      `Float'
             } ->     `Int'#}
