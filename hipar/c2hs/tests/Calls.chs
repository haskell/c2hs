-- -*-haskell-*-

module Main
where

import Monad
import C2HS

{#context lib="calls"#}

type TString   = {#type tString#}
type MyStringT = {#type MyStringType#}  -- extract a function type

main :: IO ()
main  = do
	  let barfoo = {#call fun bar#} {#call fun foo#}
	  {#call unsafe baz#} {#call fun foo#} barfoo
	  {#call printString#} {#call fun MyString as myString#}
	  -- test typedef'ed args without argument variable in prototype
	  {#call printString2#} {#call fun MyString as myString#}

{#fun foo as fooFun {} -> `Int'#}

{#fun pure bar as barFun {`Int'} -> `Float'#}

{#fun baz as bazFun {`Int', `Float'} -> `()'#}

{#fun MyString as myStringFun {} -> `String'#}

{#fun printString as printStringFun {`String'} -> `()'#}

{#fun foobar {        `String'&             , 
	      alloca- `Int'     peekIntConv*, 
		      `Float'
	     } ->     `Int'#}
