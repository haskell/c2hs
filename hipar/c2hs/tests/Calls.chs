module Main
where

import C2HS

{#context lib="calls"#}

type TString = {#type tString#}

main :: IO ()
main  = do
	  let barfoo = {#call fun bar#} {#call fun foo#}
	  {#call unsafe baz#} {#call fun foo#} barfoo
	  {#call printString#} {#call fun MyString as myString#}
	  -- test typedef'ed args without argument variable in prototype
	  {#call printString2#} {#call fun MyString as myString#}
