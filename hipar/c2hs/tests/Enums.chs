-- -*-haskell-*-
import Monad
import C2HS

{#context prefix="enums"#}

{#enum colour as Colour {underscoreToCase}#}

{#enum weird as Weird {underscoreToCase}#}

{#enum side as Side {underscoreToCase}#}

{#enum other_side as OtherSide {}#}

{#enum enum_net_type as NetType {underscoreToCase}#}

{#enum enums_enums as Enums {underscoreToCase, ENUMS_TWO as Two}#}

colourOfSide :: Side -> Colour
colourOfSide  = 
  cToEnum . {#call fun colourOfSide as colourOfSidePrim#} . cFromEnum

main :: IO () 
main  = do
	  const (return ()) discard
	  unless (1 == fromEnum One) $
	    putStrLn "1 /= One!!!"
	  putStrLn "Did it!"
	where
	  -- is not executed, only type checked
	  discard = {#get NET.nettype#} undefined :: IO CInt
