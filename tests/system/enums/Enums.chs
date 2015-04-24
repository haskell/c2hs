-- -*-haskell-*-
import Control.Monad
import Foreign
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

cToEnum :: (Integral i, Enum e) => i -> e
cToEnum  = toEnum . fromIntegral

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum  = fromIntegral . fromEnum

{#context prefix="enums"#}

{#enum colour as Colour {upcaseFirstLetter}#}

{#enum weird as Weird {underscoreToCase}#}

{#enum side as Side {underscoreToCase}#}

{#enum other_side as OtherSide {}#}

{#enum enum_net_type as NetType {underscoreToCase}#}

{#enum enums_enums as Enums {underscoreToCase, ENUMS_TWO as Two}#}

colourOfSide :: Side -> Colour
colourOfSide  =
  cToEnum . {#call fun colourOfSide as colourOfSidePrim#} . cFromEnum

#c
enum ThisThat {
  This = THIS,
  That = THAT
};

enum ThisThatCast {
  CThis = C_THIS,
  CThat = C_THAT
};
#endc
{#enum ThisThat {}#}
{#enum ThisThatCast {}#}


main :: IO ()
main  = do
	  const (return ()) discard
	  unless (1 == fromEnum One) $
	    putStrLn "1 /= One!!!"
	  putStrLn "Did it!"
	where
	  -- is not executed, only type checked
	  discard = {#get NET.nettype#} undefined :: IO CInt
