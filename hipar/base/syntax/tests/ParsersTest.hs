-- test `Parsers'
--

module Main
where

import Common
import UNames
import Errors
import Attributes
import Parsers
import State

{- for Hugs
type Attrs = ()
newAttrs _ _ = ()
type PreCST e s a = IO a
type Name = Int
getNameSupply :: IO Int
getNameSupply = return 1
names :: Int -> [Int]
names x = [x..]
putStrCIO = putStr
run _ _ m = m
printCIO :: Show a => a -> IO ()
printCIO = print
-}

data CharTok = CharTok Char Position

instance Pos CharTok where
  posOf (CharTok _ pos) = pos

instance Eq CharTok where
  (CharTok c1 _) == (CharTok c2 _) = c1 == c2

instance Show CharTok where
  showsPrec _ (CharTok c _) = showString [c]

instance Token CharTok

stringToCharToks   :: String -> [CharTok]
stringToCharToks s  = 
  cts 1 s
  where
    cts _ []     = []
    cts n (c:cs) = CharTok c ("<string>", -1, n) : cts (n + 1) cs

instance Show Attrs where
  showsPrec _ _ = showString "<attrs>"

-- simple expression grammar
--
data Expr = Var  String    Attrs
	  | Add  Expr Expr Attrs
	  | Mul  Expr Expr Attrs
	  deriving (Show)

{-
expr  =  prod *> expr' `action` glueExp
expr' = chars '+' -*> prod *> expr' `action` (Just . glueExp)
        `opt` Nothing

glueExp (p, e') = (mapMaybe (\e -> Add p e) p) e'

prod  = prim *> prod' `action` glueProd
prod' = chars '*' -*> prim *> prod' `action` (Just . glueProd)
        `opt` Nothing

glueProd (r, p') = (mapMaybe (\p -> Mul r p) r) p'

-}

expr = sep1 (\l a r -> Add l r a) (chara '+') prod

prod = sep1 (\l a r -> Mul l r a) (chara '*') prim

prim =     var 
       <|> chars '(' -*> expr *-> chars ')'

--var = list1 alphaNum `action` Var
var = alpha' *> many (:) [] alphaNum `action` \((c, at), cs) -> Var (c:cs) at
      where
        alpha' = alpha *> meta getName 
		 `action` \(CharTok c pos, n) -> (c, newAttrs pos n)
--		 `action` \(CharTok c pos) -> (c, newAttrsOnlyPos pos)

alpha    = foldr1 (<|>) (map char0 (['a'..'z']))
alphaNum = foldr1 (<|>) (map char (['a'..'z'] ++ ['0'..'9']))

--char c = const c $> skip (CharTok c nopos)
--chars c = skip (CharTok c nopos)
char0 c = token (CharTok c nopos)
char c = const c $> token (CharTok c nopos)

chara c = token (CharTok c nopos) *> meta getName
	  `action` \(CharTok _ pos, n) -> newAttrs pos n
--	  `action` \(CharTok _ pos) -> newAttrsOnlyPos pos

chars c = const () $> token (CharTok c nopos)

mapMaybe f e Nothing  = e
mapMaybe f e (Just x) = f x

getName (n:ns) = (ns, n)

--vars = many (const (1+)) 0 (char 'x')
--vars = sep1 (++) (token (CharTok '-' nopos)) ((\c -> [c]) $> char 'x')
--vars = seplist1 (token (CharTok '-' undefined)) (token (CharTok 'x' undefined)) 

{-
testparse    :: String  -> IO ()
testparse cs  = let (tree, errs) = (execParser expr . stringToCharToks) cs
--testparse cs  = let (tree, errs) = (execParser vars . stringToCharToks) cs
		in do
		  print tree
		  mapM (putStr . showError) errs
		  return ()
 -}


parse    :: Parser [Name] CharTok t -> String  -> PreCST e s (t, [CharTok])
parse p cs  = 
  do
    nsupp <- getNameSupply
    let ns = names nsupp
	(tree, errs, r) = (execParser p ns . stringToCharToks) cs
    mapM (putStrCIO . showError) errs
    return (tree, r)


main :: IO ()
main  = run ("parser test", "", "") undefined doIt

doIt :: PreCST e s ()
doIt  = let cs = "a+b*(xy+abcdefg)"
        in do
	  putStrCIO "Parsers test...\n"
	  putStrCIO "===============\n"
          (tree, _) <- parse expr cs
          printCIO tree
	  putStrCIO "Should output: \
		    \Mul (Var \"a\" <attrs>) (Add (Var \"b\" <attrs>) \
		    \(Mul (Var \"xy\" <attrs>) (Var \"abcdefg\" <attrs>) \
		    \<attrs>) <attrs>) <attrs>\n\n"
          ((abc, dot), rest) <- parse (list alpha *> char0 '.') "abc.junk"
	  let stringify = concat . map show
	  putStrCIO ("`list alpha *> char0 '.'' accepts from `abc.junk' \
		     \the prefix `" ++ stringify (abc ++ [dot])
		     ++ "',\nand the rest is `" ++ stringify rest 
		     ++ "'.\n")
