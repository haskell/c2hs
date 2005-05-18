--  Compiler Toolkit: pretty-printer combinators
--
--  Author : Manuel M. T. Chakravarty
--  Created: 16 February 95
--
--  Copyright (c) [1995..2000] Manuel M. T. Chakravarty
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  This module provides combinators for pretty-printing, following the ideas
--  in ``Pretty-printing: An Exercise in Functional Programming (DRAFT)'' by
--  John Hughes.  Subsequently, partially brought in line with Simon Peyton
--  Jones' version of John Hughes' combinators.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * In a revision of the module, the names of the exported functions where
--    brought in line with SimonPJs variant.  The old names are still exported
--    as to maintain compatibility to older code.  They will disappear
--    somewhere down the road.
--
--  * The type of `fullRender' is different from the one in SimonPJ's variant.
--
--  * `toDoc' is not supported by SimonPJ's variant.
--
--  * The combinators `($+$)', `fcat', and `fsep' are not supported.
--
--- TODO ----------------------------------------------------------------------
--
--  * currently `$|$' imposes a n^2 cost when building a text from top to
--    bottom
--
 
module Pretty (
  Doc, -- instance Show
  empty, isEmpty, char, text, nest, ($$), (<>), cat, sep, fullRender,
  --
  -- derived combinators
  --
  semi, comma, colon, dot, space, equals, lparen, rparen, lbrack, rbrack,
  lbrace, rbrace, toDoc, int, integer, float, double, rational, parens,
  brackets, braces, quotes, doubleQuotes, (<+>), hcat, hsep, vcat, hang,
  punctuate, render,
  --
  -- pretty-printing type class & precedences
  --
  Pretty(pretty, prettyPrec), usedWhen, Assoc(..), infixOp,
  --
  -- the following routines are part of the legacy interface that should not
  -- be used anymore - it will disappear in due course
  --
  textDoc, nestDoc, ($|$), (<^>), sepDocs, bestDoc,
  --
  -- *** for debugging ONLY ***
  --
  dumpDoc
) where


infixl 6 <>, <+>	-- vertical composition
infixl 5 $$		-- horizontal composition


-- default parameters
-- ------------------

dftWidth :: Int
dftWidth  = 79

dftRibbonRatio :: Float
dftRibbonRatio  = 1.5


-- representation of documents
-- ---------------------------

-- a document is a compact representation (tree shaped) of a set of layouts 
-- for a given text (EXPORTED ABSTRACTLY)
--
data Doc    = Nest      Int [DocAlt]  -- set of layouts, indented as given
data DocAlt = Text      String	      -- one row
	    | TextAbove String Doc    -- row of text above the remaining doc

-- render with defaults
--
instance Show Doc where
  showsPrec _ = showString . render

-- empty document (EXPORTED)
--
empty :: Doc
empty  = Nest 0 []

-- test for emptiness (EXPORTED)
--
isEmpty             :: Doc -> Bool
isEmpty (Nest _ [])  = True
isEmpty _	     = False

-- single character (EXPORTED)
--
char   :: Char -> Doc
char c  = text [c]

-- single line of text (EXPORTED)
--
text   :: String -> Doc
text s  = Nest 0 [Text s]

-- increase nesting of given document (EXPORTED)
--
nest                 :: Int -> Doc -> Doc
nest k (Nest m alts)  = Nest (k + m) alts

-- vertical composition of documents (EXPORTED)
--
($$)                :: Doc -> Doc -> Doc
(Nest _ []  ) $$ doc = doc
(Nest m alts) $$ doc = Nest m [below a | a <- alts]
		       where
			 below                   :: DocAlt -> DocAlt
			 below (Text s)           = let
						      doc' = nestDoc (-m) doc
						    in
						    TextAbove s doc'
			 below (TextAbove s rest) = let
						      doc' = rest 
							     $$ 
							     nestDoc (-m) doc
						    in
						    TextAbove s doc'

-- horizontal composition of documents (EXPORTED)
--
(<>)                           :: Doc -> Doc -> Doc
(Nest _ []  ) <> doc            = doc
doc           <> (Nest _ []  )  = doc
(Nest m alts) <> doc            = Nest m (concat [nextTo a | a <- alts])
  where
    nextTo                    :: DocAlt -> [DocAlt]
    nextTo (Text s)            = let
				   Nest _ bs = doc
				 in
				 [s `inFrontOf` b | b <- bs]
    nextTo (TextAbove s rest)  = [TextAbove s (rest <> doc)]

    inFrontOf                       :: String -> DocAlt -> DocAlt
    s `inFrontOf` (Text t)	       = Text (s ++ t)
    s `inFrontOf` (TextAbove t doc') = let 
					 l = length s
				       in
				       TextAbove (s ++ t) 
						 (nestDoc l doc')

-- given a list of sub-documents, generate a composite document where the 
-- sub-documents are placed next to each other (EXPORTED)
--
-- * when generating a layout a horizontal layout is only chosen
--   when the given collection of sub-documents fits on a single line
--
cat      :: [Doc] -> Doc
cat docs  = catsep (<>) docs

-- given a list of sub-documents, generate a composite document where the 
-- sub-documents are placed next to each other with some seperation between
-- each of them (EXPORTED)
--
-- * when generating a layout a horizontal layout is only chosen
--   when the given collection of sub-documents fits on a single line
--
sep      :: [Doc] -> Doc
sep docs  = catsep (<+>) docs

-- generalise `cat' and `sep'
--
catsep            :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
catsep _     []    = textDoc ""
catsep hcomb docs  = fitunion (foldr hcomb empty docs) 
			      (foldr ($$)  empty docs)
  where
    --
    -- given two documents, where the first one is a horizontal
    -- composition, we only choose a single line alternative (if at
    -- all present) from the first document
    --
    fitunion                                     :: Doc -> Doc -> Doc
    fitunion (Nest m (Text s : _)) (Nest _ alts)  = Nest m (Text s : alts)
    fitunion _                     doc            = doc

-- select the best layout from a document and return it in string form
-- (EXPORTED)
--
-- * given are the overall width and the ribbon ration, ie, the number of
--   times the ribbon fits into a line (the ribbon is the number of
--   characters on a line excluding leading and trailing white spaces)
--
fullRender                   :: Int -> Float -> Doc -> String
fullRender width ribbonRatio  = 
  let
    ribbon = round (fromIntegral width / ribbonRatio)
  in
  dropWhile (== '\n') . nestbest 0 width ribbon
  where
    --
    -- like `best', but with explicit nesting
    --
    nestbest                   :: Int -> Int -> Int -> Doc -> String
    nestbest k w r (Nest _ []  )  = ""
    nestbest k w r (Nest m alts)  = 
	     case foldr1 (choose (w - m) r) alts 
	     of
	       Text s         -> indent (k + m) s
	       TextAbove s bs -> indent (k + m) s 
				 ++ nestbest (k + m) (w - m) r bs
    --
    -- indent the given string by the given amount
    --
    indent     :: Int -> String -> String
    indent k s  = "\n" ++ copy k ' ' ++ s
		  where
		    copy   :: Int -> a -> [a]
		    copy n  = take n . repeat	     

    -- given the remaining width and ribbon together with two possible
    -- documents, choose the first one if its first line is nice; otherwise,
    -- take the second
    --
    choose                 :: Int -> Int -> DocAlt -> DocAlt -> DocAlt
    choose w r alts1 alts2  = if (nice w r (firstline alts1))
			      then alts1
			      else alts2
			      where
			        firstline (Text s)        = s
			        firstline (TextAbove s _) = s

    -- given remaining width and ribbon width decide whether a line
    -- is nice or not
    --
    nice       :: Int -> Int -> String -> Bool
    nice w r s  = (l <= w) && (l <= r)
		  where
		    l = length s


-- derived combinators
-- -------------------

-- punctuation characters (EXPORTED)
--
semi, comma, colon, dot :: Doc
semi  = char ';'
comma = char ','
colon = char ':'
dot   = char '.'

-- separators (EXPORTED)
--
space, equals :: Doc
space  = char ' '
equals = char '='

-- round parenthesis (EXPORTED)
--
lparen, rparen :: Doc
lparen = char '('
rparen = char ')'

-- square brackets (EXPORTED)
--
lbrack, rbrack :: Doc
lbrack = char '['
rbrack = char ']'

-- curly braces (EXPORTED)
--
lbrace, rbrace :: Doc
lbrace = char '{'
rbrace = char '}'

-- any value that has a textual representation (EXPORTED)
--
toDoc :: Show a => a -> Doc
toDoc  = text . show

-- ints (EXPORTED)
--
-- * these are only for compatibility with SimonPJ's `Pretty' module as `toDoc'
--   is more general
--
int      :: Int      -> Doc
int       = toDoc
integer  :: Integer  -> Doc
integer   = toDoc
float    :: Float    -> Doc
float     = toDoc
double   :: Double   -> Doc
double    = toDoc
rational :: Rational -> Doc
rational  = toDoc

-- wrap a document into various forms of brackets
--
parens, brackets, braces :: Doc -> Doc 
parens   doc = lparen <> doc <> rparen
brackets doc = lbrack <> doc <> rbrack
braces   doc = lbrace <> doc <> rbrace

-- wrap a document into quotes
--
quotes, doubleQuotes :: Doc -> Doc
quotes       doc = char '`' <> doc <> char '\''
doubleQuotes doc = char '"' <> doc <> char '"'

-- horizontal composition including a space if none of the documents is empty
-- (EXPORTED)
--
(<+>)                  :: Doc -> Doc -> Doc
d1 <+> d2 | isEmpty d1  = d2
	  | isEmpty d2  = d1
	  | otherwise   = d1 <> space <> d2

-- list version of horizontal composition (EXPORTED)
--
hcat :: [Doc] -> Doc
hcat  = foldr (<>) empty

-- list version of horizontal composition including a space (EXPORTED)
--
hsep :: [Doc] -> Doc
hsep  = foldr (<+>) empty

-- list version of vertical composition (EXPORTED)
--
vcat :: [Doc] -> Doc
vcat = foldr ($$) empty

-- hang the second document of the first, where the second one is indented
-- (EXPORTED)
--
hang             :: Doc -> Int -> Doc -> Doc
hang doc1 n doc2  = sep [doc1, nest n doc2]

-- add a punctuation document to every document in a list, but the last
-- (EXPORTED)
--
punctuate      :: Doc -> [Doc] -> [Doc]
punctuate _ []  = []
punctuate p ds  = map (<> p) (init ds) ++ [last ds]

-- render a document using the default settings
--
render :: Doc -> String
render  = fullRender dftWidth dftRibbonRatio


-- type class and precedence
-- -------------------------

-- overloaded pretty-printing function (EXPORTED)
--
class Pretty a where
  pretty     :: a -> Doc
  prettyPrec :: Int -> a -> Doc

  pretty       = prettyPrec 0
  prettyPrec _ = pretty

-- useful to keep the interface simple and general
--
instance Pretty Doc where
  pretty = id

-- conditionally apply a document transformer (EXPORTED)
--
-- * typically a function like `parens' is applied when the precedences require
--   this
--
usedWhen                        :: (Doc -> Doc) -> Bool -> Doc -> Doc
usedWhen wrap c doc | c          = wrap doc
		    | otherwise  = doc

-- associativity of an infix operator (EXPORTED)
--
data Assoc = LeftAssoc | RightAssoc | NoAssoc
	   deriving (Eq)

-- pretty print an infix operator given its precedence, lexeme, and its two
-- arguments (EXPORTED)
--
infixOp                              :: (Pretty a, Pretty b) 
				     => Assoc	  -- associativity of operator
				     -> Int	  -- precedence of operator
				     -> String    -- lexeme of operator
				     -> a	  -- left argument
				     -> b	  -- right argument
				     -> Int	  -- precedence of context
	                             -> Doc
infixOp assoc opp lexeme arg1 arg2 p  = parens `usedWhen` (p > opp) $ 
					  hsep [
					    prettyPrec leftOpp  arg1,
					    text lexeme,
					    prettyPrec rightOpp arg2
					  ]
  where
    leftOpp  = if (assoc == RightAssoc) then opp + 1 else opp
    rightOpp = if (assoc == LeftAssoc ) then opp + 1 else opp


-- the legacy interface (this is only kept for compatibility)
-- --------------------

infixr 1 $|$	-- vertical composition
infixr 1 <^>	-- horizontal composition


textDoc :: String -> Doc
textDoc  = text

nestDoc :: Int -> Doc -> Doc
nestDoc  = nest

($|$) :: Doc -> Doc -> Doc
($|$)  = ($$)

(<^>) :: Doc -> Doc -> Doc
(<^>)  = (<>)

sepDocs :: [Doc] -> Doc
sepDocs  = sep

bestDoc              :: Int -> Int -> Doc -> String
bestDoc width ribbon  = fullRender width 
				   (fromIntegral width / fromIntegral ribbon)


-- debugging support
-- -----------------

dumpDoc               :: Doc -> String
dumpDoc (Nest _ []  )  = "<empty>"
dumpDoc (Nest m alts)  = unlines . map (++ "\n--") . map outline $ alts
			 where
			   outline (Text      str  ) = str
			   outline (TextAbove str _) = str ++ "\n..."
