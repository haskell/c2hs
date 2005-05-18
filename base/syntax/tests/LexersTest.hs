-- tests `Lexer'

module Main(main)
where

import Common
import Errors
import Lexers


data Token = IdentTok  Position String
	   | PlusTok   Position
	   | StarTok   Position
	   | LParenTok Position
	   | RParenTok Position
	   deriving (Show)

initialState    :: String -> LexerState s
initialState cs  = (cs, ("<string>", 1, 1), undefined)

lexide :: Lexer s Token
lexide  = alt alpha +> (alt alpha >|< alt num) `star` epsilon
	  `lexaction` makeIde
	  where
	    alpha = ['a'..'z'] ++ ['A'..'Z']
	    num   = ['0'..'9']

	    makeIde lexeme pos = Just (IdentTok pos lexeme)

lexer :: Lexer s Token
lexer  =      lexide
         >||< char '+' `lexaction` makeSym PlusTok
         >||< char '*' `lexaction` makeSym StarTok
         >||< char '(' `lexaction` makeSym LParenTok
         >||< char ')' `lexaction` makeSym RParenTok
	 where
	   makeSym sym _ pos = Just (sym pos)

testlex    :: String -> IO ()
testlex cs  = let (ts, _, errs) = (execLexer lexer . initialState) cs
	      in do
	      print ts
	      mapM (putStr . showError) errs
	      return ()

main = testlex "foo*(x+y)"
