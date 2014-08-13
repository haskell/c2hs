--  C->Haskell Compiler: Lexer for CHS Files
--
--  Author : Manuel M T Chakravarty
--  Created: 13 August 99
--
--  Copyright (c) [1999..2005] Manuel M T Chakravarty
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
--  Lexer for CHS files; the tokens are only partially recognised.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * CHS files are assumed to be Haskell 98 files that include C2HS binding
--    hooks.
--
--  * Haskell code is not tokenised, but binding hooks (delimited by `{#'and
--    `#}') are analysed.  Therefore the lexer operates in two states
--    (realised as two lexer coupled by meta actions) depending on whether
--    Haskell code or a binding hook is currently read.  The lexer reading
--    Haskell code is called `base lexer'; the other one, `binding-hook
--    lexer'.  In addition, there is a inline-c lexer, which, as the
--    binding-hook lexer, can be triggered from the base lexer.
--
--  * Base lexer:
--
--      haskell -> (inline \\ special)*
--               | special \\ `"'
--               | comment
--               | nested
--               | hstring
--               | '{#'
--               | cpp
--      special -> `(' | `{' | `-' | `"'
--      ctrl    -> `\n' | `\f' | `\r' | `\t' | `\v'
--
--      inline  -> any \\ ctrl
--      any     -> '\0'..'\255'
--
--    Within the base lexer control codes appear as separate tokens in the
--    token list.
--
--    NOTE: It is important that `{' is an extra lexeme and not added as an
--          optional component at the end of the first alternative for
--          `haskell'.  Otherwise, the principle of the longest match will
--          divide `foo {#' into the tokens `foo {' and `#' instead of `foo '
--          and `{#'.
--
--    One line comments are handled by
--
--      comment -> `--' (any \\ `\n')* `\n'
--
--    and nested comments by
--
--      nested -> `{-' any* `-}'
--
--    where `any*' may contain _balanced_ occurrences of `{-' and `-}'.
--
--      hstring -> `"' inhstr* `"'
--      inhstr  -> ` '..`\127' \\ `"'
--               | `\"'
--
--    Pre-precessor directives as well as the switch to inline-C code are
--    formed as follows:
--
--      cpp     -> `\n#' (inline | `\t')* `\n'
--               | `\n#c' (' ' | '\t')* `\n'
--
--    We allow whitespace between the `#' and the actual directive, but in `#c'
--    and `#endc' the directive must immediately follow the `#'.  This might
--    be regarded as a not entirely orthogonal design, but simplifies matters
--    especially for `#endc'.
--
--  * On encountering the lexeme `{#', a meta action in the base lexer
--    transfers control to the following binding-hook lexer:
--
--      ident       -> letter (letter | digit | `\'')*
--      reservedid  -> `add' | `as' | `call' | `class' | `context' | `deriving'
--                   | `enum' | `foreign' | `fun' | `get' | `lib'
--                   | `downcaseFirstLetter'
--                   | `newtype' | `nocode' | `pointer' | `prefix' | `pure'
--                   | `set' | `sizeof' | `stable' | `struct' | `type'
--                   | `underscoreToCase' | `upcaseFirstLetter' | `unsafe' |
--                   | `with'
--      reservedsym -> `{#' | `#}' | `{' | `}' | `,' | `.' | `->' | `='
--                   | `=>' | '-' | `*' | `&' | `^'
--      string      -> `"' instr* `"'
--      verbhs      -> `\`' inhsverb* `\''
--      quoths      -> `\'' inhsverb* `\''
--      instr       -> ` '..`\127' \\ `"'
--      inhsverb    -> ` '..`\127' \\ `\''
--      comment     -> `--' (any \\ `\n')* `\n'
--
--    Control characters, white space, and comments are discarded in the
--    binding-hook lexer.  Nested comments are not allowed in a binding hook.
--    Identifiers can be enclosed in single quotes to avoid collision with
--    C->Haskell keywords, and in this case quoted part could also be followed by
--    what is assumed to be a valid Haskell code, which would be transferred in the
--    output file verbatim.
--
--  * In the binding-hook lexer, the lexeme `#}' transfers control back to the
--    base lexer.  An occurence of the lexeme `{#' inside the binding-hook
--    lexer triggers an error.  The symbol `{#' is not explcitly represented
--    in the resulting token stream.  However, the occurrence of a token
--    representing one of the reserved identifiers `call', `context', `enum',
--    and `field' marks the start of a binding hook.  Strictly speaking, `#}'
--    need also not occur in the token stream, as the next `haskell' token
--    marks a hook's end.  It is, however, useful for producing accurate error
--    messages (in case an hook is closed to early) to have a token
--    representing `#}'.
--
--  * The rule `ident' describes Haskell identifiers, but without
--    distinguishing between variable and constructor identifers (ie, those
--    starting with a lowercase and those starting with an uppercase letter).
--    However, we use it also to scan C identifiers; although, strictly
--    speaking, it is too general for them.  In the case of C identifiers,
--    this should not have any impact on the range of descriptions accepted by
--    the tool, as illegal identifier will never occur in a C header file that
--    is accepted by the C lexer.  In the case of Haskell identifiers, a
--    confusion between variable and constructor identifiers will be noted by
--    the Haskell compiler translating the code generated by c2hs.  Moreover,
--    identifiers that are inside quoted parts (see above) may not contain apostrophes.
--
--  * Any line starting with the character `#' is regarded to be a C
--    preprocessor directive.  With the exception of `#c' and `#endc', which
--    delimit a set of lines containing inline C code.  Hence, in the base
--    lexer, the lexeme `#c' triggers a meta action transferring control to the
--    following inline-C lexer:
--
--      c  -> inline* \\ `\n#endc'
--
--    We do neither treat C strings nor C comments specially.  Hence, if the
--    string "\n#endc" occurs in a comment, we will mistakenly regard it as
--    the end of the inline C code.  Note that the problem cannot happen with
--    strings, as C does not permit strings that extend over multiple lines.
--    At the moment, it just seems not to be worth the effort required to
--    treat this situation more accurately.
--
--    The inline-C lexer also doesn't handle pre-processor directives
--    specially.  Hence, structural pre-processor directives (namely,
--    conditionals) may occur within inline-C code only properly nested.
--
--  Shortcomings
--  ~~~~~~~~~~~~
--  Some lexemes that include single and double quote characters are not lexed
--  correctly.  See the implementation comment at `haskell' for details.
--
--
--- TODO ----------------------------------------------------------------------
--
--  * In `haskell', the case of a single `"' (without a matching second one)
--    is caught by an eplicit error raising rule.  This shouldn't be
--    necessary, but for some strange reason, the lexer otherwise hangs when a
--    single `"' appears in the input.
--
--  * Comments in the "gap" of a string are not yet supported.
--

module C2HS.CHS.Lexer (CHSToken(..), lexCHS, keywordToIdent)
where

import Data.List     ((\\))
import Data.Char     (isDigit)

import Language.C.Data.Ident
import Language.C.Data.Name
import Language.C.Data.Position

import Data.Errors    (ErrorLevel(..), makeError)
import Text.Lexers    (Regexp, Lexer, Action, epsilon, char, (+>), lexaction,
                  lexactionErr, lexmeta, (>|<), (>||<), ctrlLexer, star, plus,
                  alt, string, execLexer)
import Control.State (getNameSupply, setNameSupply)
import C2HS.State (CST, raise, raiseError)


-- token definition
-- ----------------

-- | possible tokens
--
data CHSToken = CHSTokArrow   Position          -- `->'
              | CHSTokDArrow  Position          -- `=>'
              | CHSTokDot     Position          -- `.'
              | CHSTokComma   Position          -- `,'
              | CHSTokEqual   Position          -- `='
              | CHSTokMinus   Position          -- `-'
              | CHSTokStar    Position          -- `*'
              | CHSTokAmp     Position          -- `&'
              | CHSTokHat     Position          -- `^'
              | CHSTokLBrace  Position          -- `{'
              | CHSTokRBrace  Position          -- `}'
              | CHSTokLParen  Position          -- `('
              | CHSTokRParen  Position          -- `)'
              | CHSTokHook    Position          -- `{#'
              | CHSTokEndHook Position          -- `#}'
              | CHSTokAdd     Position          -- `add'
              | CHSTokAs      Position          -- `as'
              | CHSTokCall    Position          -- `call'
              | CHSTokClass   Position          -- `class'
              | CHSTokContext Position          -- `context'
              | CHSTokNonGNU  Position          -- `nonGNU'
              | CHSTokDerive  Position          -- `deriving'
              | CHSTokDown    Position          -- `downcaseFirstLetter'
              | CHSTokEnum    Position          -- `enum'
              | CHSTokForeign Position          -- `foreign'
              | CHSTokFun     Position          -- `fun'
              | CHSTokGet     Position          -- `get'
              | CHSTokImport  Position          -- `import'
              | CHSTokLib     Position          -- `lib'
              | CHSTokNewtype Position          -- `newtype'
              | CHSTokNocode  Position          -- `nocode'
              | CHSTokOffsetof Position         -- `offsetof'
              | CHSTokPointer Position          -- `pointer'
              | CHSTokPrefix  Position          -- `prefix'
              | CHSTokPure    Position          -- `pure'
              | CHSTokQualif  Position          -- `qualified'
              | CHSTokSet     Position          -- `set'
              | CHSTokSizeof  Position          -- `sizeof'
              | CHSTokAlignof Position          -- `alignof'
              | CHSTokStable  Position          -- `stable'
              | CHSTokStruct  Position          -- `struct'
              | CHSTokType    Position          -- `type'
              | CHSTok_2Case  Position          -- `underscoreToCase'
              | CHSTokUnsafe  Position          -- `unsafe'
              | CHSTokUpper   Position          -- `upcaseFirstLetter'
              | CHSTokWith    Position Ident    -- `with'
              | CHSTokString  Position String   -- string
              | CHSTokHSVerb  Position String   -- verbatim Haskell (`...')
              | CHSTokHSQuot  Position String   -- quoted Haskell ('...')
              | CHSTokIdent   Position Ident    -- identifier
              | CHSTokHaskell Position String   -- verbatim Haskell code
              | CHSTokCPP     Position String Bool -- pre-processor directive
              | CHSTokLine    Position          -- line pragma
              | CHSTokC       Position String   -- verbatim C code
              | CHSTokCtrl    Position Char     -- control code
              | CHSTokComment Position String   -- comment

instance Pos CHSToken where
  posOf (CHSTokArrow   pos  ) = pos
  posOf (CHSTokDArrow  pos  ) = pos
  posOf (CHSTokDot     pos  ) = pos
  posOf (CHSTokComma   pos  ) = pos
  posOf (CHSTokEqual   pos  ) = pos
  posOf (CHSTokMinus   pos  ) = pos
  posOf (CHSTokStar    pos  ) = pos
  posOf (CHSTokAmp     pos  ) = pos
  posOf (CHSTokHat     pos  ) = pos
  posOf (CHSTokLBrace  pos  ) = pos
  posOf (CHSTokRBrace  pos  ) = pos
  posOf (CHSTokLParen  pos  ) = pos
  posOf (CHSTokRParen  pos  ) = pos
  posOf (CHSTokHook    pos  ) = pos
  posOf (CHSTokEndHook pos  ) = pos
  posOf (CHSTokAdd     pos  ) = pos
  posOf (CHSTokAs      pos  ) = pos
  posOf (CHSTokCall    pos  ) = pos
  posOf (CHSTokClass   pos  ) = pos
  posOf (CHSTokContext pos  ) = pos
  posOf (CHSTokNonGNU  pos  ) = pos
  posOf (CHSTokDerive  pos  ) = pos
  posOf (CHSTokDown    pos  ) = pos
  posOf (CHSTokEnum    pos  ) = pos
  posOf (CHSTokForeign pos  ) = pos
  posOf (CHSTokFun     pos  ) = pos
  posOf (CHSTokGet     pos  ) = pos
  posOf (CHSTokImport  pos  ) = pos
  posOf (CHSTokLib     pos  ) = pos
  posOf (CHSTokNewtype pos  ) = pos
  posOf (CHSTokNocode  pos  ) = pos
  posOf (CHSTokOffsetof pos ) = pos
  posOf (CHSTokPointer pos  ) = pos
  posOf (CHSTokPrefix  pos  ) = pos
  posOf (CHSTokPure    pos  ) = pos
  posOf (CHSTokQualif  pos  ) = pos
  posOf (CHSTokSet     pos  ) = pos
  posOf (CHSTokSizeof  pos  ) = pos
  posOf (CHSTokAlignof pos  ) = pos
  posOf (CHSTokStable  pos  ) = pos
  posOf (CHSTokStruct  pos  ) = pos
  posOf (CHSTokType    pos  ) = pos
  posOf (CHSTok_2Case  pos  ) = pos
  posOf (CHSTokUnsafe  pos  ) = pos
  posOf (CHSTokUpper   pos  ) = pos
  posOf (CHSTokWith    pos _) = pos
  posOf (CHSTokString  pos _) = pos
  posOf (CHSTokHSVerb  pos _) = pos
  posOf (CHSTokHSQuot  pos _) = pos
  posOf (CHSTokIdent   pos _) = pos
  posOf (CHSTokHaskell pos _) = pos
  posOf (CHSTokCPP     pos _ _) = pos
  posOf (CHSTokLine    pos  ) = pos
  posOf (CHSTokC       pos _) = pos
  posOf (CHSTokCtrl    pos _) = pos
  posOf (CHSTokComment pos _) = pos

instance Eq CHSToken where
  (CHSTokArrow    _  ) == (CHSTokArrow    _  ) = True
  (CHSTokDArrow   _  ) == (CHSTokDArrow   _  ) = True
  (CHSTokDot      _  ) == (CHSTokDot      _  ) = True
  (CHSTokComma    _  ) == (CHSTokComma    _  ) = True
  (CHSTokEqual    _  ) == (CHSTokEqual    _  ) = True
  (CHSTokMinus    _  ) == (CHSTokMinus    _  ) = True
  (CHSTokStar     _  ) == (CHSTokStar     _  ) = True
  (CHSTokAmp      _  ) == (CHSTokAmp      _  ) = True
  (CHSTokHat      _  ) == (CHSTokHat      _  ) = True
  (CHSTokLBrace   _  ) == (CHSTokLBrace   _  ) = True
  (CHSTokRBrace   _  ) == (CHSTokRBrace   _  ) = True
  (CHSTokLParen   _  ) == (CHSTokLParen   _  ) = True
  (CHSTokRParen   _  ) == (CHSTokRParen   _  ) = True
  (CHSTokHook     _  ) == (CHSTokHook     _  ) = True
  (CHSTokEndHook  _  ) == (CHSTokEndHook  _  ) = True
  (CHSTokAdd      _  ) == (CHSTokAdd      _  ) = True
  (CHSTokAs       _  ) == (CHSTokAs       _  ) = True
  (CHSTokCall     _  ) == (CHSTokCall     _  ) = True
  (CHSTokClass    _  ) == (CHSTokClass    _  ) = True
  (CHSTokContext  _  ) == (CHSTokContext  _  ) = True
  (CHSTokNonGNU   _  ) == (CHSTokNonGNU   _  ) = True
  (CHSTokDerive   _  ) == (CHSTokDerive   _  ) = True
  (CHSTokDown     _  ) == (CHSTokDown     _  ) = True
  (CHSTokEnum     _  ) == (CHSTokEnum     _  ) = True
  (CHSTokForeign  _  ) == (CHSTokForeign  _  ) = True
  (CHSTokFun      _  ) == (CHSTokFun      _  ) = True
  (CHSTokGet      _  ) == (CHSTokGet      _  ) = True
  (CHSTokImport   _  ) == (CHSTokImport   _  ) = True
  (CHSTokLib      _  ) == (CHSTokLib      _  ) = True
  (CHSTokNewtype  _  ) == (CHSTokNewtype  _  ) = True
  (CHSTokNocode   _  ) == (CHSTokNocode   _  ) = True
  (CHSTokOffsetof _  ) == (CHSTokOffsetof _  ) = True
  (CHSTokPointer  _  ) == (CHSTokPointer  _  ) = True
  (CHSTokPrefix   _  ) == (CHSTokPrefix   _  ) = True
  (CHSTokPure     _  ) == (CHSTokPure     _  ) = True
  (CHSTokQualif   _  ) == (CHSTokQualif   _  ) = True
  (CHSTokSet      _  ) == (CHSTokSet      _  ) = True
  (CHSTokSizeof   _  ) == (CHSTokSizeof   _  ) = True
  (CHSTokAlignof  _  ) == (CHSTokAlignof  _  ) = True
  (CHSTokStable   _  ) == (CHSTokStable   _  ) = True
  (CHSTokStruct   _  ) == (CHSTokStruct   _  ) = True
  (CHSTokType     _  ) == (CHSTokType     _  ) = True
  (CHSTok_2Case   _  ) == (CHSTok_2Case   _  ) = True
  (CHSTokUnsafe   _  ) == (CHSTokUnsafe   _  ) = True
  (CHSTokUpper    _  ) == (CHSTokUpper    _  ) = True
  (CHSTokWith     _ _) == (CHSTokWith     _ _) = True
  (CHSTokString   _ _) == (CHSTokString   _ _) = True
  (CHSTokHSVerb   _ _) == (CHSTokHSVerb   _ _) = True
  (CHSTokHSQuot   _ _) == (CHSTokHSQuot   _ _) = True
  (CHSTokIdent    _ _) == (CHSTokIdent    _ _) = True
  (CHSTokHaskell  _ _) == (CHSTokHaskell  _ _) = True
  (CHSTokCPP    _ _ _) == (CHSTokCPP    _ _ _) = True
  (CHSTokLine     _  ) == (CHSTokLine     _  ) = True
  (CHSTokC        _ _) == (CHSTokC        _ _) = True
  (CHSTokCtrl     _ _) == (CHSTokCtrl     _ _) = True
  (CHSTokComment  _ _) == (CHSTokComment  _ _) = True
  _                    == _                    = False

instance Show CHSToken where
  showsPrec _ (CHSTokArrow   _  ) = showString "->"
  showsPrec _ (CHSTokDArrow  _  ) = showString "=>"
  showsPrec _ (CHSTokDot     _  ) = showString "."
  showsPrec _ (CHSTokComma   _  ) = showString ","
  showsPrec _ (CHSTokEqual   _  ) = showString "="
  showsPrec _ (CHSTokMinus   _  ) = showString "-"
  showsPrec _ (CHSTokStar    _  ) = showString "*"
  showsPrec _ (CHSTokAmp     _  ) = showString "&"
  showsPrec _ (CHSTokHat     _  ) = showString "^"
  showsPrec _ (CHSTokLBrace  _  ) = showString "{"
  showsPrec _ (CHSTokRBrace  _  ) = showString "}"
  showsPrec _ (CHSTokLParen  _  ) = showString "("
  showsPrec _ (CHSTokRParen  _  ) = showString ")"
  showsPrec _ (CHSTokHook    _  ) = showString "{#"
  showsPrec _ (CHSTokEndHook _  ) = showString "#}"
  showsPrec _ (CHSTokAdd     _  ) = showString "add"
  showsPrec _ (CHSTokAs      _  ) = showString "as"
  showsPrec _ (CHSTokCall    _  ) = showString "call"
  showsPrec _ (CHSTokClass   _  ) = showString "class"
  showsPrec _ (CHSTokContext _  ) = showString "context"
  showsPrec _ (CHSTokNonGNU  _  ) = showString "nonGNU"
  showsPrec _ (CHSTokDerive  _  ) = showString "deriving"
  showsPrec _ (CHSTokDown    _  ) = showString "downcaseFirstLetter"
  showsPrec _ (CHSTokEnum    _  ) = showString "enum"
  showsPrec _ (CHSTokForeign _  ) = showString "foreign"
  showsPrec _ (CHSTokFun     _  ) = showString "fun"
  showsPrec _ (CHSTokGet     _  ) = showString "get"
  showsPrec _ (CHSTokImport  _  ) = showString "import"
  showsPrec _ (CHSTokLib     _  ) = showString "lib"
  showsPrec _ (CHSTokNewtype _  ) = showString "newtype"
  showsPrec _ (CHSTokNocode  _  ) = showString "nocode"
  showsPrec _ (CHSTokOffsetof _ ) = showString "offsetof"
  showsPrec _ (CHSTokPointer _  ) = showString "pointer"
  showsPrec _ (CHSTokPrefix  _  ) = showString "prefix"
  showsPrec _ (CHSTokPure    _  ) = showString "pure"
  showsPrec _ (CHSTokQualif  _  ) = showString "qualified"
  showsPrec _ (CHSTokSet     _  ) = showString "set"
  showsPrec _ (CHSTokSizeof  _  ) = showString "sizeof"
  showsPrec _ (CHSTokAlignof _  ) = showString "alignof"
  showsPrec _ (CHSTokStable  _  ) = showString "stable"
  showsPrec _ (CHSTokStruct  _  ) = showString "struct"
  showsPrec _ (CHSTokType    _  ) = showString "type"
  showsPrec _ (CHSTok_2Case  _  ) = showString "underscoreToCase"
  showsPrec _ (CHSTokUnsafe  _  ) = showString "unsafe"
  showsPrec _ (CHSTokUpper   _  ) = showString "upcaseFirstLetter"
  showsPrec _ (CHSTokWith    _ _) = showString "with"
  showsPrec _ (CHSTokString  _ s) = showString ("\"" ++ s ++ "\"")
  showsPrec _ (CHSTokHSVerb  _ s) = showString ("`" ++ s ++ "'")
  showsPrec _ (CHSTokHSQuot  _ s) = showString ("'" ++ s ++ "'")
  showsPrec _ (CHSTokIdent   _ i) = (showString . identToString) i
  showsPrec _ (CHSTokHaskell _ s) = showString s
  showsPrec _ (CHSTokCPP  _ s nl) = showString (if nl then "\n" else "" ++ s)
  showsPrec _ (CHSTokLine    _  ) = id            --TODO show line num?
  showsPrec _ (CHSTokC       _ s) = showString s
  showsPrec _ (CHSTokCtrl    _ c) = showChar c
  showsPrec _ (CHSTokComment _ s) = showString (if null s
                                                then ""
                                                else " -- " ++ s ++ "\n")

-- lexer state
-- -----------

-- | state threaded through the lexer
--
data CHSLexerState = CHSLS {
                       nestLvl :: Int,   -- nesting depth of nested comments
                       inHook  :: Bool,  -- within a binding hook?
                       namesup :: [Name] -- supply of unique names
                     }

-- | initial state
--
initialState :: [Name] -> CST s CHSLexerState
initialState nameSupply =
  do
    return CHSLS {
                         nestLvl = 0,
                         inHook  = False,
                         namesup = nameSupply
                }

-- | raise an error if the given state is not a final state
--
assertFinalState :: Position -> CHSLexerState -> CST s ()
assertFinalState pos CHSLS {nestLvl = nestLvl', inHook = inHook'}
  | nestLvl' > 0 = raiseError pos ["Unexpected end of file!",
                                   "Unclosed nested comment."]
  | inHook'      = raiseError pos ["Unexpected end of file!",
                                   "Unclosed binding hook."]
  | otherwise    = return ()

-- | lexer and action type used throughout this specification
--
type CHSLexer  = Lexer  CHSLexerState CHSToken
type CHSAction = Action               CHSToken
type CHSRegexp = Regexp CHSLexerState CHSToken

-- | for actions that need a new unique name
--
infixl 3 `lexactionName`
lexactionName :: CHSRegexp
              -> (String -> Position -> Name -> CHSToken)
              -> CHSLexer
re `lexactionName` action = re `lexmeta` action'
  where
    action' str pos state = let name:ns = namesup state
                            in
                            (Just $ Right (action str pos name),
                             incPos pos (length str),
                             state {namesup = ns},
                             Nothing)


-- lexical specification
-- ---------------------

-- | the lexical definition of the tokens (the base lexer)
--
chslexer :: CHSLexer
chslexer  =      haskell        -- Haskell code
            >||< nested         -- nested comments
            >||< ctrl           -- control code (that has to be preserved)
            >||< hook           -- start of a binding hook
            >||< cpp            -- a pre-processor directive (or `#c')
            >||< startmarker    -- marks beginning of input

-- | stream of Haskell code (terminated by a control character or binding hook)
--
haskell :: CHSLexer
--
-- NB: We need to make sure that '"' is not regarded as the beginning of a
--     string; however, we cannot really lex character literals properly
--     without lexing identifiers (as the latter may containing single quotes
--     as part of their lexeme).  Thus, we special case '"'.  This is still a
--     kludge, as a program fragment, such as
--
--       foo'"'strange string"
--
--     will not be handled correctly.
--
haskell  = (    anyButSpecial`star` epsilon
            >|< specialButQuotes
            >|< char '"'  +> inhstr`star` char '"'
            >|< string "'\"'"                           -- special case of "
            >|< string "--" +> anyButNL`star` epsilon   -- comment
           )
           `lexaction` copyVerbatim
           >||< char '"'                                -- this is a bad kludge
                `lexactionErr`
                  \_ pos -> (Left $ makeError LevelError pos
                                              ["Lexical error!",
                                              "Unclosed string."])
           where
             anyButSpecial    = alt (inlineSet \\ specialSet)
             specialButQuotes = alt (specialSet \\ ['"'])
             anyButNL         = alt (anySet \\ ['\n'])
             inhstr           = instr >|< char '\\' >|< string "\\\"" >|< gap
             gap              = char '\\' +> alt (' ':ctrlSet)`plus` char '\\'

-- | action copying the input verbatim to `CHSTokHaskell' tokens
--
copyVerbatim        :: CHSAction
copyVerbatim cs pos  = Just $ CHSTokHaskell pos cs

-- | nested comments
--
nested :: CHSLexer
nested  =
       string "{-"              {- for Haskell emacs mode :-( -}
       `lexmeta` enterComment
  >||<
       string "-}"
       `lexmeta` leaveComment
  where
    enterComment cs pos s =
      (copyVerbatim' cs pos,                    -- collect the lexeme
       incPos pos 2,                            -- advance current position
       s {nestLvl = nestLvl s + 1},             -- increase nesting level
       Just $ inNestedComment)                  -- continue in comment lexer
    --
    leaveComment cs pos s =
      case nestLvl s of
        0 -> (commentCloseErr pos,              -- 0: -} outside comment => err
              incPos pos 2,                     -- advance current position
              s,
              Nothing)
        1 -> (copyVerbatim' cs pos,             -- collect the lexeme
              incPos pos 2,                     -- advance current position
              s {nestLvl = nestLvl s - 1},      -- decrease nesting level
              Just chslexer)                    -- 1: continue with root lexer
        _ -> (copyVerbatim' cs pos,             -- collect the lexeme
              incPos pos 2,                     -- advance current position
              s {nestLvl = nestLvl s - 1},      -- decrease nesting level
              Nothing)                          -- _: cont with comment lexer
    --
    copyVerbatim' cs pos  = Just $ Right (CHSTokHaskell pos cs)
    --
    commentCloseErr pos =
      Just $ Left (makeError LevelError pos
                             ["Lexical error!",
                             "`-}' not preceded by a matching `{-'."])
                             {- for Haskell emacs mode :-( -}


-- | lexer processing the inner of a comment
--
inNestedComment :: CHSLexer
inNestedComment  =      commentInterior         -- inside a comment
                   >||< nested                  -- nested comments
                   >||< ctrl                    -- control code (preserved)

-- | standard characters in a nested comment
--
commentInterior :: CHSLexer
commentInterior  = (    anyButSpecial`star` epsilon
                    >|< special
                   )
                   `lexaction` copyVerbatim
                   where
                     anyButSpecial = alt (inlineSet \\ commentSpecialSet)
                     special       = alt commentSpecialSet

-- | control code in the base lexer (is turned into a token)
--
-- * this covers exactly the same set of characters as contained in `ctrlSet'
--   and `Lexers.ctrlLexer' and advances positions also like the `ctrlLexer'
--
ctrl :: CHSLexer
ctrl  =
       char '\n' `lexmeta` newline
  >||< char '\r' `lexmeta` newline
  >||< char '\v' `lexmeta` newline
  >||< char '\f' `lexmeta` formfeed
  >||< char '\t' `lexmeta` tab
  where
    newline  [c] pos = ctrlResult pos c (retPos pos)
    formfeed [c] pos = ctrlResult pos c (incPos pos 1)
    tab      [c] pos = ctrlResult pos c (incPos pos 8)

    ctrlResult pos c pos' s =
      (Just $ Right (CHSTokCtrl pos c), pos', s, Nothing)

-- | start of a binding hook (ie, enter the binding hook lexer)
--
hook :: CHSLexer
hook  = string "{#"
        `lexmeta` \_ pos s -> (Just $ Right (CHSTokHook pos),
                               incPos pos 2, s, Just bhLexer)

-- | start marker: used to identify pre-processor directive at
-- beginning of input -- this lexer just drops the start marker if it
-- hasn't been used to handle a pre-processor directive
--
startmarker :: CHSLexer
startmarker = char '\000' `lexmeta`
              \lexeme pos s -> (Nothing, incPos pos 1, s, Just chslexer)

-- | pre-processor directives and `#c'
--
-- * we lex `#c' as a directive and special case it in the action
--
-- * we lex C line number pragmas and special case it in the action
--
cpp :: CHSLexer
cpp = directive
      where
        directive =
          --(string "\n#" >|< string "\0#") +>
          alt "\n\0" +> alt " \t" `star` string "#" +>
          alt ('\t':inlineSet)`star` epsilon
          `lexmeta`
             \t@(ld:spdir) pos s ->      -- strip off the "\n" or "\0"
             let dir = drop 1 $ dropWhile (`elem` " \t") spdir
             in case dir of
                 ['c']                      ->          -- #c
                   (Nothing, incPos pos (length t), s, Just cLexer)
                 -- a #c may be followed by whitespace
                 'c':sp:_ | sp `elem` " \t" ->          -- #c
                   (Nothing, incPos pos (length t), s, Just cLexer)
                 ' ':line@(n:_) | isDigit n ->                 -- C line pragma
                   let pos' = adjustPosByCLinePragma line pos
                    in (Just $ Right (CHSTokLine pos'), pos', s, Nothing)
                 _                            ->        -- CPP directive
                   (Just $ Right (CHSTokCPP pos dir (ld == '\n')),
                    if ld == '\n' then retPos pos else incPos pos (length t),
                    s, Nothing)

adjustPosByCLinePragma :: String -> Position -> Position
adjustPosByCLinePragma str pos = adjustPos fname' row' pos
  where
    fname           = posFile pos
    str'            = dropWhite str
    (rowStr, str'') = span isDigit str'
    row'            = read rowStr
    str'''          = dropWhite str''
    fnameStr        = takeWhile (/= '"') . drop 1 $ str'''
    fname'          | null str''' || head str''' /= '"' = fname
                    -- try and get more sharing of file name strings
                    | fnameStr == fname                 = fname
                    | otherwise                         = fnameStr
    --
    dropWhite = dropWhile (\c -> c == ' ' || c == '\t')

-- | the binding hook lexer
--
bhLexer :: CHSLexer
bhLexer  =      identOrKW
           >||< symbol
           >||< strlit
           >||< hsverb
           >||< hsquot
           >||< whitespace
           >||< endOfHook
           >||< string "--" +> anyButNL`star` char '\n'   -- comment
                `lexaction` \cs pos -> Just (CHSTokComment pos (init (drop 2 cs)))
           where
             anyButNL  = alt (anySet \\ ['\n'])
             endOfHook = string "#}"
                         `lexmeta`
                          \_ pos s -> (Just $ Right (CHSTokEndHook pos),
                                       incPos pos 2, s, Just chslexer)

-- | the inline-C lexer
--
cLexer :: CHSLexer
cLexer =      inlineC                     -- inline C code
         >||< ctrl                        -- control code (preserved)
         >||< string "\n#endc"            -- end of inline C code...
              `lexmeta`                   -- ...preserve '\n' as control token
              \_ pos s -> (Just $ Right (CHSTokCtrl pos '\n'), retPos pos, s,
                           Just chslexer)
         where
           inlineC = alt inlineSet `lexaction` copyVerbatimC
           --
           copyVerbatimC :: CHSAction
           copyVerbatimC cs pos = Just $ CHSTokC pos cs

-- | whitespace
--
-- * horizontal and vertical tabs, newlines, and form feeds are filter out by
--   `Lexers.ctrlLexer'
--
whitespace :: CHSLexer
whitespace  =      (char ' ' `lexaction` \_ _ -> Nothing)
              >||< ctrlLexer

-- | identifiers and keywords
--
identOrKW :: CHSLexer
--
-- the strictness annotations seem to help a bit
--
identOrKW  =
       -- identifier or keyword
       (letter +> (letter >|< digit >|< char '\'')`star` epsilon
       `lexactionName` \cs pos name -> (idkwtok $!pos) cs name)
  where
    idkwtok pos "add"              _    = CHSTokAdd     pos
    idkwtok pos "as"               _    = CHSTokAs      pos
    idkwtok pos "call"             _    = CHSTokCall    pos
    idkwtok pos "class"            _    = CHSTokClass   pos
    idkwtok pos "context"          _    = CHSTokContext pos
    idkwtok pos "nonGNU"           _    = CHSTokNonGNU  pos
    idkwtok pos "deriving"         _    = CHSTokDerive  pos
    idkwtok pos "downcaseFirstLetter" _ = CHSTokDown    pos
    idkwtok pos "enum"             _    = CHSTokEnum    pos
    idkwtok pos "foreign"          _    = CHSTokForeign pos
    idkwtok pos "fun"              _    = CHSTokFun     pos
    idkwtok pos "get"              _    = CHSTokGet     pos
    idkwtok pos "import"           _    = CHSTokImport  pos
    idkwtok pos "lib"              _    = CHSTokLib     pos
    idkwtok pos "newtype"          _    = CHSTokNewtype pos
    idkwtok pos "nocode"           _    = CHSTokNocode  pos
    idkwtok pos "offsetof"         _    = CHSTokOffsetof pos
    idkwtok pos "pointer"          _    = CHSTokPointer pos
    idkwtok pos "prefix"           _    = CHSTokPrefix  pos
    idkwtok pos "pure"             _    = CHSTokPure    pos
    idkwtok pos "qualified"        _    = CHSTokQualif  pos
    idkwtok pos "set"              _    = CHSTokSet     pos
    idkwtok pos "sizeof"           _    = CHSTokSizeof  pos
    idkwtok pos "alignof"          _    = CHSTokAlignof pos
    idkwtok pos "stable"           _    = CHSTokStable  pos
    idkwtok pos "struct"           _    = CHSTokStruct  pos
    idkwtok pos "type"             _    = CHSTokType    pos
    idkwtok pos "underscoreToCase" _    = CHSTok_2Case  pos
    idkwtok pos "unsafe"           _    = CHSTokUnsafe  pos
    idkwtok pos "upcaseFirstLetter"_    = CHSTokUpper   pos
    idkwtok pos "with"             name = mkwith pos name
    idkwtok pos cs                 name = mkid pos cs name
    --
    mkid pos cs name = CHSTokIdent pos (mkIdent pos cs name)
    mkwith pos name = CHSTokWith pos (mkIdent pos "with" name)

keywordToIdent :: CHSToken -> CHSToken
keywordToIdent tok =
  case tok of
    CHSTokAdd     pos -> mkid pos "add"
    CHSTokAs      pos -> mkid pos "as"
    CHSTokCall    pos -> mkid pos "call"
    CHSTokClass   pos -> mkid pos "class"
    CHSTokContext pos -> mkid pos "context"
    CHSTokNonGNU  pos -> mkid pos "nonGNU"
    CHSTokDerive  pos -> mkid pos "deriving"
    CHSTokDown    pos -> mkid pos "downcaseFirstLetter"
    CHSTokEnum    pos -> mkid pos "enum"
    CHSTokForeign pos -> mkid pos "foreign"
    CHSTokFun     pos -> mkid pos "fun"
    CHSTokGet     pos -> mkid pos "get"
    CHSTokImport  pos -> mkid pos "import"
    CHSTokLib     pos -> mkid pos "lib"
    CHSTokNewtype pos -> mkid pos "newtype"
    CHSTokNocode  pos -> mkid pos "nocode"
    CHSTokOffsetof pos -> mkid pos "offsetof"
    CHSTokPointer pos -> mkid pos "pointer"
    CHSTokPrefix  pos -> mkid pos "prefix"
    CHSTokPure    pos -> mkid pos "pure"
    CHSTokQualif  pos -> mkid pos "qualified"
    CHSTokSet     pos -> mkid pos "set"
    CHSTokSizeof  pos -> mkid pos "sizeof"
    CHSTokAlignof pos -> mkid pos "alignof"
    CHSTokStable  pos -> mkid pos "stable"
    CHSTokStruct  pos -> mkid pos "struct"
    CHSTokType    pos -> mkid pos "type"
    CHSTok_2Case  pos -> mkid pos "underscoreToCase"
    CHSTokUnsafe  pos -> mkid pos "unsafe"
    CHSTokUpper   pos -> mkid pos "upcaseFirstLetter"
    CHSTokWith    pos ide -> CHSTokIdent pos ide
    _ -> tok
    where mkid pos str = CHSTokIdent pos (internalIdent str)

-- | reserved symbols
--
symbol :: CHSLexer
symbol  =      sym "->" CHSTokArrow
          >||< sym "=>" CHSTokDArrow
          >||< sym "."  CHSTokDot
          >||< sym ","  CHSTokComma
          >||< sym "="  CHSTokEqual
          >||< sym "-"  CHSTokMinus
          >||< sym "*"  CHSTokStar
          >||< sym "&"  CHSTokAmp
          >||< sym "^"  CHSTokHat
          >||< sym "{"  CHSTokLBrace
          >||< sym "}"  CHSTokRBrace
          >||< sym "("  CHSTokLParen
          >||< sym ")"  CHSTokRParen
          where
            sym cs con = string cs `lexaction` \_ pos -> Just (con pos)

-- | string
--
strlit :: CHSLexer
strlit  = char '"' +> (instr >|< char '\\')`star` char '"'
          `lexaction` \cs pos -> Just (CHSTokString pos (init . tail $ cs))

-- | verbatim code
--
hsverb :: CHSLexer
hsverb  = char '`' +> inhsverb`star` char '\''
          `lexaction` \cs pos -> Just (CHSTokHSVerb pos (init . tail $ cs))

-- | quoted code
--
hsquot :: CHSLexer
hsquot  = char '\'' +> inhsverb`star` char '\''
          `lexaction` \cs pos -> Just (CHSTokHSQuot pos (init . tail $ cs))


-- | regular expressions
--
letter, digit, instr, inhsverb :: Regexp s t
letter    = alt ['a'..'z'] >|< alt ['A'..'Z'] >|< char '_'
digit     = alt ['0'..'9']
instr     = alt ([' '..'\255'] \\ "\"\\")
inhsverb  = alt ([' '..'\127'] \\ "'")

-- | character sets
--
anySet, inlineSet, specialSet, commentSpecialSet, ctrlSet :: [Char]
anySet            = ['\1'..'\255']
inlineSet         = anySet \\ ctrlSet
specialSet        = ['{', '-', '"', '\'']
commentSpecialSet = ['{', '-']
ctrlSet           = ['\n', '\f', '\r', '\t', '\v']


-- main lexing routine
-- -------------------

-- | generate a token sequence out of a string denoting a CHS file
--
-- * the given position is attributed to the first character in the string
--
-- * errors are entered into the compiler state
--
-- * on a successfull parse, the name supply is updated
lexCHS        :: String -> Position -> CST s [CHSToken]
lexCHS cs pos  =
  do
    nameSupply <- getNameSupply
    state <- initialState nameSupply
    let (ts, lstate, errs) = execLexer chslexer ('\0':cs, pos, state)
        (_, pos', state')  = lstate
    mapM_ raise errs
    assertFinalState pos' state'
    setNameSupply $ namesup state'
    return ts
