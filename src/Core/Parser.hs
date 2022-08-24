{-|

Module      : Core.Parser
Description : Parser for the core syntax of Jeopardy.
Author      : Joachim Tilsted Kristensen.
Licence     : GNU GENERAL PUBLIC LICENSE
Maintainer  : joachkr@ifi.uio.no
Stability   : experimental
Portability : POSIX

The current syntax of the invertible programming language suggested in the
article, `Jeopardy : an invertible programming language`, submitted to the
`37th symposium on implementation of functional languages` in Copenhagen 2022.

-}

module Core.Parser
  ( Source, Parser, ParseError, parseString
  , program_, pattern_, term_, inversion_
  )
where

import Core.Syntax
import Text.Parsec
import Control.Monad  (void)
import Data.Bifunctor (bimap, second)

-- Shorthands.
type Source      = String
type ParserState = (Integer, [X])
type Parser      = Parsec Source ParserState
type Info        = (SourcePos, SourcePos)

-- * Usage:

parseString :: Parser a -> Source -> Either ParseError a
parseString p = runParser p (0, []) "<no-such-file>"

-- * Implementation:

program_ :: Parser (Program Info)
program_ =
  do lexeme $ return ()
     ds <- many1 (choice $ map try [ datatypeDefinition, functionDefinition ])
     i  <- mainInversionDeclaration
     eof
     return $ foldr (\f p -> f p) i ds

type Definition = Program Info -> Program Info

functionDefinition :: Parser Definition
functionDefinition =
  do f   <- name
     _   <- symbol "("
     p   <- pattern_
     _   <- symbol ":"
     t_p <- name
     _   <- symbol ")"
     _   <- symbol ":"
     t_t <- name
     _   <- symbol "="
     t   <- term_
     _   <- symbol "."
     return $ Function f (p, t_p) (t, t_t)

datatypeDefinition :: Parser Definition
datatypeDefinition =
  do _  <- keyword "data"
     t  <- name
     _  <- symbol "="
     cs <- many $ brackets ((,) <$> name <*> many name)
     _  <- symbol "."
     return $ Data t cs

mainInversionDeclaration :: Parser (Program Info)
mainInversionDeclaration =
  do _ <- keyword "main"
     i <- inversion_
     _ <- symbol "."
     return $ Main i

pattern_ :: Parser (Pattern Info)
pattern_ = choice $ map info
  [ brackets (Constructor <$> name <*> many pattern_)
  , do x <- name
       case x of
         "_"     -> Existential <$> fresh
         '_' : _ -> Existential <$> pure x
         _       -> pure $ Variable x
  ]

term_ :: Parser (Term Info)
term_ = choice $
  map (info . try)
  [ Application <$> inversion_ <*> pattern_
  , do _     <- keyword "case"
       t     <- term_
       _     <- symbol ":"
       t_t   <- name
       _     <- keyword "of"
       Case (t, t_t) <$> many1
         (do _   <- symbol ";"
             p_i <- pattern_
             _   <- symbol "->"
             t_i <- term_
             return (p_i, t_i)
         )
  ] ++
  [ Pattern <$> pattern_
  , parens term_
  ]


inversion_ :: Parser (Inversion Info)
inversion_ = choice
  [ info $ Conventional <$> name
  , parens $ keyword "invert" >> inversion_
  ]

-- * Utility:

-- Supplies a fresh existential identifier.
fresh :: Parser Name
fresh =
  do (i, xs) <- getState
     let  x = "_x" ++ show i
     y <- if   x `elem` xs
          then fresh
          else return x
     modifyState $ bimap (+1) (y:)
     return y

-- Parses a name.
name :: Parser Name
name = try $
  do n <- lexeme $ many1 charAllowedInName
     if     isReserved n
       then fail $ "Unexpected keyword " ++ n
       else return ()
     modifyState $ second (n:)
     return n

-- Adds source position information to a parser that consumes it.
info :: Parser (Info -> a) -> Parser a
info p =
  do i <- getPosition
     m <- p
     j <- getPosition
     return (m (i, j))

-- Parses between "(" and ")".
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Parses between "[" and "]".
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- These are reserved keywords in the Jeopardy language.
reserved :: [Name]
reserved = ["main", "data", "invert", "case", "of"]

-- Parses p and anny trailing whitespace following it.
lexeme :: Parser a -> Parser a
lexeme p =
  do a <- p
     _ <- many (void space)
     return a

-- Holds if a name constitutes a reserved keyword.
isReserved :: Name -> Bool
isReserved = flip elem reserved

-- Treats a string as a parsable symbol.
symbol :: String -> Parser ()
symbol s = void $ lexeme $ try $ string s

-- Parses a known reserved keyword.
keyword :: String -> Parser ()
keyword k = try $ void $ lexeme $
  do _ <- string k
     notFollowedBy charAllowedInName

-- Parses the character '_'.
underscore :: Parser Char
underscore =
  char '_'

-- Parses the character '-'.
dash :: Parser Char
dash =
  char '-'

-- Parses any character that is allowed to occur in a name.
charAllowedInName :: Parser Char
charAllowedInName =
  try $ choice [ letter , digit , dash, underscore ]
