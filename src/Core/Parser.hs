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

module Core.Parser where

import Core.Syntax
import Text.Parsec
import Control.Monad (void)

-- Shorthands.
type Source      = String
type ParserState = ()
type Parser      = Parsec Source ParserState

-- For now, the meta data is the cursor start and end position from where
-- the entity was parsed (used for error messages).
type    Info                = (SourcePos, SourcePos)
newtype SourceFileReference = SourceFileReference (Maybe Info)
  deriving (Eq, Show)

-- * Implementation

-- parseFromFile :: FilePath -> IO (Either ParseError (Program SourceFileReference))
-- parseFromFile fileName =
--   do input <- readFile fileName
--      return $ runParser program () fileName input

-- program :: Parser (Program SourceFileReference)
-- program =
--   do lexeme $ return ()
--      ds <- many definition
--      eof
--      return $ SourceFileReference . Just <$> Program ds

type Definition = Program SourceFileReference -> Program SourceFileReference

-- Parses a name.
name :: Parser Name
name =
  do n <- try $ lexeme $ many1 charAllowedInName
     if     not (n `elem` reserved)
       then return n
       else fail $ "Unexpected keyword " ++ n


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
     return $ Function f (p, t_p) (t, t_t)

datatypeDefinition :: Parser Definition
datatypeDefinition =
  do _  <- keyword "data"
     t  <- name
     _  <- symbol "="
     cs <- many $ brackets ((,) <$> name <*> many name)
     return $ Data t cs

mainFunctionDeclaration :: Parser (Program SourceFileReference)
mainFunctionDeclaration =
  do _ <- keyword "main"
     Main <$> inversion_

pattern_ :: Parser (Pattern SourceFileReference)
pattern_ = undefined

term_ :: Parser (Term SourceFileReference)
term_ = undefined

inversion_ :: Parser (Inversion SourceFileReference)
inversion_ = undefined

-- * Utility

-- Parses between "(" and ")".
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Parses between "[" and "]".
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- These are reserved keywords in the Jeopardy language.
reserved :: [Name]
reserved = ["main", "data", "case", "of"]

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
keyword k =
  void $ lexeme $
  do _ <- try $ string k
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
