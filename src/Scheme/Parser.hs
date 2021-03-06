module Scheme.Parser where

import Control.Applicative ((<|>), many)
import Data.Char (isLetter, isAlphaNum)
import Text.Parsec (parse)
import Text.Parsec.Error
import Text.Parsec.String
import Text.Parsec.Char (digit, satisfy, char)
import Text.Parsec.Combinator (many1, sepBy, eof, between, optional)


import Scheme.Types

astParser :: Parser AST
astParser =
  parseInt <|> parseString <|> parseSymbol <|> parseList
  where
    parseInt = Atom . AInt <$> num
    parseString = Atom . AString <$> str
    parseSymbol = ASymbol <$> symbol

    parseList = do
      _ <- char '('
      optional spaces
      r <- astParser `sepBy` spaces
      optional spaces
      _ <- char ')'

      return $ List r
      
    spaces = many1 $ char ' '

    num :: Parser Int
    num = do
      n <- many1 digit
      return (read n)

    symbol :: Parser String
    symbol = do
      first <- satisfy isLetter
      rest <- many (satisfy validIdChar)
      return (first:rest)
        where
          validIdChar c = isAlphaNum c

    str :: Parser String
    str =
      between (char '"') (char '"') symbol

parseLang :: String -> Either ParseError AST
parseLang = parse (astParser <* eof) "<stdin>"

parseStandardForms :: AST -> Either CompileError FAST
parseStandardForms (Atom a) = Right (FAtom a)
parseStandardForms (ASymbol s)
  | s == "define" = Left ReservedWordSyntaxError
  | s == "begin" = Left ReservedWordSyntaxError
  | s == "lambda" = Left ReservedWordSyntaxError
  | otherwise = Right (FReference s)
parseStandardForms (List [ASymbol "define", ASymbol var, expr])
  = FDefine var <$> parseStandardForms expr
parseStandardForms (List (ASymbol "begin":rest))
  = FBegin <$> traverse parseStandardForms rest

parseStandardForms (List [ ASymbol "lambda"
                         , List parameters
                         , body ]) =
  FLambda
  <$> traverse isSymbol parameters
  <*> parseStandardForms body
  where
    isSymbol (ASymbol s) = Right s
    isSymbol _ = Left IncorrectParameterList

parseStandardForms (List (x:rest))
  = FApply <$> (parseStandardForms x) <*> traverse parseStandardForms rest

parseStandardForms _ = Left UnknownSyntax
