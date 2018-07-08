{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scheme.Types where

import Control.Monad.State
import Data.Char (isLetter, isAlphaNum)
import Data.Map (Map)
import Text.Parsec (parse)
import Text.Parsec.Error
import Text.Parsec.String
import Text.Parsec.Char (digit, satisfy, char)
import Text.Parsec.Combinator (many1, sepBy, eof, between, optional)
import Control.Applicative ((<|>), many)

import Types
import Operations

data BasicValue = AInt Int
                | AString String
               deriving (Show)

instance ToPyExpr BasicValue where
  toPyExpr (AInt x) = PyInt x
  toPyExpr (AString s) = PyString s

data AbstractProgram a = Atom a
                       | ASymbol String
                       | List [AbstractProgram a]

deriving instance (Show a) => Show (AbstractProgram a)

type AST = AbstractProgram BasicValue

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

data CodeGenState = CodeGenState
                    { localVars :: Map String Int
                    , instructions :: [Operation]
                    , consts :: [PyExpr]
                    } deriving (Show)

newtype CodeGen a = CodeGen { unCodeGen :: State CodeGenState a }
  deriving (Functor, Applicative, Monad, MonadState CodeGenState)


addInstr :: Operation -> CodeGen ()
addInstr op = do
  instrs <- gets instructions
  modify (\s -> s { instructions = instrs ++ [op] })



