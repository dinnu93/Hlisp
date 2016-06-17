module Lib where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

-- Lisp Abstract Syntax Tree

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             deriving (Show)

-- Parser Combinators

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space 

-- Parser Combinators returning Lisp Values

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many ((noneOf "\\\"") <|> (char '\\' >> oneOf "\""))
  char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
  n <- many1 digit
  return $ Number . read $ n

-- Final Lisp Expression Parser

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber

-- read the expression and give a string 

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> error . show $ err
  Right val -> val
  
