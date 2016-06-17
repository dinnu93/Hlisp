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
parseNumber = liftM (Number . read) $ many1 digit 

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces 

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x] 


-- Final Lisp Expression Parser

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> do char '('
         x <- try parseList <|> parseDottedList
         char ')'
         return x

-- Display part

showVal :: LispVal -> String
showVal lispVal = case lispVal of
  (String contents) -> "\"" ++ contents ++ "\""
  (Atom name) -> name
  (Number contents) -> show contents
  (Bool True) -> "#t"
  (Bool False) -> "#f"
  (List contents) -> "(" ++ unwordsList contents ++ ")"
  (DottedList head tail) -> "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where
  show = showVal
  

-- read the expression and give a string 

readExpr :: String -> LispVal 
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val
  
-- Evaluator

eval :: LispVal -> LispVal
eval val = case val of
  (List [Atom "quote", value]) -> value
  (List [Atom fn , v@(List [Atom "quote", _])]) -> apply fn [v] 
  (List (Atom f : args)) -> apply f $ map eval args
  _ -> val
  
apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Bool False) ($ args) $ lookup f primitives


primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", unaryOp lispSymbol),
              ("string?", unaryOp lispString),
              ("number?", unaryOp lispNumber)]
  


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp op [arg] = op arg
unaryOp _ _ = String "It's a unary operator dummy!"

lispSymbol :: LispVal -> LispVal
lispSymbol (List [Atom "quote", xs]) = Bool True
lispSymbol _ = Bool False

lispString :: LispVal -> LispVal
lispString (String _) = Bool True
lispString _ = Bool False

lispNumber :: LispVal -> LispVal
lispNumber (Number _) = Bool True
lispNumber _ = Bool False

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0
