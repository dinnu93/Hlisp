module Lib where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Data.IORef
-- Lisp Value Data Type

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> LispVal)
             | Func { params :: [String],
                      vararg :: (Maybe String),
                      body :: [LispVal],
                      closure :: Env }
             
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
  (PrimitiveFunc _) -> "<primitive>"
  (Func {params = args,vararg = varargs,body = body,closure = env}) -> "(lambda (" ++ unwords (map show args) ++
                                                                       (case varargs of
                                                                          Nothing -> ""
                                                                          Just arg -> " . " ++ arg) ++ ") ...)"
    
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

eval :: Env -> LispVal -> IO LispVal
eval env (Atom var) = getVar env var
eval env (List [Atom "quote", value]) = return value
eval env (List [Atom "if", pred, conseq, alt]) = do result <- eval env pred
                                                    if (unpackBool result)
                                                      then eval env conseq
                                                      else eval env alt
                                                      
eval env (List [Atom "car", list]) = do value <- eval env list
                                        case value of
                                          (List (x:xs)) -> return x
                                          (DottedList (x:xs) _) -> return x
                                          _ -> error "car not applicable"
                                   
eval env (List [Atom "cdr", list]) = do value <- eval env list 
                                        case value of
                                          (List (x:xs)) -> return $ List xs
                                          (DottedList [l] ls) -> eval env ls
                                          (DottedList (l:ls) lst) -> return $ DottedList ls lst
                                          _ -> error "cdr not applicable"
                                   
eval env (List (Atom "cond" : (List [pred, conseq]) : ls)) = do result <- eval env pred
                                                                if (unpackBool result)
                                                                  then (eval env conseq)
                                                                  else (eval env (List (Atom "cond" : ls)))
                                                                  
eval env (List [Atom "set!", Atom var, form]) = do value <- eval env form
                                                   setVar env var value

eval env (List [Atom "define", Atom var, form]) = do value <- eval env form
                                                     defineVar env var value
                                                     
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  do value <- makeNormalFunc env params body
     defineVar env var value

eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  do value <- makeVarArgs varargs env params body
     defineVar env var value

eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = makeVarArgs varargs env [] body
eval env (List [fn , v@(List [Atom "quote", _])]) = do func <- eval env fn
                                                       apply func [v] 
eval env (List (f : args)) = do func <- eval env f
                                argList <- mapM (eval env) args
                                apply func argList
                                
eval env val = return val

apply :: LispVal -> [LispVal] -> IO LispVal
apply (PrimitiveFunc func) args = return $ func args
apply (Func params varargs body closure) args = (if num params /= num args && varargs == Nothing
                                                 then error "Insufficient arguments!"
                                                 else (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody)
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
                                Just argName -> bindVars env [(argName, List $ remainingArgs)]
                                Nothing -> return env

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
  where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

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
              ("number?", unaryOp lispNumber),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=))]
  


numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp op [arg] = op arg
unaryOp _ _ = String "It's a unary operator dummy!"

boolBinop :: (LispVal -> a) -> (a -> a -> Bool) -> [LispVal] -> LispVal
boolBinop unpack op args 
  | l == 2 = Bool $ op f s
  | otherwise = error "It's a binary operator dummy!"  
  where l = length args 
        f = unpack (head args)
        s = unpack (last args)

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool
          
-- Type predicates check

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
unpackNum v = error $ (show v) ++ " is not a valid number!"

unpackStr :: LispVal -> String
unpackStr (String s) = s
unpackStr v = error $ (show v) ++ " is not a valid string!"

unpackBool :: LispVal -> Bool
unpackBool (Bool b) = b
unpackBool v = error $ (show v) ++ " is not a valid bool!"

-- Adding Variables and Assignment

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IO LispVal
getVar envRef var = do env <- readIORef envRef
                       nullLispValRef <- newIORef $ String $ "Error: No variable named: " ++ var
                       readIORef . maybe nullLispValRef id . lookup var $ env 

setVar :: Env -> String -> LispVal -> IO LispVal
setVar envRef var value = do env <- readIORef envRef
                             maybe (return ()) (flip writeIORef value) (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IO LispVal
defineVar envRef var value = do alreadyDefined <- isBound envRef var
                                if alreadyDefined
                                  then do setVar envRef var value
                                          return value
                                  else do valueRef <- newIORef value
                                          env <- readIORef envRef
                                          writeIORef envRef ((var, valueRef) : env)
                                          return value


bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)
