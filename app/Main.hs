module Main where

import Lib
import System.Environment
import System.IO
import Control.Monad 

main :: IO ()
main = do env <- nullEnv
          until_ (=="quit") (prompt ">> ") $ evalAndPrint env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = (liftM show . eval env . readExpr $ expr) >>= putStrLn

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action
          
