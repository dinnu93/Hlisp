module Main where

import Lib
import System.Environment
import System.IO
import Control.Monad 

main :: IO ()
main = forever $ do
  code <- prompt ">> "
  putStrLn $ code

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

 
