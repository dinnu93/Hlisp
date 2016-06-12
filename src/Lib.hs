module Lib where
import qualified Data.Char as C

-- Simple Interpreter for calculator like expressions :)

data LExpr = Lit Integer 
           | Add LExpr LExpr 
           | Mul LExpr LExpr 
           | Sub LExpr LExpr 
           | Div LExpr LExpr 
           deriving (Show,Eq)




