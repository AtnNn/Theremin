module Theremin.Parser

import Theremin.Prelude
import Theremin.Language.Parser
import Theremin.AST
import Theremin.Builtins

file :: Parser Char [(Symbol, )]
file = block $ do 
  stmts <- many statement
  return $ Let (concat stmts) true

block :: Parser Char a -> Parser Char a
block = undefined

statement :: Parser Char [Satement
statement = choice [data_, declaration]

data_ :: Parser Char Statement
data_ = do
  token "data"
  block $ do
    _ <- name
    c <- sepBy (token "|")

name = do
  first <- match (`elem` alpha)
  rest <- many $ match (`elem` alphaNum)

alpha = "_" ++ ['a'..'z'] ++ ['A'..'Z']

alphaNum = ['0'..'9'] ++ alpha