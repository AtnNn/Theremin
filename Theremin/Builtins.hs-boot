module Theremin.Builtins where
import Theremin.Prelude
import {-# SOURCE #-} Theremin.AST
showBuiltinVal :: Val n -> Maybe String
showsPrecBuiltinExpr :: Int -> Expr -> Maybe ShowS
add, mul :: Expr -> Expr -> Expr
z :: Expr
s :: Expr -> Expr