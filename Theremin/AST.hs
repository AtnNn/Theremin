module Theremin.AST where

import Theremin.Prelude
import {-# SOURCE #-} Theremin.Builtins

data Expr =
  Let [Stmt] Expr |
  Case Expr [Clause] |
  CaseElse Expr Clause Expr |
  Lambda Args Locals Expr |
  App Expr [Expr] |
  Val (Val Name)

data Stmt = Set Expr Expr | Assert Expr
          deriving Show

type Name = Int

data Val n =
  Rec String [Val n] | 
  Sym String |
  Fun Args Locals (Closure (Val n)) Expr |
  Var n |
  Any

type CompleteVal = Val Void

type Closure n = Map Name n
type Clause = (Expr, Expr)
type Args = [Name]
type Locals = [Name]

instance Show Expr where
  showsPrec p expr = fromMaybe (go expr) $ showsPrecBuiltinExpr p expr
    where
      go (Let stmts expr) =
        showParen(p > 0) $
        showString "let {" .
        intercalates "; " (map (showsPrec 0) stmts) .
        showString "} in " .
        showsPrec 0 expr
      go (Case expr clauses) =
        showParen (p > 0) $
        showString "case " .
        showsPrec 0 expr .
        showString " of {" .
        intercalates "; " (map (showsPrec 0) clauses) .
        showString "}"
      go (CaseElse expr clause otherwise) =
        showParen (p > 0) $
        showString "case " .
        showsPrec 0 expr .
        showString " of " .
        showsPrec 0 clause .
        showString " else " .
        showsPrec 0 otherwise
      go (Lambda pats locals body) =
        showParen (p > 0) $
        showString "\\" .
        intercalates " " (map (showsPrec 1) pats) .
        showString " -> [" .
        intercalates ", " (map (showsPrec 0) locals) .
        showString "] " .
        showsPrec 0 body
      go (App f x) =
        showParen (p > 0) $
        showsPrec 1 f . 
        showString "(" . 
        intercalates ", " (map (showsPrec 0) x) .
        showString ")"
      go (Val val) = showsPrec p val

instance Show n => Show (Val n) where
  show val = fromMaybe (go val) $ showBuiltinVal val
    where
    go (Rec sym args) = sym ++ " " ++ intercalate " " (map show args)
    go (Sym s) = ":" ++ s
    go (Fun pats locals body _) =
      "\\(" ++
      intercalate ", " (map show pats) ++
      ") -> " ++
      show locals ++ " " ++
      show body
    go (Var n) = "var" ++ show n
    go Any = "_"

instance Num Expr where
  a + b = add a b
  a * b = mul a b
  abs a = error "no abs for Expr"
  signum a = error "no signum for Expr"
  fromInteger x | x < 0 = error $ "fromInteger " ++ show x ++ " :: Expr"
                | otherwise = go x where go 0 = z; go x = s (go (x - 1))
  negate a = error "Cannot negate Expr"