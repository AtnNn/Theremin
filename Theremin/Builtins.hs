module Theremin.Builtins where

import Theremin.Prelude
import Theremin.AST

con :: String -> Expr
con sym = Val $ Rec "Con" [Sym sym]

true_ : false_ : z_ : s_ :
  if_ :
  lte_ : not_ : add_ : mul_ :
  argument_ : returnValue_ : isSubsetOf_ :
  helpName0 : _ = [0..]

builtinFunNames = fromList [
  (lte_, "lte"),
  (not_, "not"),
  (add_, "add"),
  (mul_, "mul"),
  (s_, "s")
  ]

ref = Val . Var

builtins :: [Stmt]
name0 :: Name
(name0, builtins) = let
  var = state $ \n -> (n, n + 1)
  n =: expr = tell [Set (ref n) expr]
  lambda1 n l f = var >>= \x -> tell [Set (Val $ Var n) $ Lambda [x] l $ f (ref x)]
  lambda2 n l f = do
    x <- var; y <- var 
    tell [Set (ref n) $ Lambda [x, y] l $ f (ref x) (ref y)]
  lambda3 n l f = do
    x <- var; y <- var; z <- var
    tell [Set (ref n) $ Lambda [x, y, z] l $ f (ref x) (ref y) (ref z)]
  in runWriter $ flip execStateT helpName0 $ do
    true_ =: con "True"
    false_ =: con "False"
    z_ =: con "Z"
    s_ =: con "S"
    lambda3 if_ [] $ \a b c -> Case a [(true, b), (false, c)]
    xx <- var; yy <- var
    lambda2 lte_ [xx, yy] $ \x y -> 
      Case x [
        (z, true),
        (s (ref xx),
          Case y [
            (z, false),
            (s (ref yy), lte (ref xx) (ref yy))])]
    lambda1 not_ [] $ \bb ->
      Case bb [(true, false), (false, true)] 
    x <- var
    lambda2 add_ [x] $ \a b ->
      Case a [(z, b),
              (s (ref x), s (add (ref x) b))]
    x <- var
    lambda2 mul_ [x] $ \a b ->
      Case a [(z, z),
              (s (ref x), add b (mul (ref x) b))]      

true = App (ref true_) []
false = App (ref false_) []
z = App (ref z_) []
s x = App (ref s_) [x]
if' c i e = App (ref if_) [c, i ,e]
lte a b = App (ref lte_) [a, b]
not' b = App (ref not_) [b]
add a b = App (ref add_) [a, b]
mul a b = App (ref mul_) [a, b]
argument f i = App (ref argument_) [f, i]
returnValue f = App (ref returnValue_) [f]
isSubsetOf a b = App (ref isSubsetOf_) [a, b]

showBuiltinVal :: Val n -> Maybe String
showBuiltinVal (Rec "Z" []) = Just "0"
showBuiltinVal (Rec "S" [x]) = show . (+1) <$> valToInt x
showBuiltinVal (Sym sym) = Just $ ":" ++ sym
showBuiltinVal _ = Nothing

showsPrecBuiltinExpr :: Int -> Expr -> Maybe ShowS
showsPrecBuiltinExpr p expr =
  go expr
  where
    go (Val (Var n)) = showString <$> lookup n builtinFunNames
    go (App (Val (Var f)) args) 
      | f == true_, [] <- args = Just $ showString "true"
      | f == false_, [] <- args = Just $ showString "false"
      | f == z_, [] <- args= Just $ showString "0"
      | f == s_, [n] <- args= shows . (+ (1 :: Int)) <$> exprToInt n
      | f == if_, [c, t, e] <- args =
        Just $ showParen (p>0) $
        showString "if " .
        showsPrec 0 c .
        showString " then " .
        showsPrec 0 t .
        showString " else " .
        showsPrec 0 e
    go _ = Nothing

valToInt :: Val n -> Maybe Int
valToInt (Rec "Z" []) = Just 0
valToInt (Rec "S" [x]) = (1 +) <$> valToInt x
valToInt _ = Nothing

exprToInt (App z_ []) = Just 0
exprToInt (App s_ [n]) = (1+) <$> exprToInt n
exprToInt (Val v) = valToInt v
exprToInt _ = Nothing