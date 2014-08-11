{-# OPTIONS_GHC
  -Wall
  -fno-warn-unused-binds
  -fno-warn-unused-imports
  -fno-warn-unused-matches
  -fno-warn-name-shadowing
  -fno-warn-missing-signatures #-}

{-# LANGUAGE ImplicitParams #-}

module Igtol where

import Debug.Trace

import Prelude hiding (fail, lookup)
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad (join, liftM2, forM_, zipWithM)
import Control.Monad.State (State, state, get, evalState, execStateT)
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.Map (Map, lookup, empty, union, unions, singleton, fromList)
import Data.Maybe (fromMaybe)
import Data.List (intercalate, intersperse)

data Expr =
  Ref Name |
  Let [Stmt] Expr |
  Case Expr [(Pat, Expr)] |
  Lambda [Pat] Expr |
  App Expr [Expr] |
  Val Val

data Stmt = Set Pat Expr | Assert Expr
          deriving Show

data Pat = Any | Bind Name | Match String [Pat]
         deriving Show

type Name = Int

data Val = Rec String [Val] | Sym String | Fun EvalEnv [Pat] Expr

type EvalEnv = Map Name Val

data CheckEnv = CheckEnv Name [Expr]

type Clause = (Pat, Expr)

type Result = Either String

type Loc = [String]

type Free = [Name]

intercalates :: String -> [ShowS] -> ShowS
intercalates sep = foldr (.) id . intersperse (showString sep)

instance Show Expr where
  showsPrec p expr = fromMaybe (go expr) $ showsPrecBuiltinExpr p expr
    where
      go (Ref n) = showString "x" . shows n
      go (Let stmts expr) =
        showParen(p > 0) $
        showString "let {" .
        intercalates ";" (map (showsPrec 0) stmts) .
        showString "} in " .
        showsPrec 0 expr
      go (Case expr clauses) =
        showParen (p > 0) $
        showString "case " .
        showsPrec 0 expr .
        showString " of {" .
        intercalates ";" (map (showsPrec 0) clauses) .
        showString "}"
      go (Lambda pats body) =
        showParen (p > 0) $
        showString "\\" .
        intercalates " " (map (showsPrec 1) pats) .
        showString " -> " .
        showsPrec 0 body
      go (App f x) =
        showParen (p > 0) $
        showsPrec 1 f . 
        showString "(" . 
        intercalates ", " (map (showsPrec 0) x) .
        showString ")"
      go (Val val) = showsPrec p val

instance Show Val where
  show val = fromMaybe (go val) $ showBuiltinVal val
    where
    go (Rec sym args) = sym ++ " " ++ intercalate " " (map show args)
    go (Sym s) = ":" ++ s
    go (Fun env pats body) =
      "using " ++
      show env ++
      "\\(" ++
      intercalate ", " (map show pats) ++
      ") -> " ++
      show body

instance Num Expr where
  a + b = add a b
  a * b = mul a b
  abs a = error "no abs for Expr"
  signum a = error "no signum for Expr"
  fromInteger x | x < 0 = error $ "fromInteger " ++ show x ++ " :: Expr"
                | otherwise = go x where go 0 = z; go x = s (go (x - 1))

fail :: Show a => String -> a -> Result x
fail msg a = Left $ msg ++ " " ++ show a

con :: String -> Expr
con sym = Val $ Rec "Con" [Sym sym]

true_ : false_ : z_ : s_ : if_ : lte_ : not_ :
  add_ : mul_ : helpName0 : _ = [0..]

builtinFunNames = fromList [
  (lte_, "lte"),
  (not_, "not"),
  (add_, "add"),
  (mul_, "mul")
  ]

builtins :: [Stmt]
name0 :: Name
(name0, builtins) = let
  var = state $ \n -> (n, n + 1)
  n =: expr = tell [Set (Bind n) expr]
  lambda1 n f = var >>= \x -> tell [Set (Bind n) $ Lambda [Bind x] $ f (Ref x)]
  lambda2 n f = do
    x <- var; y <- var 
    tell [Set (Bind n) $ Lambda [Bind x, Bind y] $ f (Ref x) (Ref y)]
  lambda3 n f = do
    x <- var; y <- var; z <- var
    tell [Set (Bind n) $ Lambda [Bind x, Bind y, Bind z] $ f (Ref x) (Ref y) (Ref z)]
  in runWriter $ flip execStateT helpName0 $ do
    true_ =: con "True"
    false_ =: con "False"
    z_ =: con "Z"
    s_ =: con "S"
    lambda3 if_ $ \a b c ->
      Case a [(Match "True" [], b), (Match "False" [], c)]
    xx <- var; yy <- var
    lambda2 lte_ $ \x y -> 
      Case x [
         (Match "Z" [], true),
         (Match "S" [Bind xx],
          Case y [
            (Match "Z" [], false),
            (Match "S" [Bind yy], lte (Ref xx) (Ref yy))])]
    lambda1 not_ $ \bb ->
      Case bb [(Match "True" [], false), (Match "False" [], true)] 
    x <- var
    lambda2 add_ $ \a b ->
      Case a [(Match "Z" [], b),
              (Match "S" [Bind x], s (add (Ref x) b))]
    lambda2 mul_ $ \a b ->
      Case a [(Match "Z" [], z),
              (Match "S" [Bind x], add b (mul (Ref x) b))]      
    
true = App (Ref true_) []
false = App (Ref false_) []
z = App (Ref z_) []
s x = App (Ref s_) [x]
if' c i e = App (Ref if_) [c, i ,e]
lte a b = App (Ref lte_) [a, b]
not' b = App (Ref not_) [b]
add a b = App (Ref add_) [a, b]
mul a b = App (Ref mul_) [a, b]

showsPrecBuiltinExpr :: Int -> Expr -> Maybe ShowS
showsPrecBuiltinExpr p expr =
  go expr
  where
    go (Ref n) = showString <$> lookup n builtinFunNames
    go (App (Ref f) args) 
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

showBuiltinVal :: Val -> Maybe String
showBuiltinVal (Rec "Z" []) = Just "0"
showBuiltinVal (Rec "S" [x]) = show . (+1) <$> valToInt x
showBuiltinVal (Sym sym) = Just $ ":" ++ sym
showBuiltinVal _ = Nothing

exprToInt (App z_ []) = Just 0
exprToInt (App s_ [n]) = (1+) <$> exprToInt n
exprToInt _ = Nothing

valToInt :: Val -> Maybe Int
valToInt (Rec "Z" []) = Just 0
valToInt (Rec "S" [x]) = (1 +) <$> valToInt x
valToInt _ = Nothing

tests :: [Expr]
tests = [
  not' true,
  lte 1 2,
  lte 2 2,
  lte 2 2,
  add 2 3,
  mul 2 3
  ]

evalTop = eval empty . Let builtins

testEval = forM_ tests $ \test -> do
  putStr "Expr: "
  print test
  putStr "Val: "
  putStrLn $ either show show $ evalTop test

eval :: EvalEnv -> Expr -> Result Val
eval env x =
  -- (\ret -> flip trace ret $ "Eval: " ++ show x ++ "\nRet: " ++ show ret) $
  go x
  where
    go (Ref n) = maybe (fail "No such name" (n, env)) return $ lookup n env
    go (Let stmts expr) = flip eval expr . union env =<< evalStmts env stmts
    go (Case expr pats) = evalCase env pats =<< eval env expr
    go (Lambda args body) = return $ Fun env args body
    go (App fun args) = join $ liftM2 (evalApp env) (eval env fun) (sequence $ map (eval env) args)
    go (Val val) = return $ val

evalApp :: EvalEnv -> Val -> [Val] -> Result Val
evalApp env (Fun closure params body) args =
  maybe (fail "invalid arguments" (params, args))
  (flip eval body . (`union` union closure env) . unions) . sequence $ zipWith (match env) params args
evalApp _ (Rec "Con" [Sym sym]) args = return $ Rec sym args
evalApp _ val _ = fail "Can only apply Con or Fun but got" val

evalCase :: EvalEnv -> [Clause] -> Val -> Result Val
evalCase env clauses val = foldr go (fail "no match for case" (clauses, val)) clauses
  where go (pat, expr) next = maybe next (\binds -> eval (union binds env) expr) $ match env pat val

match :: EvalEnv -> Pat -> Val -> Maybe EvalEnv
match _ Any val = Just empty
match env (Bind n) val = Just $ singleton n val
match env (Match want pats) (Rec got fields)
  | length pats == length fields && want == got =
    fmap unions $ sequence $ zipWith (match env) pats fields
match _ _ _ = Nothing

evalStmts :: EvalEnv -> [Stmt] -> Result EvalEnv
evalStmts env stmts = fmap (union env . unions) $ sequence $ map (evalStmt env) stmts

evalStmt :: EvalEnv -> Stmt -> Result EvalEnv
evalStmt env (Assert _) = return env
evalStmt env (Set pat expr) = maybe (fail "Set failed" (pat, expr)) (\bind -> return $ union bind env) . match env pat =<< eval env expr

mkName :: State CheckEnv Name
mkName = state $ \(CheckEnv n e) -> (n, CheckEnv (n + 1) e)

assert :: Expr -> State CheckEnv ()
assert expr = state $ \(CheckEnv n e) -> ((), CheckEnv n (expr : e))

load :: Loc -> Expr -> State CheckEnv (Name, Free)
load loc (Ref n) = return (n, [n])
load loc (Let stmts expr) = do
  sfree <- loadStmts ("in let statement" : loc) stmts
  (n, efree) <- load ("in let expression" : loc) expr
  return (n, sfree ++ efree)
load loc (Case expr clauses) = do
  (en, efree) <- load ("in case expression" : loc) expr
  (n, cfree) <- loadClauses ("in case clause" : loc) en clauses
  return (n, efree ++ cfree)

loadStmts loc stmts = fmap concat $ zipWithM (loadStmt loc) [1..] stmts

loadStmt :: Loc -> Int -> Stmt -> State CheckEnv Free
loadStmt loc i (Assert expr) = assert expr >> return []
loadStmt loc i (Set n expr) = do
  (en, free) <- load (show i : loc) expr
  assert $ unify n en
  return free

loadClauses = undefined

unify = undefined