{-# OPTIONS_GHC
  -Wall
  -fno-warn-unused-binds
  -fno-warn-unused-imports
  -fno-warn-unused-matches
  -fno-warn-name-shadowing
  -fno-warn-missing-signatures #-}

{-# LANGUAGE ImplicitParams #-}

module Theremin where

import Debug.Trace

import Prelude hiding (fail, lookup)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Control.Monad (
  join, liftM2, forM_, forM, zipWithM, zipWithM_, replicateM)
import Control.Monad.Error (ErrorT, runErrorT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (
  State, StateT, state, get, evalState, execStateT, evalStateT, runStateT, put)
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.List (intercalate, intersperse, tails)
import Data.Map (Map, lookup, empty, union, unions, singleton, fromList, unionWith)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import Data.Set (Set, difference)

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
  Fun Args Locals Expr | -- TODO: closure
  Var n |
  Any

type EvalEnv = (FreeScope, Scope, Map ScopedName ScopedVal)

type FreeScope = Int

type Scope = [Int]

type Eval a = StateT EvalEnv (ErrorT String Identity) a

data CheckEnv = CheckEnv Name [Expr]

type Clause = (Expr, Expr)

type Loc = [String]

type Args = [Name]

type Locals = [Name]

type ScopedName = (Scope, Name)

type ScopedVal = Val ScopedName

(<<) = flip (>>)

intercalates :: String -> [ShowS] -> ShowS
intercalates sep = foldr (.) id . intersperse (showString sep)

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
    go (Fun pats locals body) =
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

fail :: Show a => String -> a -> Eval x
fail msg a = throwError $ msg ++ " " ++ show a

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
    lambda2 argument_ [] $ \f i -> Val $ Sym "TODO"
    lambda1 returnValue_ [] $ \f -> Val $ Sym "TODO"
    lambda2 isSubsetOf_ [] $ \a b -> Val $ Sym "TODO"
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

showBuiltinVal :: Val n -> Maybe String
showBuiltinVal (Rec "Z" []) = Just "0"
showBuiltinVal (Rec "S" [x]) = show . (+1) <$> valToInt x
showBuiltinVal (Sym sym) = Just $ ":" ++ sym
showBuiltinVal _ = Nothing

exprToInt (App z_ []) = Just 0
exprToInt (App s_ [n]) = (1+) <$> exprToInt n
exprToInt _ = Nothing

valToInt :: Val n -> Maybe Int
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

runEval :: Eval a -> Either String (a, EvalEnv)
runEval m = runIdentity $ runErrorT $ runStateT m (0, [], empty)

evalTop = fmap fst . runEval . eval . Let builtins

test = forM_ tests $ \test -> do
  putStr "Expr: "
  print test
  putStr "Val: "
  putStrLn $ either show show $ evalTop test

scope :: [Name] -> ([ScopedVal] -> Eval a) -> Eval a
scope ns m = do
  (old, new) <- state $ \(f, s, env) -> ((s, f:s), (f+1, f:s, env))
  --trace ("Enter: " ++ show new) $ return ()
  forM_ ns $ \n -> set (new, n) (Var (new, n))
  ret <- m $ map (\n -> Var (new, n)) ns
  --trace ("Exit: " ++ show new) $ return ()
  state $ \(f, s, env) -> ((), (f, old, env))
  return ret

set n a = do
  checkOccurs n a
  --trace ("Set: " ++ show n ++ " -> " ++ show a) $ return ()
  state $ \(f, s, env) -> (a, (f, s, singleton n a `union` env))

unify :: ScopedVal -> ScopedVal -> Eval ScopedVal
unify a b = do a' <- unref a; b' <- unref b; go a' b' where
  go (Var n) a = set n a
  go a (Var n) = set n a
  go (Rec con1 args1) (Rec con2 args2)
    | con1 == con2 && length args1 == length args2 = do
      args <- zipWithM unify args1 args2
      return $ Rec con1 args
  go (Sym s1) (Sym s2) | s1 == s2 = return $ Sym s1
  go Any a = return a
  go a Any = return a
  go a b = fail "Could not unify" (a, b)

checkOccurs :: ScopedName -> ScopedVal -> Eval ()
checkOccurs n (Var m) | n == m = return ()
checkOccurs n val = go val where
  go (Var m)
    | m == n = fail "failed occurs check" n
    | otherwise = do
      (_, _, env) <- get
      go $ fromMaybe Any $ lookup n env
  go (Rec _ fields) = mapM_ (checkOccurs n) fields
  go _ = return ()

lookupEval :: Name -> Eval ScopedVal
lookupEval n = do (_, scope, env) <- get; foldr (go env) def (tails scope)
  where
    def = return $ Var ([], n)
    go env s next =
      maybe next unref (lookup (s, n) env)

evalVal :: Val Name -> Eval ScopedVal
evalVal Any = return $ Any
evalVal (Var n) = lookupEval n
evalVal (Rec con fields) = Rec con <$> mapM evalVal fields
evalVal (Sym sym) = return $ Sym sym
evalVal (Fun args locals body) = return $ Fun args locals body

eval :: Expr -> Eval ScopedVal
eval x = do
  --trace ("Eval: " ++ show x) $ return ()
  unref =<< go x
  --trace ("Ret: " ++ show ret) $ return ret
  where
    go (Let stmts expr) = eval expr << evalStmts stmts
    go (Case expr clauses) = evalCase clauses =<< eval expr
    go (CaseElse expr clause otherwise) = 
      evalCaseElse clause otherwise =<< eval expr
    go (Lambda args locals body) = evalLambda args locals body
    go (App fun args) = do
      fun' <- eval fun
      args' <- sequence $ map eval args
      evalApp fun' args'
    go (Val (Var n)) = lookupEval n
    go (Val val) = evalVal val

unref :: ScopedVal -> Eval ScopedVal
unref (Var n) = do
  (_, _, env) <- get
  case lookup n env of
    Nothing -> return $ Var n
    Just Any -> return $ Var n
    Just (Var m) | n == m -> return $ Var n
    Just val -> unref val
unref (Rec con fields) = Rec con <$> mapM unref fields
unref val = return $ val

evalLambda args locals body = do
  -- TODO: closure
  return $ Fun args locals body

evalApp :: ScopedVal -> [ScopedVal] -> Eval ScopedVal
evalApp f@(Fun params locals body) args
  | length args /= length params =
    fail "Invalid number of arguments in function application: " (f, args)
  | otherwise = scope (params ++ locals) $ \vars -> do
      zipWithM_ unify vars args
      eval body
evalApp (Rec "Con" [Sym sym]) args = return $ Rec sym args
evalApp val _ = fail "Can only apply Con or Fun but got" val

evalCaseElse :: Clause -> Expr -> ScopedVal -> Eval ScopedVal
evalCaseElse (pat, expr) otherwise val = do
  patval <- eval pat
  maybe
    (eval otherwise)
    (\fsenv -> put fsenv >> eval expr)
    =<< match patval val

evalCase :: [Clause] -> ScopedVal -> Eval ScopedVal
evalCase clauses val = do
  bindss <- mapM go clauses
  case catMaybes bindss of
    [] -> fail "No match" (clauses, val)
    [(expr, fsenv)] -> put fsenv >> eval expr
    xs -> fail "Too many matches" (clauses, val, xs)
  where
    go (pat, expr) = do
      patval <- eval pat
      fmap ((,) expr) <$> match patval val

match :: ScopedVal -> ScopedVal -> Eval (Maybe EvalEnv)
match a b = try $ unify a b >> return ()

try :: Eval () -> Eval (Maybe EvalEnv)
try m = do
  fsenv <- get
  let ret = runIdentity $ runErrorT $ runStateT m fsenv
  case ret of
    Left _ -> return Nothing
    Right ((), fsenv') -> return $ Just fsenv'

evalStmts :: [Stmt] -> Eval ()
evalStmts stmts = mapM_ evalStmt stmts

evalStmt :: Stmt -> Eval ()
evalStmt (Assert _) = return ()
evalStmt (Set pat expr) = do
  --trace ("Stmt: " ++ show pat ++ " = " ++ show expr) $ return ()
  val <- eval expr
  patval <- eval pat
  _ <- unify patval val
  return ()

mkName :: State CheckEnv Name
mkName = state $ \(CheckEnv n e) -> (n, CheckEnv (n + 1) e)

assert :: Expr -> State CheckEnv ()
assert expr = state $ \(CheckEnv n e) -> ((), CheckEnv n (expr : e))

load :: Loc -> Expr -> State CheckEnv Name
load loc (Let stmts expr) = do
  loadStmts ("in let statement" : loc) stmts
  load ("in let expression" : loc) expr
load loc (Case expr clauses) = do
  n <- load ("in caseOne expression" : loc) expr
  loadClauses ("in caseOne clause" : loc) n clauses
load loc (CaseElse expr clause otherwise) = do
  undefined
load loc (Lambda args _locals body) = do
  n <- mkName
  ret <- load ("lambda body" : loc) body
  forM_ (zip [0..] args) $ \(i, arg) ->
    assert $ Case (argument (ref n) (fromInteger i)) [(ref arg, true)]
  assert $ Case (returnValue (ref n)) [(ref ret, true)]
  return n
load loc (App f args) = do
  ret <- mkName
  fn <- load ("function" : loc) f
  argns <- zipWithM (\i arg -> load (("argument" ++ show i) : loc) arg)
           [0 :: Int ..] args
  assert $ ref ret `isSubsetOf` returnValue (ref fn)
  forM_ (zip [0..] argns) $ \(i, arg) ->
    assert $ ref arg `isSubsetOf` argument (ref fn) (fromInteger i)
  return ret
load loc (Val val) = loadVal loc val

loadStmts loc stmts = zipWithM_ (loadStmt loc) [1..] stmts

loadStmt :: Loc -> Int -> Stmt -> State CheckEnv ()
loadStmt loc i (Assert expr) = assert expr
loadStmt loc i (Set pat expr) = do
  exprn <- load (show i : loc) expr
  patn <- load (("pattern" ++ show i) : loc) pat
  assert $ Case (ref exprn) [(ref patn, true)]
  

loadClauses :: Loc -> Name -> [Clause] -> State CheckEnv Name
loadClauses loc n clauses = do
  clausenps <-
    zipWithM (\i clause -> loadClause (show i : loc) clause) [1 :: Int ..] clauses
  ret <- mkName
  let pat = Case (ref n) $ map (\(patn, exprn) -> (ref patn, ref exprn)) clausenps
  assert $ Case (ref ret) [(pat, true)]
  return ret

loadClause :: Loc -> Clause -> State CheckEnv (Name, Name)
loadClause loc (pat, expr) = do
  patn <- load ("pattern" : loc) pat
  exprn <- load ("expression" : loc) pat
  return (patn, exprn)

loadVal loc Any = do
  mkName
loadVal loc (Var n) = do
  -- TODO: inside a case statement over this variable, make a
  -- new name and use isSubsetOf. Verify the validity of this scheme as well.
  return n
loadVal loc (Fun args locals body) = do
  undefined
loadVal loc (Sym sym) = do
  undefined
loadVal loc (Rec con fields) = do
  undefined

data The a = The {
  
  }

assuming :: x -> The a -> The a

first :: [The a] -> The a

unique :: [The a] -> The a

fail :: The a

