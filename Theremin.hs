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
import Control.Applicative ((<$>))
import Control.Arrow (first)
import Control.Monad (
  join, liftM2, forM_, forM, zipWithM, zipWithM_, replicateM)
import Control.Monad.Error (ErrorT, runErrorT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (
  State, StateT, state, get, evalState, execStateT, evalStateT, runStateT)
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.List (intercalate, intersperse)
import Data.Map (Map, lookup, empty, union, unions, singleton, fromList, unionWith)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, catMaybes)
import Data.Set (Set, difference)

data Expr =
  Let [Stmt] Expr |
  Case Expr [Clause] |
  CaseElse Expr Clause Expr |
  Lambda Args Expr |
  App Expr [Expr] |
  Val Val

data Stmt = Set Expr Expr | Assert Expr
          deriving Show

type Name = Int

data Val =
  Rec String [Val] | 
  Sym String | 
  Fun Args Expr | -- TODO: closure
  Var Name |
  Any

type EvalEnv = Map Name Val

type Eval a = StateT EvalEnv (ErrorT String Identity) a

data CheckEnv = CheckEnv Name [Expr]

type Clause = (Expr, Expr)

type Loc = [String]

type Free = [Name]

type Args = [Name]

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
    go (Fun pats body) =
      "\\(" ++
      intercalate ", " (map show pats) ++
      ") -> " ++
      show body
    go (Var n) = "x" ++ show n
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
  (mul_, "mul")
  ]

ref = Val . Var

builtins :: [Stmt]
name0 :: Name
(name0, builtins) = let
  var = state $ \n -> (n, n + 1)
  n =: expr = tell [Set (ref n) expr]
  lambda1 n f = var >>= \x -> tell [Set (Val $ Var n) $ Lambda [x] $ f (ref x)]
  lambda2 n f = do
    x <- var; y <- var 
    tell [Set (ref n) $ Lambda [x, y] $ f (ref x) (ref y)]
  lambda3 n f = do
    x <- var; y <- var; z <- var
    tell [Set (ref n) $ Lambda [x, y, z] $ f (ref x) (ref y) (ref z)]
  in runWriter $ flip execStateT helpName0 $ do
    true_ =: con "True"
    false_ =: con "False"
    z_ =: con "Z"
    s_ =: con "S"
    lambda2 argument_ $ \f i -> Val $ Sym "TODO"
    lambda1 returnValue_ $ \f -> Val $ Sym "TODO"
    lambda2 isSubsetOf_ $ \a b -> Val $ Sym "TODO"
    lambda3 if_ $ \a b c -> Case a [(true, b), (false, c)]
    xx <- var; yy <- var
    lambda2 lte_ $ \x y -> 
      Case x [
        (z, true),
        (s (ref xx),
          Case y [
            (z, false),
            (s (ref yy), lte (ref xx) (ref yy))])]
    lambda1 not_ $ \bb ->
      Case bb [(true, false), (false, true)] 
    x <- var
    lambda2 add_ $ \a b ->
      Case a [(z, b),
              (s (ref x), s (add (ref x) b))]
    lambda2 mul_ $ \a b ->
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

runEval :: Eval a -> Either String (a, EvalEnv)
runEval m = runIdentity $ runErrorT $ runStateT m empty

evalTop = fmap fst . runEval . eval . Let builtins

test = forM_ tests $ \test -> do
  putStr "Expr: "
  print test
  putStr "Val: "
  putStrLn $ either show show $ evalTop test

locals :: [Name] -> Eval a -> Eval a
locals ns m = do
  state $ (,) () . (fromList (zip ns (repeat Any)) `union`)
  ret <- m
  state $ \env ->
    let locals = fromList $ flip map ns $ \n ->
          (n, subst locals (fromMaybe Any (lookup n env)))
    in ((), Map.map (subst locals) (foldr Map.delete env ns))
  return ret

subst :: Map Name Val -> Val -> Val
subst s Any = Any
subst s (Rec sym args) = Rec sym (map (subst s) args)
subst s (Sym sym) = Sym sym
subst s (Fun args body) = Fun args body
subst s (Var n) = fromMaybe (Var n) (lookup n s)

extend :: [(Name, Val)] -> Eval ()
extend ((n, val) : xs) = do
  env <- get
  let old = fromMaybe Any $ lookup n env
  new <- unify val old
  trace (show ("replacing", n, old, new)) $ return ()
  state $ \env -> ((), singleton n new `union` env)
  extend xs
extend [] = return ()

unify :: Val -> Val -> Eval Val
unify Any a = return a
unify a Any = return a
unify (Var n) a = checkOccurs n a >> extend [(n, a)] >> return a
unify a (Var n) = checkOccurs n a >> extend [(n, a)] >> return a
unify (Rec con1 args1) (Rec con2 args2)
  | con1 == con2 && length args1 == length args2 = do
    args <- zipWithM unify args1 args2
    return $ Rec con1 args
unify (Sym s1) (Sym s2) | s1 == s2 = return $ Sym s1
unify a b = fail "Could not unify" (a, b)

checkOccurs :: Name -> Val -> Eval ()
checkOccurs n (Var m) | m == n = fail "failed occurs check" n
checkOccurs n (Rec _ fields) = mapM_ (checkOccurs n) fields
checkOccurs _ _ = return ()

lookupEval :: Name -> Eval Val
lookupEval n = do
  env <- get
  return $ fromMaybe (Var n) (lookup n env)

eval :: Expr -> Eval Val
eval x =
  fmap (\ret -> flip trace ret $ "Eval: " ++ show x ++ "\nRet: " ++ show ret) $
  go x
  where
    go (Let stmts expr) = eval expr << evalStmts stmts
    go (Case expr clauses) = evalCase clauses =<< eval expr
    go (CaseElse expr clause otherwise) = 
      evalCaseElse clause otherwise =<< eval expr
    go (Lambda args body) = evalLambda args body
    go (App fun args) = do
      fun' <- eval fun
      args' <- sequence $ map eval args
      evalApp fun' args'
    go (Val (Var n)) = lookupEval n
    go (Val val) = return $ val

evalLambda args body = do
  -- TODO: closure
  return $ Fun args body

evalApp :: Val -> [Val] -> Eval Val
evalApp f@(Fun params body) args
  | length args /= length params =
    fail "Invalid number of arguments in function application: " (f, args)
  | otherwise = locals params $ do
      extend $ zip params args
      eval body
evalApp (Rec "Con" [Sym sym]) args = return $ Rec sym args
evalApp val _ = fail "Can only apply Con or Fun but got" val

evalCaseElse :: Clause -> Expr -> Val -> Eval Val
evalCaseElse (pat, expr) otherwise val = do
  patval <- eval pat
  maybe
    (eval otherwise)
    (\binds -> extend binds >> eval expr)
    =<< match patval val

evalCase :: [Clause] -> Val -> Eval Val
evalCase clauses val = do
  bindss <- mapM go clauses
  case catMaybes bindss of
    [] -> fail "No match" (clauses, val)
    [(expr, binds)] -> extend binds >> eval expr
    xs -> fail "Too many matches" (clauses, val, xs)
  where
    go (pat, expr) = do
      patval <- eval pat
      fmap ((,) expr) <$> match patval val

match :: Val -> Val -> Eval (Maybe [(Name, Val)])
match _ Any = fail "cannot match indefinite value Any" ()
match pat (Var n) = fail "cannot match indefinite value Var" n
match (Rec want pats) (Rec have fields)
  | length pats == length fields && want == have = do
    matches <- sequence $ zipWith match pats fields
    return $ fmap concat $ sequence matches
match (Sym a) (Sym b) | a == b = return $ Just []
-- TODO: Var n -> lookup n in environment and unify both
match (Var n) val = return $ Just $ [(n, val)]
match Any val = return $ Just []
match _ _ = return $ Nothing

evalStmts :: [Stmt] -> Eval ()
evalStmts stmts = mapM_ evalStmt stmts

evalStmt :: Stmt -> Eval ()
evalStmt (Assert _) = return ()
evalStmt (Set pat expr) = do
  val <- eval expr
  patval <- eval pat
  maybe
    (fail "Set failed" (pat, val))
    extend
    =<< match patval val

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
load loc (Lambda args body) = do
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
loadVal loc (Fun args body) = do
  undefined
loadVal loc (Sym sym) = do
  undefined
loadVal loc (Rec con fields) = do
  undefined