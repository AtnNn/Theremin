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
import Control.Monad (join, liftM2, forM_, forM, zipWithM, zipWithM_)
import Control.Monad.State (State, state, get, evalState, execStateT)
import Control.Monad.Writer (Writer, tell, runWriter)
import Data.List (intercalate, intersperse)
import Data.Map (Map, lookup, empty, union, unions, singleton, fromList)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Set (Set, difference)

data Expr =
  Ref Name |
  Let [Stmt] Expr |
  CaseOne Expr [Clause] |
  CaseSeq Expr [Clause] |
  Lambda [Name] Expr |
  App Expr [Expr] |
  Val Val

data Stmt = Set Expr Expr | Assert Expr
          deriving Show

type Name = Int

data Val =
  Rec String [Val] | 
  Sym String | 
  Fun EvalEnv [Name] Expr | 
  Bind Name |
  Any

type EvalEnv = Map Name Val

data CheckEnv = CheckEnv Name [Expr]

type Clause = (Expr, Expr)

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
      go (CaseOne expr clauses) =
        showParen (p > 0) $
        showString "caseOne " .
        showsPrec 0 expr .
        showString " of {" .
        intercalates "; " (map (showsPrec 0) clauses) .
        showString "}"
      go (CaseSeq expr clauses) =
        showParen (p > 0) $
        showString "caseSeq " .
        showsPrec 0 expr .
        showString " of {" .
        intercalates "; " (map (showsPrec 0) clauses) .
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
    go (Bind n) = "@x" ++ show n
    go Any = "_"

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

builtins :: [Stmt]
name0 :: Name
(name0, builtins) = let
  var = state $ \n -> (n, n + 1)
  n =: expr = tell [Set (Val $ Bind n) expr]
  lambda1 n f = var >>= \x -> tell [Set (Val $ Bind n) $ Lambda [x] $ f (Ref x)]
  lambda2 n f = do
    x <- var; y <- var 
    tell [Set (Val $ Bind n) $ Lambda [x, y] $ f (Ref x) (Ref y)]
  lambda3 n f = do
    x <- var; y <- var; z <- var
    tell [Set (Val $ Bind n) $ Lambda [x, y, z] $ f (Ref x) (Ref y) (Ref z)]
  in runWriter $ flip execStateT helpName0 $ do
    true_ =: con "True"
    false_ =: con "False"
    z_ =: con "Z"
    s_ =: con "S"
    lambda2 argument_ $ \f i -> Val $ Sym "TODO"
    lambda1 returnValue_ $ \f -> Val $ Sym "TODO"
    lambda2 isSubsetOf_ $ \a b -> Val $ Sym "TODO"
    lambda3 if_ $ \a b c -> CaseOne a [(true, b), (false, c)]
    xx <- var; yy <- var
    lambda2 lte_ $ \x y -> 
      CaseOne x [
        (z, true),
        (s (Val $ Bind xx),
          CaseOne y [
            (z, false),
            (s (Val $ Bind yy), lte (Ref xx) (Ref yy))])]
    lambda1 not_ $ \bb ->
      CaseOne bb [(true, false), (false, true)] 
    x <- var
    lambda2 add_ $ \a b ->
      CaseOne a [(z, b),
              (s (Val $ Bind x), s (add (Ref x) b))]
    lambda2 mul_ $ \a b ->
      CaseOne a [(z, z),
              (s (Val $ Bind x), add b (mul (Ref x) b))]      

true = App (Ref true_) []
false = App (Ref false_) []
z = App (Ref z_) []
s x = App (Ref s_) [x]
if' c i e = App (Ref if_) [c, i ,e]
lte a b = App (Ref lte_) [a, b]
not' b = App (Ref not_) [b]
add a b = App (Ref add_) [a, b]
mul a b = App (Ref mul_) [a, b]
argument f i = App (Ref argument_) [f, i]
returnValue f = App (Ref returnValue_) [f]
isSubsetOf a b = App (Ref isSubsetOf_) [a, b]

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

test = forM_ tests $ \test -> do
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
    go (CaseOne expr pats) = evalCaseOne env pats =<< eval env expr
    go (CaseSeq expr pats) = evalCaseSeq env pats =<< eval env expr
    go (Lambda args body) = return $ Fun env args body
    go (App fun args) = join $ liftM2 (evalApp env) (eval env fun) (sequence $ map (eval env) args)
    go (Val val) = return $ val

evalApp :: EvalEnv -> Val -> [Val] -> Result Val
evalApp env f@(Fun closure params body) args
  | length args /= length params =
    fail "Invalid number of arguments in function application: " (f, args)
  | otherwise = do
      let binds = fromList $ zip params args
      eval (binds `union` union closure env) body
evalApp _ (Rec "Con" [Sym sym]) args = return $ Rec sym args
evalApp _ val _ = fail "Can only apply Con or Fun but got" val

evalCaseSeq :: EvalEnv -> [Clause] -> Val -> Result Val
evalCaseSeq env clauses val =
  foldr go (fail "no match for case" (clauses, val)) clauses
  where
    go (pat, expr) next = do
      patval <- eval env pat
      maybe next (\binds -> eval (union binds env) expr) =<< match patval val

evalCaseOne :: EvalEnv -> [Clause] -> Val -> Result Val
evalCaseOne env clauses val = do
  bindss <- mapM go clauses
  case catMaybes bindss of
    [] -> fail "No match" (clauses, val)
    [(expr, binds)] -> eval (union binds env) expr
    xs -> fail "Too many matches" (clauses, val, xs)
  where
    go (pat, expr) = do
      patval <- eval env pat
      fmap ((,) expr) <$> match patval val

match :: Val -> Val -> Result (Maybe EvalEnv)
match _ (Bind n) = Left $ "cannot match indefinite value Bind " ++ show n
match _ Any = Left $ "cannot match indefinite value Any"
match (Rec want pats) (Rec have fields)
  | length pats == length fields && want == have = do
    matches <- sequence $ zipWith match pats fields
    return $ fmap unions $ sequence matches
match (Sym a) (Sym b) | a == b = return $ Just empty
match (Bind n) val = return $ Just $ singleton n val
match Any val = return $ Just empty
match _ _ = return $ Nothing

evalStmts :: EvalEnv -> [Stmt] -> Result EvalEnv
evalStmts env stmts = fmap (union env . unions) $ sequence $ map (evalStmt env) stmts

evalStmt :: EvalEnv -> Stmt -> Result EvalEnv
evalStmt env (Assert _) = return env
evalStmt env (Set pat expr) = do
  val <- eval env expr
  patval <- eval env pat
  maybe
    (fail "Set failed" (pat, val))
    (\bind -> return $ union bind env)
    =<< match patval val

mkName :: State CheckEnv Name
mkName = state $ \(CheckEnv n e) -> (n, CheckEnv (n + 1) e)

assert :: Expr -> State CheckEnv ()
assert expr = state $ \(CheckEnv n e) -> ((), CheckEnv n (expr : e))

load :: Loc -> Expr -> State CheckEnv Name
load loc (Ref n) = do
  n' <- mkName
  assert $ Ref n' `isSubsetOf` Ref n
  return n'
load loc (Let stmts expr) = do
  loadStmts ("in let statement" : loc) stmts
  load ("in let expression" : loc) expr
load loc (CaseOne expr clauses) = do
  n <- load ("in caseOne expression" : loc) expr
  loadClauses ("in caseOne clause" : loc) False n clauses
load loc (CaseSeq expr clauses) = do
  n <- load ("in caseSeq expression" : loc) expr
  loadClauses ("in caseSeq clause" : loc) True n clauses
load loc (Lambda args body) = do
  n <- mkName
  ret <- load ("lambda body" : loc) body
  forM_ (zip [0..] args) $ \(i, arg) ->
    assert $ CaseOne (argument (Ref n) (fromInteger i)) [(Ref arg, true)]
  assert $ CaseOne (returnValue (Ref n)) [(Ref ret, true)]
  return n
load loc (App f args) = do
  ret <- mkName
  fn <- load ("function" : loc) f
  argns <- zipWithM (\i arg -> load (("argument" ++ show i) : loc) arg)
           [0 :: Int ..] args
  assert $ Ref ret `isSubsetOf` returnValue (Ref fn)
  forM_ (zip [0..] argns) $ \(i, arg) ->
    assert $ Ref arg `isSubsetOf` argument (Ref fn) (fromInteger i)
  return ret
load loc (Val val) = loadVal loc val

loadStmts loc stmts = zipWithM_ (loadStmt loc) [1..] stmts

loadStmt :: Loc -> Int -> Stmt -> State CheckEnv ()
loadStmt loc i (Assert expr) = assert expr
loadStmt loc i (Set pat expr) = do
  exprn <- load (show i : loc) expr
  patn <- load (("pattern" ++ show i) : loc) pat
  assert $ CaseOne (Ref exprn) [(Ref patn, true)]
  

loadClauses :: Loc -> Bool -> Name -> [Clause] -> State CheckEnv Name
loadClauses loc seq n clauses = do
  let case' = if seq then CaseSeq else CaseOne
  clausenps <-
    zipWithM (\i clause -> loadClause (show i : loc) clause) [1 :: Int ..] clauses
  ret <- mkName
  let pat = case' (Ref n) $ map (\(patn, exprn) -> (Ref patn, Ref exprn)) clausenps
  assert $ CaseOne (Ref ret) [(pat, true)]
  return ret

loadClause :: Loc -> Clause -> State CheckEnv (Name, Name)
loadClause loc (pat, expr) = do
  patn <- load ("pattern" : loc) pat
  exprn <- load ("expression" : loc) pat
  return (patn, exprn)

loadVal loc Any = do
  mkName
loadVal loc (Bind n) = do
  undefined
loadVal loc (Fun closure args body) = do
  undefined
loadVal loc (Sym sym) = do
  undefined
loadVal loc (Rec con fields) = do
  undefined