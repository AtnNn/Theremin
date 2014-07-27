{-# OPTIONS_GHC -Wall -fno-warn-unused-binds -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Constraints where

import Debug.Trace

import Prelude hiding (fail)
import Control.Monad (join, liftM2)
import Control.Monad.State (State, state, get, evalState)

data Expr =
  Ref Name |
  Let [Stmt] Expr |
  Case Expr [(Pat, Expr)] |
  Con String |
  Lambda [Pat] Expr |
  App Expr [Expr] |
  Error Expr
  deriving Show

data Stmt = Set Pat Expr | Assume Expr
          deriving Show

data Pat = Any | Bind Name | Match Expr [Pat]
         deriving Show

type Name = Int

data Val = Val String [Val] | Rec String | Fun Env [Pat] Expr
         deriving Show

type Env = [(Name, Val)]

type Clause = (Pat, Expr)

type Result = Either String

fail :: Show a => String -> a -> Result x
fail s a = Left $ s ++ " " ++ show a

builtins :: State Name ([Stmt], [Expr])
builtins = state $ \next -> flip evalState ([], next) $ do
  true <- val $ Con "True"
  false <- val $ Con "False"
  z <- val $ Con "Z"
  s <- val $ Con "S"
  a <- var; b <- var; c <- var
  return ()
  if' <- val $ Lambda [Bind a, Bind b, Bind c] $
         Case (Ref a) [(Match true [], Ref b), (Match false [], Ref c)]
  x <- var; y <- var; xx <- var; yy <- var
  lte <- fix $ \lte -> Lambda [Bind x, Bind y] $
         Case (Ref x) [
           (Match z [], true),
           (Match s [Bind xx],
            Case (Ref y) [
              (Match z [], false),
              (Match s [Bind yy], App lte [Ref xx, Ref yy])])]
  bb <- var
  not' <- val $ Lambda [Bind bb] $
         Case (Ref bb) [(Match true [], (App false [])), (Match false [], (App true []))]
  (stmts, n) <- get
  return ((stmts, [true, false, z, s, if', lte, not']), n)
  where
    val v = state $ \(e, n) -> (Ref n, (Set (Bind n) v : e, n + 1))
    fix f = state $ \(e, n) -> (Ref n, (Set (Bind n) (f (Ref n)) : e, n + 1))
    var = state $ \(e, n) -> (n, (e, n+1))

test :: [Result Val]
test = flip evalState 0 $ do
  (b, [true, false, z, s, if', lte, not']) <- builtins
  return $ map (eval [] . Let b) [
    App not' [App true []]
    ]

eval :: Env -> Expr -> Result Val
eval env (Ref n) = maybe (fail "No such name" (n, env)) return $ lookup n env
eval env (Let stmts expr) = flip eval expr . (env ++) =<< evalStmts env stmts
eval env (Case expr pats) = evalCase env pats =<< eval env expr
eval env (Con sym) = return $ Rec sym
eval env (Lambda args body) = return $ Fun env args body
eval env (App fun args) = join $ liftM2 (evalApp env) (eval env fun) (sequence $ map (eval env) args)
eval env (Error expr) = fail "Error:" =<< eval env expr

evalApp :: Env -> Val -> [Val] -> Result Val
evalApp env (Fun closure params body) args =
  maybe (fail "invalid arguments" (params, args)) (flip eval body . (\binds -> binds ++ closure ++ env) . concat) . sequence =<< (sequence $ zipWith (match env) params args)
evalApp _ (Rec sym) args = return $ Val sym args
evalApp _ val _ = fail "Can only apply Con or Fun but got" val

evalCase :: Env -> [Clause] -> Val -> Result Val
evalCase env clauses val = foldr go (fail "no match for case" (clauses, val)) clauses
  where go (pat, expr) next = maybe next (\binds -> eval (binds ++ env) expr) =<< match env pat val

match :: Env -> Pat -> Val -> Result (Maybe Env)
match _ Any val = return $ Just []
match env (Bind n) val = return $ Just [(n, val)]
match env (Match expr pats) (Val got fields)
  | length pats == length fields = do
    val <- eval env expr
    case val of
      Rec want | want == got ->
        fmap (fmap concat . sequence) $ sequence $ zipWith (match env) pats fields
      _ -> return Nothing
match _ _ _ = return Nothing

evalStmts :: Env -> [Stmt] -> Result Env
evalStmts env stmts = fmap ((env ++) . concat) $ sequence $ map (evalStmt env) stmts

evalStmt :: Env -> Stmt -> Result Env
evalStmt env (Assume _) = return env
evalStmt env (Set pat expr) = maybe (fail "Set failed" (pat, expr)) (\bind -> return $ bind ++ env) =<< match env pat =<< eval env expr
