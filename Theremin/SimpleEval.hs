module Theremin.SimpleEval where

import Theremin.Prelude
import Theremin.AST
import Theremin.Builtins
import qualified Control.Monad

type Env s = Map Name (STVal s)
data EvalEnv s = EvalEnv { local, global :: Env s }
type Eval s a = StateT (EvalEnv s) (ExceptT String (ST s)) a
data STVar s = STVar Name (STRef s (Val (STVar s)))
type STVal s = Val (STVar s)

instance Show (EvalEnv s) where
  show (EvalEnv loc glo) = "EvalEnv " ++ show loc ++ " " ++ show glo

instance Show (STVar s) where
  show (STVar n _) = "STVar " ++ show n

instance Eq (STVar s) where
  (STVar _ a) == (STVar _ b) = a == b

setLocal :: Name -> STVal s -> Eval s ()
setLocal n v = modify' $ \(EvalEnv loc glo) -> EvalEnv (insert n v loc) glo

setGlobal :: Name -> STVal s -> Eval s ()
setGlobal n v = modify' $ \(EvalEnv loc glo) -> EvalEnv loc (insert n v glo)

stVarRef :: STVar s -> STRef s (STVal s)
stVarRef (STVar _ ref) = ref

newSTVar :: Name -> STVal s -> Eval s (STVar s)
newSTVar n = lift . lift . fmap (STVar n) . newSTRef

readSTVar :: STVar s -> Eval s (STVal s)
readSTVar = lift . lift . readSTRef . stVarRef

writeSTVar :: STVar s -> STVal s -> Eval s ()
writeSTVar v = lift . lift . writeSTRef (stVarRef v)

fail :: Show a => String -> a -> Eval s x
fail msg a = throwError $ msg ++ " " ++ show a

runEval :: (forall s . Eval s a) -> Either String a
runEval m = runST $ runExceptT $ fmap fst $ runStateT m $ EvalEnv empty empty

toComplete :: Val (STVar s) -> Eval s CompleteVal
toComplete a =
  case a of
    Var ref -> toComplete =<< readSTVar ref
    Rec s l -> Rec s <$> mapM toComplete l
    Sym s -> return $ Sym s
    Fun a l c e -> do c' <- mapM toComplete c; return $ Fun a l c' e
    Any -> return $ Any

evalTop :: Expr -> Either String CompleteVal
evalTop e = runEval (toComplete =<< eval (Let builtins e))

scope :: Closure (STVal s) -> [Name] -> ([STVal s] -> Eval s a) -> Eval s a
scope clo ns m = do
  EvalEnv loc glo <- get
  put $ EvalEnv clo glo
  vars <- mapM newLocalVar ns
  ret <- m vars
  modify' $ \(EvalEnv _ glo) -> EvalEnv loc glo
  return ret

newLocalVar :: Name -> Eval s (STVal s)
newLocalVar n = do
  var <- newSTVar n Any
  setLocal n (Var var)
  return (Var var)

unifyVar :: STVar s -> STVal s -> Eval s (STVal s)
unifyVar var b = do
  checkOccurs var b
  a <- readSTVar var
  c <- unify a b
  writeSTVar var c
  return c

unify :: STVal s -> STVal s -> Eval s (STVal s)
unify a b = do a' <- unref a; b' <- unref b; go a' b' where
  go (Var var) a = unifyVar var a
  go a (Var var) = unifyVar var a
  go (Rec con1 args1) (Rec con2 args2)
    | con1 == con2 && length args1 == length args2 = do
      args <- zipWithM unify args1 args2
      return $ Rec con1 args
  go (Sym s1) (Sym s2) | s1 == s2 = return $ Sym s1
  go Any a = return a
  go a Any = return a
  go a b = fail "Could not unify" (a, b)

checkOccurs :: STVar s -> Val (STVar s) -> Eval s ()
checkOccurs var (Var m) | m == var = return ()
checkOccurs var val = go val where
  go (Var m)
    | m == var = fail "failed occurs check" (var, val)
    | otherwise = do
      go =<< readSTVar var
  go (Rec _ fields) = mapM_ (checkOccurs var) fields
  go _ = return ()

lookupEnv :: Name -> Eval s (STVal s)
lookupEnv n = do
  EvalEnv loc glo <- get
  ifJust (lookup n loc) unref $ 
    ifJust (lookup n glo) unref $ do
      var <- newSTVar n Any
      setGlobal n (Var var)
      return (Var var)

evalVal :: Val Name -> Eval s (STVal s)
evalVal Any = return $ Any
evalVal (Var n) = lookupEnv n
evalVal (Rec con fields) = Rec con <$> mapM evalVal fields
evalVal (Sym sym) = return $ Sym sym
evalVal (Fun args locals closure body) = do
  closure' <- mapM evalVal closure
  return $ Fun args locals closure' body

eval :: Expr -> Eval s (STVal s)
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
    go (Val (Var n)) = lookupEnv n
    go (Val val) = evalVal val

unref :: STVal s -> Eval s (STVal s)
unref (Var var) = do
  val <- readSTVar var
  case val of
    Any -> return $ Var var
    _ -> unref val
unref (Rec con fields) = Rec con <$> mapM unref fields
unref val = return $ val

evalLambda args locals body = do
  EvalEnv loc _ <- get
  return $ Fun args locals loc body

evalApp :: STVal s -> [STVal s] -> Eval s (STVal s)
evalApp f@(Fun params locals closure body) args
  | length args /= length params =
    fail "Invalid number of arguments in function application: " (f, args)
  | otherwise = scope closure (params ++ locals) $ \vars -> do
      zipWithM_ unify vars args
      eval body
evalApp (Rec "Con" [Sym sym]) args = return $ Rec sym args
evalApp val _ = fail "Can only apply Con or Fun but got" val

evalCaseElse :: Clause -> Expr -> STVal s -> Eval s (STVal s)
evalCaseElse (pat, expr) otherwise val = do
  patval <- eval pat
  maybe
    (eval otherwise)
    (\fsenv -> put fsenv >> eval expr)
    =<< match patval val

evalCase :: [Clause] -> STVal s -> Eval s (STVal s)
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

match :: STVal s -> STVal s -> Eval s (Maybe (EvalEnv s))
match a b = try $ unify a b >> return ()

try :: Eval s () -> Eval s (Maybe (EvalEnv s))
try m = do
  fsenv <- get
  ret <- lift $ lift $ runExceptT $ runStateT m fsenv
  case ret of
    Left _ -> return Nothing
    Right ((), fsenv') -> return $ Just fsenv'

evalStmts :: [Stmt] -> Eval s ()
evalStmts stmts = mapM_ evalStmt stmts

evalStmt :: Stmt -> Eval s ()
evalStmt (Assert expr) = do
  test <- eval expr
  case test of
    Rec "True" [] -> return ()
    otherwise -> fail "Failed assert" expr
evalStmt (Set pat expr) = do
  --trace ("Stmt: " ++ show pat ++ " = " ++ show expr) $ return ()
  val <- eval expr
  patval <- eval pat
  _ <- unify patval val
  return ()