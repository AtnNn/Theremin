
module Theremin where

import Theremin.Prelude
import Theremin.AST
import Theremin.Builtins
import Theremin.SimpleEval

data CheckEnv = CheckEnv Name [Expr]

type Loc = [String]

main = test

tests :: [Expr]
tests = [
  not' true,
  lte 1 2,
  lte 2 2,
  lte 2 1,
  add 2 3,
  mul 2 3
  ]

test = forM_ tests $ \test -> do
  putStr "Expr: "
  print test
  putStr "Val: "
  putStrLn $ either show show $ evalTop test
