module Theremin.Language.Parser where

import Theremin.Prelude

data Parser c a = Parser

many :: Parser c a -> Parser c [a]
many = undefined

choice :: [Parser c a] -> Parser c a
choice = undefined

match :: (c -> Bool) -> Parser c c
match = undefined