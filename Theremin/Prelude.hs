module Theremin.Prelude (
  module Theremin.Prelude,
  module Prelude,
  module Control.Applicative,
  module Control.Arrow,
  module Control.Monad,
  module Control.Monad.Except,
  module Control.Monad.Identity,
  module Control.Monad.Trans,
  module Control.Monad.Reader,
  module Control.Monad.State,
  module Control.Monad.Writer,
  module Control.Monad.ST,
  module Control.Monad.Fix,
  module Data.STRef,
  module Data.List,
  module Data.Map,
  module Data.Maybe,
  module Data.Set,
  module Data.Either,
  module Data.Traversable,
  module Data.Foldable,
  module Data.Void,
  module System.IO,
  ) where

import Prelude (
  String, Int, ShowS, Either(..), Bool(..), Char,
  Num(..), Ord(..), Eq(..), Show(..), Functor(..),
  (.), id, ($), (++), map, flip, fst, snd, (&&),
  showString, showParen, shows,
  error, otherwise, undefined)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Control.Monad (
  Monad((>>), (>>=), return), (=<<),
  join, liftM2, zipWithM, zipWithM_, replicateM)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Trans (lift)
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Monad.State (
  MonadState(get, put, state), State, StateT,
  modify', evalState, execStateT, evalStateT, runStateT)
import Control.Monad.Writer (Writer, tell, runWriter)
import Control.Monad.ST (ST, runST)
import Control.Monad.Fix (fix)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.List (length, intercalate, intersperse, tails, zip)
import Data.Map (
  Map, lookup, empty, union, unions, singleton, fromList, unionWith, insert)
import Data.Maybe (Maybe(..), maybe, fromMaybe, catMaybes, listToMaybe)
import Data.Set (Set, difference)
import Data.Either (either)
import Data.Traversable (forM, Traversable(sequence, traverse, mapM))
import Data.Foldable (foldr, foldl', foldrM, mapM_, forM_, toList)
import Data.Void (Void)
import System.IO (putStr, putStrLn, print)

intercalates :: String -> [ShowS] -> ShowS
intercalates sep = foldr (.) id . intersperse (showString sep)

(<<) :: Monad m => m a -> m b -> m a
(<<) = flip (>>)

ifJust :: Maybe a -> (a -> b) -> b -> b
ifJust Nothing _ b = b
ifJust (Just a) f _ = f a

ifNothing :: Maybe a -> b -> (a -> b) -> b
ifNothing Nothing b _ = b
ifNothing (Just a) _ f = f a

