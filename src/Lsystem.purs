module Lsystem where

import Prelude
import Data.List.Lazy (concatMap)
import Data.List.Lazy.Types (List)
import Control.Safely as Safely
import Data.List as L
import Control.Monad.Rec.Class (class MonadRec)

lsystem
  :: forall a
   . List a
  -> (a -> List a) 
  -> Int 
  -> List a
lsystem init prod n = go init n
  where
    go s 0 = s
    go s n' = go (concatMap prod s) (n' - 1)

interpret
  :: forall a s m
   . MonadRec m
  => List a
  -> (s -> a -> m s) 
  -> s 
  -> m s
interpret xs interpreter state = 
  Safely.foldM interpreter state (L.fromFoldable xs)

