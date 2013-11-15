module Jvmm.Hierarchy.Output where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Foldable
import Data.Traversable

import Jvmm.Trans.Output

-- CLASS HIERARCHY --
---------------------
type ClassHierarchy = Hierarchy Class

-- Arbitrary hierarchy whic can be easily traversed with monadic mapper.
data Hierarchy a =
  Node a [Hierarchy a]
  deriving (Eq, Ord, Show)

instance Functor Hierarchy where
  fmap f (Node v children) = Node (f v) (map (fmap f) children)

instance Foldable Hierarchy where
  foldMap f (Node v children) = f v `mappend` mconcat (map (foldMap f) children)

instance Traversable Hierarchy where
  mapM f (Node v children) =
    liftM2 Node (f v) (Prelude.mapM (Data.Traversable.mapM f) children)

