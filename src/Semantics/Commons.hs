module Semantics.Commons where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Foldable
import Data.Traversable

import qualified Text.Show.Pretty as Pretty

-- GENERAL --
-------------
applyAndCompose :: (b -> a -> a) -> [b] -> a -> a
applyAndCompose f = Prelude.foldl (flip (.)) Prelude.id . map f

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

errorDebug :: (Show a) => a -> b
errorDebug = error . Pretty.ppShow

