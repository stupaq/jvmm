module Semantics.Imperative (imperative) where

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Traversable as Traversable

import Semantics.Commons
import qualified Semantics.Errors as Err
import Semantics.Errors (rethrow, ErrorInfoT, runErrorInfoM)
import Semantics.APTree


imperative :: ClassHierarchy -> ErrorInfoT Identity ClassHierarchy
imperative classes = return classes

