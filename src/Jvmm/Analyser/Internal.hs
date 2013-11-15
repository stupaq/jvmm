module Jvmm.Analyser.Internal where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State

import qualified Jvmm.Errors as Err
import Jvmm.Errors (rethrow, ErrorInfoT)
import Jvmm.Trans.Output
import Jvmm.Hierarchy.Output

-- Implements statics semantics analysis, for now it includes:
-- - partial evaluation of constant expressions
-- - prunning of unreachable branches and no-op loops

-- ANALYSER MONAD --
--------------------

