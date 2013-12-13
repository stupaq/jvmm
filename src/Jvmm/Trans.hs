module Jvmm.Trans where
import Jvmm.Trans.Internal
import qualified Jvmm.Trans.Output as O

import Control.Monad.Identity

import Syntax.AbsJvmm (Program)

import Jvmm.Errors (ErrorInfoT)

-- TRANSLATION LAYER ----------------------------------------------------------
--  Rewrites parser output into frontend-agnostic format that can be processed
--  by next layer(s). Resolves syntactic sugar and grammar irregularities.
-------------------------------------------------------------------------------

trans :: Program -> ErrorInfoT Identity O.CompilationUnit
trans = tProgram

